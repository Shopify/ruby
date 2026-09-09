//! Guards, patch points, and side exits.

use crate::backend::lir::{self, asm_ccall, asm_comment, Assembler, Opnd, SideExit, SideExitRecompile, SideExitTarget, StackMap, Target};
use crate::cruby::*;
use crate::hir::{self, FrameState, Function, Invariant, Recompile};
use crate::hir::SideExitReason::{self, *};
use crate::hir_type::{types, Type};
use crate::invariants::{
    track_bop_assumption, track_cme_assumption, track_no_ep_escape_assumption,
    track_no_newobj_hook_assumption, track_no_singleton_class_assumption,
    track_no_trace_point_assumption, track_root_box_assumption, track_single_ractor_assumption,
    track_stable_constant_names_assumption,
};
use crate::payload::IseqVersionRef;
use crate::stats::Counter;
use super::{build_stack_map, gen_incr_counter, jit_frame_for_state, JITState};

pub(super) fn gen_guard_less(jit: &mut JITState, asm: &mut Assembler, function: &Function, left: Opnd, right: Opnd, reason: SideExitReason, state: &FrameState) -> Opnd {
    asm.cmp(left, right);
    asm.jge(jit, side_exit(jit, function, state, reason));
    left
}

pub(super) fn gen_guard_greater_eq(jit: &mut JITState, asm: &mut Assembler, function: &Function, left: Opnd, right: Opnd, state: &FrameState) -> Opnd {
    asm.cmp(left, right);
    asm.jl(jit, side_exit(jit, function, state, SideExitReason::GuardGreaterEq));
    left
}

pub(super) fn gen_patch_point(jit: &mut JITState, asm: &mut Assembler, function: &Function, invariant: &Invariant, state: &FrameState) {
    let invariant = *invariant;
    let exit = build_side_exit(jit, function, state);

    // Let compile_exits compile a side exit. Let scratch_split lower it with split_patch_point.
    asm.patch_point(Target::SideExit(Box::new(SideExitTarget { exit, reason: PatchPoint(invariant) })), invariant, jit.version);
}

pub fn split_patch_point(asm: &mut Assembler, target: &Target, invariant: Invariant, version: IseqVersionRef) {
    let Target::Label(exit_label) = *target else {
        unreachable!("PatchPoint's target should have been lowered to Target::Label by compile_exits: {target:?}");
    };

    // Fill nop instructions if the last patch point is too close.
    asm.patch_point_pad();

    // Remember the current address as a patch point
    asm.pos_marker(move |code_ptr, cb| {
        let side_exit_ptr = cb.resolve_label(exit_label);
        match invariant {
            Invariant::BOPRedefined { klass, bop } => {
                track_bop_assumption(klass, bop, code_ptr, side_exit_ptr, version);
            }
            Invariant::MethodRedefined { klass: _, method: _, cme } => {
                track_cme_assumption(cme, code_ptr, side_exit_ptr, version);
            }
            Invariant::StableConstantNames { idlist } => {
                track_stable_constant_names_assumption(idlist, code_ptr, side_exit_ptr, version);
            }
            Invariant::NoTracePoint => {
                track_no_trace_point_assumption(code_ptr, side_exit_ptr, version);
            }
            Invariant::NoNewObjHook => {
                track_no_newobj_hook_assumption(code_ptr, side_exit_ptr, version);
            }
            Invariant::NoEPEscape(iseq) => {
                track_no_ep_escape_assumption(iseq, code_ptr, side_exit_ptr, version);
            }
            Invariant::SingleRactorMode => {
                track_single_ractor_assumption(code_ptr, side_exit_ptr, version);
            }
            Invariant::NoSingletonClass { klass } => {
                track_no_singleton_class_assumption(klass, code_ptr, side_exit_ptr, version);
            }
            Invariant::RootBoxOnly => {
                track_root_box_assumption(code_ptr, side_exit_ptr, version);
            }
        }
    });
}

pub(super) fn gen_side_exit(jit: &mut JITState, asm: &mut Assembler, function: &Function, reason: &SideExitReason, recompile: Option<Recompile>, state: &FrameState) {
    asm.jmp(side_exit_with_recompile(jit, function, state, *reason, recompile));
}

pub(super) fn gen_is_a(jit: &mut JITState, asm: &mut Assembler, obj: Opnd, class: Opnd) -> lir::Opnd {
    let builtin_type = match class {
        Opnd::Value(value) if value == unsafe { rb_cString } => Some(RUBY_T_STRING),
        Opnd::Value(value) if value == unsafe { rb_cArray } => Some(RUBY_T_ARRAY),
        Opnd::Value(value) if value == unsafe { rb_cHash } => Some(RUBY_T_HASH),
        _ => None
    };

    if let Some(builtin_type) = builtin_type {
        asm_comment!(asm, "IsA by matching builtin type");
        let hir_block_id = asm.current_block().hir_block_id;
        let rpo_idx = asm.current_block().rpo_index;

        // Create a result block that all paths converge to
        let result_block = asm.new_block(hir_block_id, false, rpo_idx);
        let result_edge = |v| Target::Block(Box::new(lir::BranchEdge {
            target: result_block,
            args: vec![v],
        }));

        let val = asm.load_mem(obj);

        // Immediate -> definitely not String/Array/Hash
        asm.test(val, Opnd::UImm(RUBY_IMMEDIATE_MASK as u64));
        asm.jnz(jit, result_edge(Qfalse.into()));

        // Qfalse -> definitely not String/Array/Hash
        asm.cmp(val, Qfalse.into());
        asm.je(jit, result_edge(Qfalse.into()));

        // Heap object -> check builtin type
        let flags = asm.load(Opnd::mem(VALUE_BITS, val, RUBY_OFFSET_RBASIC_FLAGS));
        let obj_builtin_type = asm.and(flags, Opnd::UImm(RUBY_T_MASK as u64));
        asm.cmp(obj_builtin_type, Opnd::UImm(builtin_type as u64));
        let result = asm.csel_e(Qtrue.into(), Qfalse.into());
        asm.jmp(result_edge(result));

        // Result block -- receives the value via block parameter (phi node)
        asm.set_current_block(result_block);
        let label = jit.get_label(asm, result_block, hir_block_id);
        asm.write_label(label);
        let param = asm.new_block_param(VALUE_BITS);
        asm.current_block().add_parameter(param);
        param
    } else {
        asm_ccall!(asm, rb_obj_is_kind_of, obj, class)
    }
}

pub(super) fn gen_is_method_cfunc(asm: &mut Assembler, val: lir::Opnd, cd: *const rb_call_data, cfunc: *const u8, state: &FrameState) -> lir::Opnd {
    unsafe extern "C" {
        fn rb_vm_method_cfunc_is(iseq: IseqPtr, cd: *const rb_call_data, recv: VALUE, cfunc: *const u8) -> VALUE;
    }
    asm_ccall!(asm, rb_vm_method_cfunc_is, VALUE::from(state.iseq).into(), Opnd::const_ptr(cd), val, Opnd::const_ptr(cfunc))
}

pub(super) fn gen_has_type(jit: &mut JITState, asm: &mut Assembler, val: lir::Opnd, val_type: Type, ty: Type) -> lir::Opnd {
    if ty.is_subtype(types::Fixnum) {
        asm.test(val, Opnd::UImm(RUBY_FIXNUM_FLAG as u64));
        asm.csel_nz(Opnd::Imm(1), Opnd::Imm(0))
    } else if ty.is_subtype(types::Flonum) {
        // Flonum: (val & RUBY_FLONUM_MASK) == RUBY_FLONUM_FLAG
        let masked = asm.and(val, Opnd::UImm(RUBY_FLONUM_MASK as u64));
        asm.cmp(masked, Opnd::UImm(RUBY_FLONUM_FLAG as u64));
        asm.csel_e(Opnd::Imm(1), Opnd::Imm(0))
    } else if ty.is_subtype(types::StaticSymbol) {
        // Static symbols have (val & 0xff) == RUBY_SYMBOL_FLAG
        // Use 8-bit comparison like YJIT does.
        // If `val` is a constant (rare but possible), put it in a register to allow masking.
        let val = asm.load_imm(val);
        asm.cmp(val.with_num_bits(8), Opnd::UImm(RUBY_SYMBOL_FLAG as u64));
        asm.csel_e(Opnd::Imm(1), Opnd::Imm(0))
    } else if ty.is_subtype(types::NilClass) {
        asm.cmp(val, Qnil.into());
        asm.csel_e(Opnd::Imm(1), Opnd::Imm(0))
    } else if ty.is_subtype(types::TrueClass) {
        asm.cmp(val, Qtrue.into());
        asm.csel_e(Opnd::Imm(1), Opnd::Imm(0))
    } else if ty.is_subtype(types::FalseClass) {
        asm.cmp(val, Qfalse.into());
        asm.csel_e(Opnd::Imm(1), Opnd::Imm(0))
    } else if ty.is_immediate() {
        // All immediate types' guard should have been handled above
        panic!("unexpected immediate guard type: {ty}");
    } else if let Some(expected_class) = ty.runtime_exact_ruby_class() {
        let hir_block_id = asm.current_block().hir_block_id;
        let rpo_idx = asm.current_block().rpo_index;

        // Create a result block that all paths converge to
        let result_block = asm.new_block(hir_block_id, false, rpo_idx);
        let result_edge = |v| Target::Block(Box::new(lir::BranchEdge {
            target: result_block,
            args: vec![v],
        }));

        // If val isn't in a register, load it to use it as the base of Opnd::mem later.
        // TODO: Max thinks codegen should not care about the shapes of the operands except to create them. (Shopify/ruby#685)
        let val = asm.load_mem(val);

        let is_known_heap_basic_object = val_type.is_subtype(types::HeapBasicObject);
        if !is_known_heap_basic_object {
            // Immediate -> definitely not the class
            asm.test(val, (RUBY_IMMEDIATE_MASK as u64).into());
            asm.jnz(jit, result_edge(Opnd::Imm(0)));

            // Qfalse -> definitely not the class
            asm.cmp(val, Qfalse.into());
            asm.je(jit, result_edge(Opnd::Imm(0)));
        }

        // Heap object -> check klass field
        let klass = asm.load(Opnd::mem(64, val, RUBY_OFFSET_RBASIC_KLASS));
        asm.cmp(klass, Opnd::Value(expected_class));
        let result = asm.csel_e(Opnd::UImm(1), Opnd::Imm(0));
        asm.jmp(result_edge(result));

        // Result block -- receives the value via block parameter (phi node)
        asm.set_current_block(result_block);
        let label = jit.get_label(asm, result_block, hir_block_id);
        asm.write_label(label);
        let param = asm.new_block_param(VALUE_BITS);
        asm.current_block().add_parameter(param);
        param
    } else if let Some(builtin_type) = ty.builtin_type_equivalent() {
        let hir_block_id = asm.current_block().hir_block_id;
        let rpo_idx = asm.current_block().rpo_index;

        // Create a result block that all paths converge to
        let result_block = asm.new_block(hir_block_id, false, rpo_idx);
        let result_edge = |v| Target::Block(Box::new(lir::BranchEdge {
            target: result_block,
            args: vec![v],
        }));

        // If val isn't in a register, load it to use it as the base of Opnd::mem later.
        let val = asm.load_mem(val);

        let is_known_heap_basic_object = val_type.is_subtype(types::HeapBasicObject);
        if !is_known_heap_basic_object {
            // Immediate -> definitely not the class
            asm.test(val, (RUBY_IMMEDIATE_MASK as u64).into());
            asm.jnz(jit, result_edge(Opnd::Imm(0)));

            // Qfalse -> definitely not the class
            asm.cmp(val, Qfalse.into());
            asm.je(jit, result_edge(Opnd::Imm(0)));
        }

        // Heap object
        // Mask and check the builtin type
        let flags = asm.load(Opnd::mem(VALUE_BITS, val, RUBY_OFFSET_RBASIC_FLAGS));
        let tag   = asm.and(flags, Opnd::UImm(RUBY_T_MASK as u64));
        asm.cmp(tag, Opnd::UImm(builtin_type as u64));
        let result = asm.csel_e(Opnd::UImm(1), Opnd::Imm(0));
        asm.jmp(result_edge(result));

        // Result block -- receives the value via block parameter (phi node)
        asm.set_current_block(result_block);
        let label = jit.get_label(asm, result_block, hir_block_id);
        asm.write_label(label);
        let param = asm.new_block_param(VALUE_BITS);
        asm.current_block().add_parameter(param);
        param
    } else {
        unimplemented!("unsupported type: {ty}");
    }
}

pub(super) fn gen_guard_type(jit: &mut JITState, asm: &mut Assembler, function: &Function, val: lir::Opnd, val_type: Type, guard_type: Type, recompile: Option<Recompile>, state: &FrameState) -> lir::Opnd {
    let is_known_heap_basic_object = val_type.is_subtype(types::HeapBasicObject);
    gen_incr_counter(asm, Counter::guard_type_count);
    if guard_type.is_subtype(types::Fixnum) {
        asm.test(val, Opnd::UImm(RUBY_FIXNUM_FLAG as u64));
        asm.jz(jit, side_exit_with_recompile(jit, function, state, GuardType(guard_type), recompile));
    } else if guard_type.is_subtype(types::Flonum) {
        // Flonum: (val & RUBY_FLONUM_MASK) == RUBY_FLONUM_FLAG
        let masked = asm.and(val, Opnd::UImm(RUBY_FLONUM_MASK as u64));
        asm.cmp(masked, Opnd::UImm(RUBY_FLONUM_FLAG as u64));
        asm.jne(jit, side_exit_with_recompile(jit, function, state, GuardType(guard_type), recompile));
    } else if guard_type.is_subtype(types::StaticSymbol) {
        // Static symbols have (val & 0xff) == RUBY_SYMBOL_FLAG
        // Use 8-bit comparison like YJIT does.
        // If `val` is a constant (rare but possible), put it in a register to allow masking.
        let val = asm.load_imm(val);
        asm.cmp(val.with_num_bits(8), Opnd::UImm(RUBY_SYMBOL_FLAG as u64));
        asm.jne(jit, side_exit_with_recompile(jit, function, state, GuardType(guard_type), recompile));
    } else if guard_type.is_subtype(types::NilClass) {
        asm.cmp(val, Qnil.into());
        asm.jne(jit, side_exit_with_recompile(jit, function, state, GuardType(guard_type), recompile));
    } else if guard_type.is_subtype(types::TrueClass) {
        asm.cmp(val, Qtrue.into());
        asm.jne(jit, side_exit_with_recompile(jit, function, state, GuardType(guard_type), recompile));
    } else if guard_type.is_subtype(types::FalseClass) {
        asm.cmp(val, Qfalse.into());
        asm.jne(jit, side_exit_with_recompile(jit, function, state, GuardType(guard_type), recompile));
    } else if guard_type.is_immediate() {
        // All immediate types' guard should have been handled above
        panic!("unexpected immediate guard type: {guard_type}");
    } else if let Some(expected_class) = guard_type.runtime_exact_ruby_class() {
        asm_comment!(asm, "guard exact class for non-immediate types");

        // If val isn't in a register, load it to use it as the base of Opnd::mem later.
        // TODO: Max thinks codegen should not care about the shapes of the operands except to create them. (Shopify/ruby#685)
        let val = asm.load_mem(val);

        let side_exit = side_exit_with_recompile(jit, function, state, GuardType(guard_type), recompile);
        if !is_known_heap_basic_object {
            // Check if it's a special constant
            asm.test(val, (RUBY_IMMEDIATE_MASK as u64).into());
            asm.jnz(jit, side_exit.clone());

            // Check if it's false
            asm.cmp(val, Qfalse.into());
            asm.je(jit, side_exit.clone());
        }

        // Load the class from the object's klass field
        let klass = asm.load(Opnd::mem(64, val, RUBY_OFFSET_RBASIC_KLASS));

        asm.cmp(klass, Opnd::Value(expected_class));
        asm.jne(jit, side_exit);
    } else if let Some(builtin_type) = guard_type.builtin_type_equivalent() {
        let side = side_exit_with_recompile(jit, function, state, GuardType(guard_type), recompile);

        if !is_known_heap_basic_object {
            // Check special constant
            asm.test(val, Opnd::UImm(RUBY_IMMEDIATE_MASK as u64));
            asm.jnz(jit, side.clone());

            // Check false
            asm.cmp(val, Qfalse.into());
            asm.je(jit, side.clone());
        }

        // Mask and check the builtin type
        let val = asm.load_mem(val);
        let flags = asm.load(Opnd::mem(VALUE_BITS, val, RUBY_OFFSET_RBASIC_FLAGS));
        let tag   = asm.and(flags, Opnd::UImm(RUBY_T_MASK as u64));
        asm.cmp(tag, Opnd::UImm(builtin_type as u64));
        asm.jne(jit, side);
    } else if guard_type.bit_equal(types::HeapBasicObject) {
        let side_exit = side_exit_with_recompile(jit, function, state, GuardType(guard_type), recompile);
        asm.cmp(val, Opnd::Value(Qfalse));
        asm.je(jit, side_exit.clone());
        asm.test(val, (RUBY_IMMEDIATE_MASK as u64).into());
        asm.jnz(jit, side_exit);
    } else {
        unimplemented!("unsupported type: {guard_type}");
    }
    val
}

pub(super) fn gen_guard_bit_equals(jit: &mut JITState, asm: &mut Assembler, function: &Function, val: lir::Opnd, expected: hir::Const, reason: SideExitReason, recompile: Option<Recompile>, state: &FrameState) -> lir::Opnd {
    if matches!(reason, SideExitReason::GuardShape(_) ) {
        gen_incr_counter(asm, Counter::guard_shape_count);
    }
    let expected_opnd: Opnd = match expected {
        hir::Const::Value(v) => { Opnd::Value(v) }
        hir::Const::CInt64(v) => { v.into() }
        hir::Const::CPtr(v) => { Opnd::const_ptr(v) }
        hir::Const::CShape(v) => { Opnd::UImm(v.0 as u64) }
        _ => panic!("gen_guard_bit_equals: unexpected hir::Const {expected:?}"),
    };
    asm.cmp(val, expected_opnd);
    asm.jnz(jit, side_exit_with_recompile(jit, function, state, reason, recompile));
    val
}

pub(super) fn mask_to_opnd(mask: hir::Const) -> Option<Opnd> {
    match mask {
        hir::Const::CUInt8(v) => Some(Opnd::UImm(v as u64)),
        hir::Const::CUInt16(v) => Some(Opnd::UImm(v as u64)),
        hir::Const::CUInt32(v) => Some(Opnd::UImm(v as u64)),
        hir::Const::CUInt64(v) => Some(Opnd::UImm(v)),
        _ => None
    }
}

pub(super) fn gen_guard_any_bit_set(jit: &mut JITState, asm: &mut Assembler, function: &Function, val: lir::Opnd, mask: hir::Const, reason: SideExitReason, recompile: Option<Recompile>, state: &FrameState) -> lir::Opnd {
    let mask_opnd = mask_to_opnd(mask).unwrap_or_else(|| panic!("gen_guard_any_bit_set: unexpected hir::Const {mask:?}"));
    asm.test(val, mask_opnd);
    asm.jz(jit, side_exit_with_recompile(jit, function, state, reason, recompile));
    val
}

pub(super) fn gen_guard_no_bits_set(jit: &mut JITState, asm: &mut Assembler, function: &Function, val: lir::Opnd, mask: hir::Const, reason: SideExitReason, state: &FrameState) -> lir::Opnd {
    let mask_opnd = mask_to_opnd(mask).unwrap_or_else(|| panic!("gen_guard_no_bits_set: unexpected hir::Const {mask:?}"));
    asm.test(val, mask_opnd);
    asm.jnz(jit, side_exit(jit, function, state, reason));
    val
}

pub(super) fn side_exit(jit: &JITState, function: &Function, state: &FrameState, reason: SideExitReason) -> Target {
    let exit = build_side_exit(jit, function, state);
    Target::SideExit(Box::new(SideExitTarget { exit, reason }))
}

pub(super) fn side_exit_with_recompile(jit: &JITState, function: &Function, state: &FrameState, reason: SideExitReason, recompile: Option<Recompile>) -> Target {
    let mut exit = build_side_exit(jit, function, state);
    exit.recompile = recompile.map(|_| SideExitRecompile {
        compiled_iseq: Opnd::Value(VALUE::from(jit.iseq())),
        frame_iseq: Opnd::Value(VALUE::from(state.iseq)),
        insn_idx: state.insn_idx() as u32,
    });
    Target::SideExit(Box::new(SideExitTarget { exit, reason }))
}

pub(super) fn build_side_exit(jit: &JITState, function: &Function, state: &FrameState) -> SideExit {
    let mut stack = Vec::new();
    for &insn_id in state.stack() {
        stack.push(jit.get_opnd(insn_id));
    }

    let mut locals = Vec::new();
    for &insn_id in state.locals() {
        locals.push(jit.get_opnd(insn_id));
    }

    SideExit{
        pc: Opnd::const_ptr(state.pc),
        stack,
        locals,
        iseq: state.iseq,
        stack_map: build_caller_stack_map(jit, function, state),
        recompile: None,
    }
}

pub(super) fn build_caller_stack_map(jit: &JITState, function: &Function, state: &FrameState) -> Option<StackMap> {
    let caller = state.caller()?;
    let caller_state = function.frame_state(caller);
    let stack_map = build_stack_map(jit, function, &caller_state);
    if stack_map.is_empty() {
        return None;
    }

    let jit_frame = jit_frame_for_state(&caller_state, stack_map.len());
    Some(StackMap::new(stack_map, jit_frame, caller_state.depth))
}
