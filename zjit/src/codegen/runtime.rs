//! VM state and value lowering.

use std::ffi::c_int;

use crate::backend::lir::{self, asm_ccall, asm_comment, Assembler, CFP, EC, Opnd, SP, Target};
use crate::cruby::*;
use crate::hir::{FieldName, FrameState, Function, SpecialBackrefSymbol, SpecialObjectType};
use crate::hir::SideExitReason;
use crate::hir_type::Type;
use crate::stats::Counter;
use super::{
    JITState, gen_incr_counter, gen_prepare_leaf_call_with_gc, gen_prepare_non_leaf_call,
    side_exit,
};
use super::calls::gen_trace_fallback;

pub(super) fn gen_get_ep(asm: &mut Assembler, level: u32) -> Opnd {
    // Load environment pointer EP from CFP into a register
    let ep_opnd = Opnd::mem(64, CFP, RUBY_OFFSET_CFP_EP);
    let mut ep_opnd = asm.load(ep_opnd);

    for _ in 0..level {
        // Get the previous EP from the current EP
        // See GET_PREV_EP(ep) macro
        // VALUE *prev_ep = ((VALUE *)((ep)[VM_ENV_DATA_INDEX_SPECVAL] & ~0x03))
        const UNTAGGING_MASK: Opnd = Opnd::Imm(!0x03);
        let offset = SIZEOF_VALUE_I32 * VM_ENV_DATA_INDEX_SPECVAL;
        ep_opnd = asm.load(Opnd::mem(64, ep_opnd, offset));
        ep_opnd = asm.and(ep_opnd, UNTAGGING_MASK);
    }

    ep_opnd
}

pub(super) fn gen_defined(jit: &JITState, asm: &mut Assembler, function: &Function, op_type: defined_type, obj: VALUE, pushval: VALUE, tested_value: Opnd, lep_level: u32, state: &FrameState) -> Opnd {
    match op_type as defined_type {
        DEFINED_YIELD => {
            // `lep_level` was precomputed at HIR construction so we can materialize the local EP
            // inline without walking the parent iseq chain here.
            let lep = gen_get_ep(asm, lep_level);
            let block_handler = asm.load(Opnd::mem(64, lep, SIZEOF_VALUE_I32 * VM_ENV_DATA_INDEX_SPECVAL));
            let pushval = asm.load(pushval.into());
            asm.cmp(block_handler, VM_BLOCK_HANDLER_NONE.into());
            asm.csel_e(Qnil.into(), pushval)
        }
        _ => {
            // Save the PC and SP because the callee may allocate or call #respond_to?
            gen_prepare_non_leaf_call(jit, asm, function, state);

            // TODO: Inline the cases for each op_type
            // Call vm_defined(ec, reg_cfp, op_type, obj, v)
            let def_result = asm_ccall!(asm, rb_vm_defined, EC, CFP, op_type.into(), obj.into(), tested_value);

            asm.cmp(def_result.with_num_bits(8), 0.into());
            asm.csel_ne(pushval.into(), Qnil.into())
        }
    }
}

pub(super) fn gen_is_block_given(asm: &mut Assembler, block_handler: Opnd) -> Opnd {
    asm.cmp(block_handler, VM_BLOCK_HANDLER_NONE.into());
    asm.csel_e(Qfalse.into(), Qtrue.into())
}

pub(super) fn gen_setlocal(asm: &mut Assembler, val: Opnd, val_type: Type, local_ep_offset: u32, level: u32) {
    let local_ep_offset = c_int::try_from(local_ep_offset).unwrap_or_else(|_| panic!("Could not convert local_ep_offset {local_ep_offset} to i32"));
    if level > 0 {
        gen_incr_counter(asm, Counter::vm_write_to_parent_iseq_local_count);
    }
    let ep = gen_get_ep(asm, level);

    // When we've proved that we're writing an immediate,
    // we can skip the write barrier.
    if val_type.is_immediate() {
        let offset = -(SIZEOF_VALUE_I32 * local_ep_offset);
        asm.mov(Opnd::mem(64, ep, offset), val);
    } else {
        // We're potentially writing a reference to an IMEMO/env object,
        // so take care of the write barrier with a function.
        let local_index = -local_ep_offset;
        asm_ccall!(asm, rb_vm_env_write, ep, local_index.into(), val);
    }
}

pub(super) fn gen_is_block_param_modified(asm: &mut Assembler, flags: Opnd) -> Opnd {
    asm.test(flags, VM_FRAME_FLAG_MODIFIED_BLOCK_PARAM.into());
    asm.csel_nz(Opnd::Imm(1), Opnd::Imm(0))
}

pub(super) fn gen_getblockparam(jit: &mut JITState, asm: &mut Assembler, function: &Function, ep_offset: u32, level: u32, state: &FrameState) -> Opnd {
    gen_prepare_leaf_call_with_gc(asm, state);
    // Bail out if write barrier is required.
    let ep = gen_get_ep(asm, level);
    let flags = Opnd::mem(VALUE_BITS, ep, SIZEOF_VALUE_I32 * (VM_ENV_DATA_INDEX_FLAGS as i32));
    asm.test(flags, VM_ENV_FLAG_WB_REQUIRED.into());
    asm.jnz(jit, side_exit(jit, function, state, SideExitReason::BlockParamWbRequired));

    // Convert block handler to Proc.
    let block_handler = asm.load(Opnd::mem(VALUE_BITS, ep, SIZEOF_VALUE_I32 * VM_ENV_DATA_INDEX_SPECVAL));
    let proc = asm_ccall!(asm, rb_vm_bh_to_procval, EC, block_handler);

    let local_ep_offset = c_int::try_from(ep_offset).unwrap_or_else(|_| {
        panic!("Could not convert local_ep_offset {ep_offset} to i32")
    });
    let offset = -(SIZEOF_VALUE_I32 * local_ep_offset);
    asm.mov(Opnd::mem(VALUE_BITS, ep, offset), proc);

    let flags = Opnd::mem(VALUE_BITS, ep, SIZEOF_VALUE_I32 * (VM_ENV_DATA_INDEX_FLAGS as i32));
    let flags_val = asm.load(flags);
    let modified = asm.or(flags_val, VM_FRAME_FLAG_MODIFIED_BLOCK_PARAM.into());
    asm.store(flags, modified);

    asm.load(Opnd::mem(VALUE_BITS, ep, offset))
}

pub(super) fn gen_get_constant_path(jit: &JITState, asm: &mut Assembler, function: &Function, ic: *const iseq_inline_constant_cache, state: &FrameState) -> Opnd {
    unsafe extern "C" {
        fn rb_vm_opt_getconstant_path(ec: EcPtr, cfp: CfpPtr, ic: *const iseq_inline_constant_cache) -> VALUE;
    }

    // Anything could be called on const_missing
    gen_prepare_non_leaf_call(jit, asm, function, state);

    asm_ccall!(asm, rb_vm_opt_getconstant_path, EC, CFP, Opnd::const_ptr(ic))
}

pub(super) fn gen_getconstant(jit: &mut JITState, asm: &mut Assembler, function: &Function, klass: Opnd, id: ID, allow_nil: Opnd, state: &FrameState) -> Opnd {
    unsafe extern "C" {
        fn rb_vm_get_ev_const(ec: EcPtr, klass: VALUE, id: ID, allow_nil: VALUE) -> VALUE;
    }

    // Constant lookup can raise and run arbitrary Ruby code via const_missing.
    gen_prepare_non_leaf_call(jit, asm, function, state);

    asm_ccall!(asm, rb_vm_get_ev_const, EC, klass, id.0.into(), allow_nil)
}

pub(super) fn gen_getivar(asm: &mut Assembler, recv: Opnd, id: ID, ic: *const iseq_inline_iv_cache_entry, state: &FrameState) -> Opnd {
    gen_trace_fallback(asm, "getivar");
    if ic.is_null() {
        asm_ccall!(asm, rb_ivar_get, recv, id.0.into())
    } else {
        let iseq = Opnd::Value(state.iseq.into());
        asm_ccall!(asm, rb_vm_getinstancevariable, iseq, recv, id.0.into(), Opnd::const_ptr(ic))
    }
}

pub(super) fn gen_setivar(jit: &mut JITState, asm: &mut Assembler, function: &Function, recv: Opnd, id: ID, ic: *const iseq_inline_iv_cache_entry, val: Opnd, state: &FrameState) {
    gen_trace_fallback(asm, "setivar");
    // Setting an ivar can raise FrozenError, so we need proper frame state for exception handling.
    gen_prepare_non_leaf_call(jit, asm, function, state);
    if ic.is_null() {
        asm_ccall!(asm, rb_ivar_set, recv, id.0.into(), val);
    } else {
        let iseq = Opnd::Value(state.iseq.into());
        asm_ccall!(asm, rb_vm_setinstancevariable, iseq, recv, id.0.into(), val, Opnd::const_ptr(ic));
    }
}

pub(super) fn gen_getclassvar(jit: &mut JITState, asm: &mut Assembler, function: &Function, id: ID, ic: *const iseq_inline_cvar_cache_entry, state: &FrameState) -> Opnd {
    gen_prepare_non_leaf_call(jit, asm, function, state);
    asm_ccall!(asm, rb_vm_getclassvariable, VALUE::from(state.iseq).into(), CFP, id.0.into(), Opnd::const_ptr(ic))
}

pub(super) fn gen_setclassvar(jit: &mut JITState, asm: &mut Assembler, function: &Function, id: ID, val: Opnd, ic: *const iseq_inline_cvar_cache_entry, state: &FrameState) {
    gen_prepare_non_leaf_call(jit, asm, function, state);
    asm_ccall!(asm, rb_vm_setclassvariable, VALUE::from(state.iseq).into(), CFP, id.0.into(), val, Opnd::const_ptr(ic));
}

pub(super) fn gen_getglobal(jit: &mut JITState, asm: &mut Assembler, function: &Function, id: ID, state: &FrameState) -> Opnd {
    // `Warning` module's method `warn` can be called when reading certain global variables
    gen_prepare_non_leaf_call(jit, asm, function, state);

    asm_ccall!(asm, rb_gvar_get, id.0.into())
}

pub(super) fn gen_setglobal(jit: &mut JITState, asm: &mut Assembler, function: &Function, id: ID, val: Opnd, state: &FrameState) {
    // When trace_var is used, setting a global variable can cause exceptions
    gen_prepare_non_leaf_call(jit, asm, function, state);

    asm_ccall!(asm, rb_gvar_set, id.0.into(), val);
}

pub(super) fn gen_putspecialobject(jit: &JITState, asm: &mut Assembler, function: &Function, value_type: SpecialObjectType, state: &FrameState) -> Opnd {
    // rb_vm_get_special_object for CBASE/CONST_BASE can call rb_singleton_class,
    // which allocates (may trigger GC) and can raise TypeError on non-class
    // receivers (e.g. `123.instance_eval { Const = 1 }`). Treat as non-leaf so
    // the PC is saved for GC and stack/locals are spilled for rescue.
    gen_prepare_non_leaf_call(jit, asm, function, state);

    // Get the EP of the current CFP and load it into a register
    let ep_opnd = Opnd::mem(64, CFP, RUBY_OFFSET_CFP_EP);
    let ep_reg = asm.load(ep_opnd);

    asm_ccall!(asm, rb_vm_get_special_object, ep_reg, Opnd::UImm(u64::from(value_type)))
}

pub(super) fn gen_getspecial_symbol(asm: &mut Assembler, symbol_type: SpecialBackrefSymbol, state: &FrameState) -> Opnd {
    // rb_backref_get reaches rb_vm_svar_lep, which calls CFP_PC/CFP_ISEQ on the
    // current frame, so the PC must be saved before the call.
    gen_prepare_leaf_call_with_gc(asm, state);

    // Fetch a "special" backref based on the symbol type
    let backref = asm_ccall!(asm, rb_backref_get,);

    match symbol_type {
        SpecialBackrefSymbol::LastMatch => {
            asm_ccall!(asm, rb_reg_last_match, backref)
        }
        SpecialBackrefSymbol::PreMatch => {
            asm_ccall!(asm, rb_reg_match_pre, backref)
        }
        SpecialBackrefSymbol::PostMatch => {
            asm_ccall!(asm, rb_reg_match_post, backref)
        }
        SpecialBackrefSymbol::LastGroup => {
            asm_ccall!(asm, rb_reg_match_last, backref)
        }
    }
}

pub(super) fn gen_getspecial_number(asm: &mut Assembler, nth: u64, state: &FrameState) -> Opnd {
    // rb_backref_get reaches rb_vm_svar_lep, which calls CFP_PC/CFP_ISEQ on the
    // current frame, so the PC must be saved before the call.
    gen_prepare_leaf_call_with_gc(asm, state);

    // Fetch the N-th match from the last backref based on type shifted by 1
    let backref = asm_ccall!(asm, rb_backref_get,);

    asm_ccall!(asm, rb_reg_nth_match, Opnd::Imm((nth >> 1).try_into().unwrap()), backref)
}

pub(super) fn gen_check_interrupts(jit: &mut JITState, asm: &mut Assembler, function: &Function, state: &FrameState) {
    // Check for interrupts
    // see RUBY_VM_CHECK_INTS(ec) macro
    asm_comment!(asm, "RUBY_VM_CHECK_INTS(ec)");
    // Not checking interrupt_mask since it's zero outside finalize_deferred_heap_pages,
    // signal_exec, or rb_postponed_job_flush.
    let interrupt_flag = asm.load(Opnd::mem(32, EC, RUBY_OFFSET_EC_INTERRUPT_FLAG));
    asm.test(interrupt_flag, interrupt_flag);
    asm.jnz(jit, side_exit(jit, function, state, SideExitReason::Interrupt));
}

pub(super) fn gen_defined_ivar(asm: &mut Assembler, self_val: Opnd, id: ID, pushval: VALUE) -> lir::Opnd {
    asm_ccall!(asm, rb_zjit_defined_ivar, self_val, id.0.into(), Opnd::Value(pushval))
}

pub(super) fn gen_checkmatch(jit: &JITState, asm: &mut Assembler, function: &Function, target: Opnd, pattern: Opnd, flag: u32, state: &FrameState) -> lir::Opnd {
    // rb_vm_check_match is not leaf unless flag is VM_CHECKMATCH_TYPE_WHEN.
    // See also: leafness_of_checkmatch() and check_match()
    if flag != VM_CHECKMATCH_TYPE_WHEN {
        gen_prepare_non_leaf_call(jit, asm, function, state);
    }

    unsafe extern "C" {
        fn rb_vm_check_match(ec: EcPtr, target: VALUE, pattern: VALUE, flag: u32) -> VALUE;
    }

    asm_ccall!(asm, rb_vm_check_match, EC, target, pattern, flag.into())
}

pub(super) fn gen_load_pc(asm: &mut Assembler) -> Opnd {
    asm.load(Opnd::mem(64, CFP, RUBY_OFFSET_CFP_PC))
}

pub(super) fn gen_load_ec() -> Opnd {
    EC
}

pub(super) fn gen_load_sp() -> Opnd {
    SP
}

pub(super) fn gen_load_self(asm: &mut Assembler) -> Opnd {
    asm.load(Opnd::mem(64, CFP, RUBY_OFFSET_CFP_SELF))
}

pub(super) fn gen_load_field(asm: &mut Assembler, recv: Opnd, id: FieldName, offset: i32, num_bits: u8) -> Opnd {
    gen_incr_counter(asm, Counter::load_field_count);
    asm_comment!(asm, "Load field id={id} offset={offset}");
    let recv = asm.load_mem(recv);
    asm.load(Opnd::mem(num_bits, recv, offset))
}

pub(super) fn gen_store_field(asm: &mut Assembler, recv: Opnd, id: FieldName, offset: i32, val: Opnd, num_bits: u8) {
    gen_incr_counter(asm, Counter::store_field_count);
    asm_comment!(asm, "Store field id={id} offset={offset}");
    let recv = asm.load_mem(recv);
    asm.store(Opnd::mem(num_bits, recv, offset), val);
}

pub(super) fn gen_write_barrier(jit: &mut JITState, asm: &mut Assembler, recv: Opnd, val: Opnd, val_type: Type) {
    // See RB_OBJ_WRITE/rb_obj_write: it's just assignment and rb_obj_written().
    // rb_obj_written() does: if (!RB_SPECIAL_CONST_P(val)) { rb_gc_writebarrier(recv, val); }
    if !val_type.is_immediate() {
        asm_comment!(asm, "Write barrier");
        let recv = asm.load_mem(recv);

        // Create a result block that all paths converge to
        let hir_block_id = asm.current_block().hir_block_id;
        let rpo_idx = asm.current_block().rpo_index;
        let result_block = asm.new_block(hir_block_id, false, rpo_idx);
        let result_edge = Target::Block(Box::new(lir::BranchEdge { target: result_block, args: vec![] }));

        // If non-false immediate, don't fire write barrier
        asm.test(val, Opnd::UImm(RUBY_IMMEDIATE_MASK as u64));
        asm.jnz(jit, result_edge.clone());

        // If false, don't fire write barrier
        asm.cmp(val, Qfalse.into());
        asm.je(jit, result_edge.clone());

        // Heap object; fire the write barrier
        asm_ccall!(asm, rb_gc_writebarrier, recv, val);
        asm.jmp(result_edge);

        // Join block
        asm.set_current_block(result_block);
        let label = jit.get_label(asm, result_block, hir_block_id);
        asm.write_label(label);
    }
}

pub(super) fn gen_const_value(val: VALUE) -> lir::Opnd {
    // Just propagate the constant value and generate nothing
    Opnd::Value(val)
}

pub(super) fn gen_const_cptr(val: *const u8) -> lir::Opnd {
    Opnd::const_ptr(val)
}

pub(super) fn gen_const_long(val: i64) -> lir::Opnd {
    Opnd::Imm(val)
}

pub(super) fn gen_const_uint16(val: u16) -> lir::Opnd {
    Opnd::UImm(val as u64)
}

pub(super) fn gen_const_uint32(val: u32) -> lir::Opnd {
    Opnd::UImm(val as u64)
}

pub(super) fn gen_const_attr_index_t(val: attr_index_t) -> lir::Opnd {
    Opnd::UImm(val as u64)
}

pub(super) fn gen_param(asm: &mut Assembler, _idx: usize) -> lir::Opnd {
    let vreg = asm.new_block_param(VALUE_BITS);
    asm.current_block().add_parameter(vreg);
    vreg
}
