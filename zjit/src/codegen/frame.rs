use crate::backend::lir::{self, asm_ccall, asm_comment, Assembler, C_RET_OPND, CFP, EC, NATIVE_BASE_PTR, Opnd, SP, StackMapEntry};
use crate::cruby::{
    get_iseq_body_local_table_size, insn_len, local_idx_to_ep_offset, rb_callable_method_entry_t,
    rb_iseq_pc_at_idx, CfpPtr, EcPtr, IseqPtr, VALUE, RUBY_OFFSET_CFP_BLOCK_CODE,
    RUBY_OFFSET_CFP_EP, RUBY_OFFSET_CFP_JIT_RETURN, RUBY_OFFSET_CFP_PC, RUBY_OFFSET_CFP_SELF,
    RUBY_OFFSET_CFP_SP, RUBY_OFFSET_EC_CFP, RUBY_SIZEOF_CONTROL_FRAME, SIZEOF_VALUE,
    SIZEOF_VALUE_I32, VM_ENV_DATA_SIZE, ZJIT_JIT_RETURN_C_FRAME,
};
use crate::hir::{FrameState, Function, SideExitReason::StackOverflow};
use crate::options::InlineDepth;
use crate::stats::Counter;
use crate::cast::IntoUsize;

use super::{gen_incr_counter, side_exit, JITEntry, JITFrame, JITState, PC_POISON};
use crate::cruby::IseqAccess;
use crate::cruby::zjit_jit_frame;

/// Map an entry point to the bytecode PC used by its initial JITFrame.
/// JIT call entries use `opt_table[jit_entry_idx]`; the interpreter entry uses
/// `opt_table.last()` for the fall-through path where all optionals are filled.
pub(super) fn entry_pc(iseq: IseqPtr, jit_entry_idx: Option<usize>) -> *const VALUE {
    let params = unsafe { iseq.params() };
    let opt_table = params.opt_table_slice();
    let entry_idx = jit_entry_idx.unwrap_or_else(|| opt_table.len() - 1);
    let entry_insn_idx = opt_table.get(entry_idx)
        .unwrap_or_else(|| panic!("entry_pc: opt_table out of bounds. {params:#?}, entry_idx={entry_idx}"))
        .as_u32();
    unsafe { rb_iseq_pc_at_idx(iseq, entry_insn_idx) }
}

/// Compile a frame setup. If jit_entry_idx is Some, remember the address of it as a JIT entry.
pub(super) fn gen_entry_point(jit: &mut JITState, asm: &mut Assembler, jit_entry_idx: Option<usize>) {
    if let Some(jit_entry_idx) = jit_entry_idx {
        let jit_entry = JITEntry::new(jit_entry_idx);
        jit.jit_entries.push(jit_entry.clone());
        asm.pos_marker(move |code_ptr, _| {
            jit_entry.borrow_mut().start_addr.set(Some(code_ptr));
        });
    }
    asm.frame_setup(&[]);

    // Publish a valid entry JITFrame before setting cfp->jit_return. The entry point is
    // always the top-level frame (depth 0). Inlined frames get their own deeper
    // slots in gen_push_inline_frame().
    let jit_frame = JITFrame::new_iseq(entry_pc(jit.iseq(), jit_entry_idx), jit.iseq(), 0);
    asm.mov(Opnd::mem(64, NATIVE_BASE_PTR, -SIZEOF_VALUE_I32), Opnd::const_ptr(jit_frame));
    asm.mov(Opnd::mem(64, CFP, RUBY_OFFSET_CFP_JIT_RETURN), NATIVE_BASE_PTR);

    // Direct JIT-to-JIT callers switch the CFP register before calling this entry
    // point, but they leave ec->cfp pointing at the caller until cfp->jit_return
    // is valid so signal-based frame walkers never observe a half-published callee.
    asm.mov(Opnd::mem(64, EC, RUBY_OFFSET_EC_CFP), CFP);
}

/// Compile code that exits from JIT code with a return value
pub(super) fn gen_return(asm: &mut Assembler, val: lir::Opnd) {
    // Pop the current frame (ec->cfp++)
    // Note: the return PC is already in the previous CFP
    asm_comment!(asm, "pop stack frame");
    let incr_cfp = asm.add(CFP, RUBY_SIZEOF_CONTROL_FRAME.into());
    asm.mov(CFP, incr_cfp);
    asm.mov(Opnd::mem(64, EC, RUBY_OFFSET_EC_CFP), CFP);

    // Order here is important. Because we're about to tear down the frame,
    // we need to load the return value, which might be part of the frame.
    asm.load_into(C_RET_OPND, val);

    // Return from the function
    asm.frame_teardown(&[]); // matching the setup in gen_entry_point()
    asm.cret(C_RET_OPND);
}

pub(super) fn gen_throw(jit: &mut JITState, asm: &mut Assembler, function: &Function, throw_state: u32, val: lir::Opnd, state: &FrameState) {
    gen_incr_counter(asm, Counter::throw_count);

    // The interpreter pops the thrown value before calling vm_throw(), so keep it out of the cfp->sp we publish.
    let state = state.with_stack_size(state.stack_size() - 1); // -1 for popped throw value
    // rb_vm_throw() allocates with THROW_DATA_NEW() and may raise LocalJumpError, and the interpreter reads this
    // frame's locals and stack while unwinding, so publish them in the same way as any other non-leaf fallback call.
    gen_prepare_fallback_call(jit, asm, function, &state);

    asm_comment!(asm, "throw");
    unsafe extern "C" {
        fn rb_zjit_throw(ec: EcPtr, cfp: CfpPtr, throw_state: usize, throwobj: VALUE) -> VALUE;
    }
    asm_ccall!(asm, rb_zjit_throw, EC, CFP, Opnd::UImm(throw_state.into()), val);

    // rb_zjit_throw() never returns. Trap in case it somehow does, and end the
    // LIR block with an unreachable ret to give it a normal terminator.
    asm.abort();
    asm.cret(C_RET_OPND);
}
/// Byte offset from NATIVE_BASE_PTR of the JITFrame storage slot for a frame at
/// the given inlining depth. Depth 0 (the top-level frame) lives at
/// `[NATIVE_BASE_PTR - 8]`; each deeper inlined frame gets the next slot below.
/// gen_function() reserves `inlining_depth() + 1` slots, so every live frame's
/// depth maps to a distinct slot inside that reserved region.
pub(super) fn jit_frame_slot_offset(depth: InlineDepth) -> i32 {
    -(SIZEOF_VALUE_I32 * (depth as i32 + 1))
}

/// Compute the value to store in a frame's `cfp->jit_return` for the given
/// inlining depth. CFP_ZJIT_FRAME(cfp) reads the JITFrame pointer from
/// `((VALUE *)cfp->jit_return)[-1]`, so jit_return must point one VALUE above
/// the frame's storage slot (see jit_frame_slot_offset()). Depth 0 lands exactly
/// on NATIVE_BASE_PTR, matching the non-inlined protocol; deeper frames need an
/// address computed relative to it.
pub(super) fn cfp_jit_return_for_depth(asm: &mut Assembler, depth: InlineDepth) -> Opnd {
    if depth == 0 {
        NATIVE_BASE_PTR
    } else {
        asm.lea(Opnd::mem(64, NATIVE_BASE_PTR, -(SIZEOF_VALUE_I32 * depth as i32)))
    }
}

pub(super) fn jit_frame_next_pc(state: &FrameState) -> *const VALUE {
    let opcode: usize = state.get_opcode().try_into().unwrap();
    unsafe { state.pc.offset(insn_len(opcode) as isize) }
}

pub(super) fn jit_frame_for_state(state: &FrameState, stack_map_size: usize) -> *const zjit_jit_frame {
    JITFrame::new_iseq(jit_frame_next_pc(state), state.iseq, stack_map_size)
}

/// Save only the PC to CFP. Use this when you need to call gen_save_sp()
/// immediately after with a custom stack size (e.g., gen_ccall_with_frame
/// adjusts SP to exclude receiver and arguments).
pub(super) fn gen_write_jit_frame(asm: &mut Assembler, state: &FrameState, stack_map_size: usize) -> *const zjit_jit_frame {
    gen_incr_counter(asm, Counter::vm_write_jit_frame_count);
    asm_comment!(asm, "save JITFrame to CFP");
    let jit_frame = jit_frame_for_state(state, stack_map_size);
    asm.mov(Opnd::mem(64, NATIVE_BASE_PTR, jit_frame_slot_offset(state.depth)), Opnd::const_ptr(jit_frame));

    // CFP_PC for a live JIT frame routes through the JITFrame on the native
    // stack (cfp->jit_return points at this frame's slot), so we don't need to
    // touch cfp->pc here. Poisoning cfp->pc with PC_POISON would actively
    // break the case where rb_zjit_materialize_frames() previously copied
    // jit_frame->pc into cfp->pc and cleared cfp->jit_return: the JIT keeps
    // running, lands on this routine again, and the poison would replace
    // the valid materialized pc behind the GC's back.
    jit_frame
}

/// Save the current PC on the CFP as a preparation for calling a C function
/// that may allocate objects and trigger GC. Use gen_prepare_non_leaf_call()
/// if it may raise exceptions or call arbitrary methods.
///
/// Unlike YJIT, we don't need to save the stack slots to protect them from GC
/// because the backend spills all live registers onto the C stack on CCall.
/// However, to avoid marking uninitialized stack slots, this also updates SP,
/// which may have cfp->sp for a past frame or a past non-leaf call.
pub(super) fn gen_prepare_call_with_gc(asm: &mut Assembler, state: &FrameState, leaf: bool, stack_map_size: usize) -> *const zjit_jit_frame {
    let jit_frame = gen_write_jit_frame(asm, state, stack_map_size);
    gen_save_sp(asm, state.stack_size());
    if leaf {
        asm.expect_leaf_ccall(state.stack_size());
    }
    jit_frame
}

pub(super) fn gen_prepare_leaf_call_with_gc(asm: &mut Assembler, state: &FrameState) {
    // In gen_prepare_call_with_gc(), we update cfp->sp for leaf calls too.
    //
    // Here, cfp->sp may be pointing to either of the following:
    //   1. cfp->sp for a past frame, which gen_push_frame() skips to initialize
    //   2. cfp->sp set by gen_prepare_non_leaf_call() for the current frame
    //
    // When (1), to avoid marking dead objects, we need to set cfp->sp for the current frame.
    // When (2), setting cfp->sp at gen_push_frame() and not updating cfp->sp here could lead to
    // keeping objects longer than it should, so we set cfp->sp at every call of this function.
    //
    // We use state.without_stack() to pass stack_size=0 to gen_save_sp() because we don't write
    // VM stack slots on leaf calls, which leaves those stack slots uninitialized. ZJIT keeps
    // live objects on the C stack, so they are protected from GC properly.
    gen_prepare_call_with_gc(asm, &state.without_stack(), true, 0);
}

/// Save the current SP on the CFP
pub(super) fn gen_save_sp(asm: &mut Assembler, stack_size: usize) {
    // Update cfp->sp which will be read by the interpreter. We also have the SP register in JIT
    // code, and ZJIT's codegen currently assumes the SP register doesn't move, e.g. gen_param().
    // So we don't update the SP register here. We could update the SP register to avoid using
    // an extra register for asm.lea(), but you'll need to manage the SP offset like YJIT does.
    gen_incr_counter(asm, Counter::vm_write_sp_count);
    asm_comment!(asm, "save SP to CFP: {}", stack_size);
    let sp_addr = asm.lea(Opnd::mem(64, SP, stack_size as i32 * SIZEOF_VALUE_I32));
    let cfp_sp = Opnd::mem(64, CFP, RUBY_OFFSET_CFP_SP);
    asm.mov(cfp_sp, sp_addr);
}

/// Spill locals onto the stack.
pub(super) fn gen_spill_locals(jit: &JITState, asm: &mut Assembler, state: &FrameState) {
    // TODO: Avoid spilling locals that have been spilled before and not changed.
    gen_incr_counter(asm, Counter::vm_write_locals_count);
    asm_comment!(asm, "spill locals");
    for (idx, &insn_id) in state.locals().enumerate() {
        asm.mov(Opnd::mem(64, SP, (-local_idx_to_ep_offset(state.iseq, idx) - 1) * SIZEOF_VALUE_I32), jit.get_opnd(insn_id));
    }
}

/// Spill the virtual stack onto the stack.
pub(super) fn gen_spill_stack(jit: &JITState, asm: &mut Assembler, function: &Function, state: &FrameState) {
    // This function does not call gen_save_sp() at the moment because
    // gen_send_without_block_direct() spills stack slots above SP for arguments.
    gen_incr_counter(asm, Counter::vm_write_stack_count);
    asm_comment!(asm, "spill stack");

    let mut offset = state.stack_size() as i32;
    for entry in build_stack_map(jit, function, state) {
        match entry {
            StackMapEntry::Opnd(opnd) => {
                offset -= 1;
                asm.mov(Opnd::mem(64, SP, offset * SIZEOF_VALUE_I32), opnd);
            }
            StackMapEntry::Skip(skip) => {
                offset -= skip as i32;
            }
            // Only gen_prepare_non_leaf_call() prepends this, and it doesn't spill.
            StackMapEntry::BasePtr { .. } => unreachable!("build_stack_map() does not emit BasePtr"),
        }
    }
}

/// Prepare for VM fallback helpers that read arguments from the VM stack.
///
/// Direct JIT-to-JIT calls keep cfp->sp lazy, so this must publish SP before
/// writing stack slots. Otherwise spilling the stack can overwrite frame
/// metadata below the real VM-stack base.
pub(super) fn gen_prepare_fallback_call(jit: &JITState, asm: &mut Assembler, function: &Function, state: &FrameState) {
    gen_write_jit_frame(asm, state, 0);
    gen_save_sp(asm, state.stack_size());
    gen_spill_locals(jit, asm, state);
    gen_spill_stack(jit, asm, function, state);
}

/// Build entries for Ruby stack values that need materialization. The actual
/// JITFrame entries are encoded by the register allocator, where VReg locations
/// on the native stack are known.
pub(super) fn build_stack_map(jit: &JITState, function: &Function, state: &FrameState) -> Vec<StackMapEntry> {
    let mut stack = Vec::new();
    let mut current_state = state.clone();
    loop {
        stack.extend(current_state.stack().rev().copied().map(|insn_id| {
            let opnd = jit.get_opnd(insn_id);
            assert!(
                matches!(opnd, Opnd::Value(_) | Opnd::VReg { .. }),
                "FrameState should only reference Opnd::Value or Opnd::VReg, but got: {opnd:?}",
            );
            StackMapEntry::Opnd(opnd)
        }));

        let Some(caller) = current_state.caller() else {
            break;
        };
        stack.push(StackMapEntry::Skip(inline_frame_stack_gap(current_state.iseq)));
        current_state = function.frame_state(caller);
    }
    stack
}

pub(super) fn inline_frame_stack_gap(iseq: IseqPtr) -> usize {
    // The extra slot is for the callee's receiver below its local table.
    // We currently never map out the stack for `invokeblock`, which doesn't
    // put a receiver on cfp->sp stack.
    1 + unsafe { get_iseq_body_local_table_size(iseq) }.to_usize() + VM_ENV_DATA_SIZE.to_usize()
}

/// Prepare for calling a C function that may call an arbitrary method.
/// Use gen_prepare_leaf_call_with_gc() if the method is leaf but allocates objects.
pub(super) fn gen_prepare_non_leaf_call(jit: &JITState, asm: &mut Assembler, function: &Function, state: &FrameState) {
    // Anchor the stack map on a private copy of SP rather than on cfp->sp. The callee is free to
    // use the stack map after pushing through and moving cfp->sp (e.g. rb_funcall() + a raise in
    // vm_callee_setup_arg()).
    let mut stack_map = vec![StackMapEntry::BasePtr {
        slot_index: jit.base_ptr_slot_index(state.depth),
        stack_size: state.stack_size().try_into().expect("stack size overflow"),
    }];
    stack_map.extend(build_stack_map(jit, function, state));
    let jit_frame = gen_prepare_call_with_gc(asm, state, false, stack_map.len());

    // NOTE(alan): This store can be done once per CFP switch, but analysis is required
    //             to avoid the store in functions that make no non-leaf call.
    asm_comment!(asm, "save SP as the stack map anchor");
    asm.mov(Opnd::mem(64, NATIVE_BASE_PTR, jit.base_ptr_slot_native_base_ptr_offset()), SP);

    // Remember the stack map in case it raises an exception
    // and the interpreter uses the stack for handling the exception
    asm.stack_map(stack_map, jit_frame, state.depth);

    // Spill locals in case the method looks at caller Bindings
    gen_spill_locals(jit, asm, state);
}

/// Frame metadata written by gen_push_frame()
pub(super) struct ControlFrame {
    pub(super) recv: Opnd,
    pub(super) iseq: Option<IseqPtr>,
    pub(super) cme: *const rb_callable_method_entry_t,
    pub(super) frame_type: u32,
    /// The [`VM_ENV_DATA_INDEX_SPECVAL`] slot of the frame.
    /// For the type of frames we push, block handler or the parent EP.
    pub(super) specval: lir::Opnd,
    /// Whether to write block_code = 0 at frame push time.
    /// True when the callee ISEQ may write to block_code (has send/invokesuper/invokeblock).
    pub(super) write_block_code: bool,
}

/// Compile an interpreter frame
pub(super) fn gen_push_frame(asm: &mut Assembler, argc: usize, state: &FrameState, frame: ControlFrame) {
    // Locals are written by the callee frame on side-exits or non-leaf calls

    // See vm_push_frame() for details
    asm_comment!(asm, "push cme, specval, frame type");
    // ep[-2]: cref of cme
    let local_size = if let Some(iseq) = frame.iseq {
        (unsafe { get_iseq_body_local_table_size(iseq) }) as i32
    } else {
        0
    };
    let ep_offset = state.stack().len() as i32 + local_size - argc as i32 + VM_ENV_DATA_SIZE as i32 - 1;
    // ep[-2]: CME
    asm.store(Opnd::mem(64, SP, (ep_offset - 2) * SIZEOF_VALUE_I32), VALUE::from(frame.cme).into());
    // ep[-1]: specval
    asm.store(Opnd::mem(64, SP, (ep_offset - 1) * SIZEOF_VALUE_I32), frame.specval);
    // ep[0]: ENV_FLAGS
    asm.store(Opnd::mem(64, SP, ep_offset * SIZEOF_VALUE_I32), frame.frame_type.into());

    // Write to the callee CFP
    fn cfp_opnd(offset: i32) -> Opnd {
        Opnd::mem(64, CFP, offset - (RUBY_SIZEOF_CONTROL_FRAME as i32))
    }

    asm_comment!(asm, "push callee control frame");

    if frame.iseq.is_some() {
        // PC, SP, and ISEQ are written lazily by the callee on side-exits, non-leaf calls, or GC.
        // cfp->jit_return will be written by gen_entry_point() on the callee after this frame push.
        if frame.write_block_code {
            asm_comment!(asm, "write block_code for iseq that may use it");
            asm.mov(cfp_opnd(RUBY_OFFSET_CFP_BLOCK_CODE), 0.into());
        }
    } else {
        // C frames don't have a PC and ISEQ in normal operation. ISEQ frames set PC on gen_write_jit_frame().
        // When runtime checks are enabled we poison the PC for C frames so accidental reads stand out.
        if let (None, Some(pc)) = (frame.iseq, PC_POISON) {
            asm.mov(Opnd::mem(64, CFP, RUBY_OFFSET_CFP_PC), Opnd::const_ptr(pc));
        }
        let new_sp = asm.lea(Opnd::mem(64, SP, (ep_offset + 1) * SIZEOF_VALUE_I32));
        asm.mov(cfp_opnd(RUBY_OFFSET_CFP_SP), new_sp);
        // block_code must be written explicitly because the interpreter reads
        // captured->code.ifunc directly from cfp->block_code (not through JITFrame).
        // Without this, stale data from a previous frame occupying this CFP slot
        // can be used as an ifunc pointer, causing a segfault.
        asm.mov(cfp_opnd(RUBY_OFFSET_CFP_BLOCK_CODE), 0.into());
        // C frames share a single static JITFrame (rb_zjit_c_frame). Setting
        // cfp->jit_return to the ZJIT_JIT_RETURN_C_FRAME sentinel tells
        // CFP_ZJIT_FRAME() to use that shared frame, so we don't need to
        // allocate a per-call JITFrame for C method pushes.
        asm.mov(cfp_opnd(RUBY_OFFSET_CFP_JIT_RETURN), (ZJIT_JIT_RETURN_C_FRAME as usize).into());
    }

    asm.mov(cfp_opnd(RUBY_OFFSET_CFP_SELF), frame.recv);
    let ep = asm.lea(Opnd::mem(64, SP, ep_offset * SIZEOF_VALUE_I32));
    asm.mov(cfp_opnd(RUBY_OFFSET_CFP_EP), ep);
}

/// Stack overflow check: fails if CFP<=SP at any point in the callee.
pub(super) fn gen_stack_overflow_check(jit: &mut JITState, asm: &mut Assembler, function: &Function, state: &FrameState, stack_growth: usize) {
    asm_comment!(asm, "stack overflow check");
    // vm_push_frame() checks it against a decremented cfp, and CHECK_VM_STACK_OVERFLOW0
    // adds to the margin another control frame with `&bounds[1]`.
    const { assert!(RUBY_SIZEOF_CONTROL_FRAME % SIZEOF_VALUE == 0, "sizeof(rb_control_frame_t) is a multiple of sizeof(VALUE)"); }
    let cfp_growth = 2 * (RUBY_SIZEOF_CONTROL_FRAME / SIZEOF_VALUE);
    let peak_offset = (cfp_growth + stack_growth) * SIZEOF_VALUE;
    let stack_limit = asm.lea(Opnd::mem(64, SP, peak_offset as i32));
    asm.cmp(CFP, stack_limit);
    asm.jbe(jit, side_exit(jit, function, state, StackOverflow));
}
