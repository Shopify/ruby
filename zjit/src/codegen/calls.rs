//! Call lowering.

use super::*;

pub(super) fn gen_invokebuiltin(jit: &JITState, asm: &mut Assembler, function: &Function, state: &FrameState, bf: &rb_builtin_function, leaf: bool, args: Vec<Opnd>) -> lir::Opnd {
    if leaf {
        gen_prepare_leaf_call_with_gc(asm, state);
    } else {
        // Anything can happen inside builtin functions
        gen_prepare_non_leaf_call(jit, asm, function, state);
    }

    let mut cargs = vec![EC];
    cargs.extend(args);

    asm.count_call_to(unsafe { std::ffi::CStr::from_ptr(bf.name).to_str().unwrap() });
    asm.ccall(bf.func_ptr as *const u8, cargs)
}

/// Generate code for a C function call that pushes a frame
pub(super) fn gen_ccall_with_frame(
    jit: &mut JITState,
    asm: &mut Assembler,
    function: &Function,
    cfunc: *const u8,
    name: ID,
    recv: Opnd,
    args: Vec<Opnd>,
    cme: *const rb_callable_method_entry_t,
    block: Option<BlockHandler>,
    state: &FrameState,
) -> lir::Opnd {
    gen_incr_counter(asm, Counter::non_variadic_cfunc_optimized_send_count);
    gen_stack_overflow_check(jit, asm, function, state, state.stack_size());

    let args_with_recv_len = args.len() + 1;
    let caller_stack_size = state.stack().len() - args_with_recv_len;

    // Can't use gen_prepare_non_leaf_call() because we need to adjust the SP
    // to account for the receiver and arguments (and block arguments if any)
    gen_write_jit_frame(asm, state, 0);
    gen_save_sp(asm, caller_stack_size);
    gen_spill_stack(jit, asm, function, state);
    gen_spill_locals(jit, asm, state);

    let block_handler_specval = if let Some(BlockHandler::BlockIseq(block_iseq)) = block {
        // Change cfp->block_code in the current frame. See vm_caller_setup_arg_block().
        // VM_CFP_TO_CAPTURED_BLOCK then turns &cfp->self into a block handler.
        // rb_captured_block->code.iseq aliases with cfp->block_code.
        asm.store(Opnd::mem(64, CFP, RUBY_OFFSET_CFP_BLOCK_CODE), VALUE::from(block_iseq).into());
        let cfp_self_addr = asm.lea(Opnd::mem(64, CFP, RUBY_OFFSET_CFP_SELF));
        asm.or(cfp_self_addr, Opnd::Imm(1))
    } else {
        VM_BLOCK_HANDLER_NONE.into()
    };

    gen_push_frame(asm, args_with_recv_len, state, ControlFrame {
        recv,
        iseq: None,
        cme,
        frame_type: VM_FRAME_MAGIC_CFUNC | VM_FRAME_FLAG_CFRAME | VM_ENV_FLAG_LOCAL,
        specval: block_handler_specval,
        write_block_code: false,
    });

    asm_comment!(asm, "switch to new SP register");
    let sp_offset = (caller_stack_size + VM_ENV_DATA_SIZE.to_usize()) * SIZEOF_VALUE;
    let new_sp = asm.add(SP, sp_offset.into());
    asm.mov(SP, new_sp);

    asm_comment!(asm, "switch to new CFP");
    let new_cfp = asm.sub(CFP, RUBY_SIZEOF_CONTROL_FRAME.into());
    asm.mov(CFP, new_cfp);
    asm.store(Opnd::mem(64, EC, RUBY_OFFSET_EC_CFP), CFP);

    let mut cfunc_args = vec![recv];
    cfunc_args.extend(args);
    asm.count_call_to_with(|| qualified_method_name(unsafe { (*cme).owner }, name));
    let result = asm.ccall(cfunc, cfunc_args);

    asm_comment!(asm, "pop C frame");
    let new_cfp = asm.add(CFP, RUBY_SIZEOF_CONTROL_FRAME.into());
    asm.mov(CFP, new_cfp);
    asm.store(Opnd::mem(64, EC, RUBY_OFFSET_EC_CFP), CFP);

    asm_comment!(asm, "restore SP register for the caller");
    let new_sp = asm.sub(SP, sp_offset.into());
    asm.mov(SP, new_sp);

    result
}

/// Lowering for [`Insn::CCall`]. This is a low-level raw call that doesn't know
/// anything about the callee, so handling for e.g. GC safety is dealt with elsewhere.
pub(super) fn gen_ccall(asm: &mut Assembler, cfunc: *const u8, name: ID, owner: VALUE, recv: Opnd, args: Vec<Opnd>) -> lir::Opnd {
    let mut cfunc_args = vec![recv];
    cfunc_args.extend(args);
    asm.count_call_to_with(|| if owner == Qnil { name.contents_lossy().to_string() } else { qualified_method_name(owner, name) });
    asm.ccall(cfunc, cfunc_args)
}

// Change cfp->block_code in the current frame. See vm_caller_setup_arg_block().
// VM_CFP_TO_CAPTURED_BLOCK then turns &cfp->self into a block handler.
// rb_captured_block->code.iseq aliases with cfp->block_code.
fn gen_block_handler_specval(asm: &mut Assembler, blockiseq: IseqPtr) -> lir::Opnd {
    asm.store(Opnd::mem(VALUE_BITS, CFP, RUBY_OFFSET_CFP_BLOCK_CODE), VALUE::from(blockiseq).into());
    let cfp_self_addr = asm.lea(Opnd::mem(VALUE_BITS, CFP, RUBY_OFFSET_CFP_SELF));
    asm.or(cfp_self_addr, Opnd::Imm(1))
}

/// Generate code for a variadic C function call
/// func(int argc, VALUE *argv, VALUE recv)
pub(super) fn gen_ccall_variadic(
    jit: &mut JITState,
    asm: &mut Assembler,
    function: &Function,
    cfunc: *const u8,
    name: ID,
    recv: Opnd,
    args: Vec<Opnd>,
    cme: *const rb_callable_method_entry_t,
    block: Option<BlockHandler>,
    state: &FrameState,
) -> lir::Opnd {
    gen_incr_counter(asm, Counter::variadic_cfunc_optimized_send_count);
    gen_stack_overflow_check(jit, asm, function, state, state.stack_size());

    let args_with_recv_len = args.len() + 1;

    // Compute the caller's stack size after consuming recv and args.
    // state.stack() includes recv + args, so subtract both.
    let caller_stack_size = state.stack_size() - args_with_recv_len;

    // Can't use gen_prepare_non_leaf_call() because we need to adjust the SP
    // to account for the receiver and arguments (like gen_ccall_with_frame does)
    gen_write_jit_frame(asm, state, 0);
    gen_save_sp(asm, caller_stack_size);
    gen_spill_stack(jit, asm, function, state);
    gen_spill_locals(jit, asm, state);

    let block_handler_specval = if let Some(BlockHandler::BlockIseq(blockiseq)) = block {
        gen_block_handler_specval(asm, blockiseq)
    } else {
        VM_BLOCK_HANDLER_NONE.into()
    };

    gen_push_frame(asm, args_with_recv_len, state, ControlFrame {
        recv,
        iseq: None,
        cme,
        frame_type: VM_FRAME_MAGIC_CFUNC | VM_FRAME_FLAG_CFRAME | VM_ENV_FLAG_LOCAL,
        specval: block_handler_specval,
        write_block_code: false,
    });

    asm_comment!(asm, "switch to new SP register");
    let sp_offset = (caller_stack_size + VM_ENV_DATA_SIZE.to_usize()) * SIZEOF_VALUE;
    let new_sp = asm.add(SP, sp_offset.into());
    asm.mov(SP, new_sp);

    asm_comment!(asm, "switch to new CFP");
    let new_cfp = asm.sub(CFP, RUBY_SIZEOF_CONTROL_FRAME.into());
    asm.mov(CFP, new_cfp);
    asm.store(Opnd::mem(64, EC, RUBY_OFFSET_EC_CFP), CFP);

    let argv_ptr = gen_push_opnds(jit, asm, &args);
    asm.count_call_to_with(|| qualified_method_name(unsafe { (*cme).owner }, name));
    let result = asm.ccall(cfunc, vec![args.len().into(), argv_ptr, recv]);

    asm_comment!(asm, "pop C frame");
    let new_cfp = asm.add(CFP, RUBY_SIZEOF_CONTROL_FRAME.into());
    asm.mov(CFP, new_cfp);
    asm.store(Opnd::mem(64, EC, RUBY_OFFSET_EC_CFP), CFP);

    asm_comment!(asm, "restore SP register for the caller");
    let new_sp = asm.sub(SP, sp_offset.into());
    asm.mov(SP, new_sp);

    result
}

pub(super) fn gen_trace_fallback(asm: &mut Assembler, reason: &str) {
    if !get_option!(trace_fallbacks) {
        return;
    }
    let reason_cstr = std::ffi::CString::new(reason.to_string())
        .unwrap_or_else(|_| std::ffi::CString::new("unknown").unwrap());
    let reason_ptr = reason_cstr.into_raw() as *const u8;
    use crate::state::rb_zjit_record_fallback_stack;
    asm_ccall!(asm, rb_zjit_record_fallback_stack, Opnd::const_ptr(reason_ptr));
}

fn gen_trace_send_fallback(asm: &mut Assembler, reason: &SendFallbackReason) {
    if !get_option!(trace_fallbacks) {
        return;
    }
    gen_trace_fallback(asm, &format!("{reason}"));
}

/// Compile a dynamic dispatch with block
pub(super) fn gen_send(
    jit: &mut JITState,
    asm: &mut Assembler,
    function: &Function,
    cd: *const rb_call_data,
    blockiseq: IseqPtr,
    state: &FrameState,
    reason: SendFallbackReason,
) -> lir::Opnd {
    gen_incr_send_fallback_counter(asm, reason);
    gen_trace_send_fallback(asm, &reason);

    gen_prepare_fallback_call(jit, asm, function, state);
    asm_comment!(asm, "call #{} with dynamic dispatch", ruby_call_method_name(cd));
    unsafe extern "C" {
        fn rb_vm_send(ec: EcPtr, cfp: CfpPtr, cd: VALUE, blockiseq: IseqPtr) -> VALUE;
    }
    asm_ccall!(
        asm,
        rb_vm_send,
        EC, CFP, Opnd::const_ptr(cd), VALUE::from(blockiseq).into()
    )
}

/// Compile a dynamic dispatch with `...`
pub(super) fn gen_send_forward(
    jit: &mut JITState,
    asm: &mut Assembler,
    function: &Function,
    cd: *const rb_call_data,
    blockiseq: IseqPtr,
    state: &FrameState,
    reason: SendFallbackReason,
) -> lir::Opnd {
    gen_incr_send_fallback_counter(asm, reason);
    gen_trace_send_fallback(asm, &reason);

    gen_prepare_fallback_call(jit, asm, function, state);

    asm_comment!(asm, "call #{} with dynamic dispatch", ruby_call_method_name(cd));
    unsafe extern "C" {
        fn rb_vm_sendforward(ec: EcPtr, cfp: CfpPtr, cd: VALUE, blockiseq: IseqPtr) -> VALUE;
    }
    asm_ccall!(
        asm,
        rb_vm_sendforward,
        EC, CFP, Opnd::const_ptr(cd), VALUE::from(blockiseq).into()
    )
}

/// Compile a dynamic dispatch without block
pub(super) fn gen_send_without_block(
    jit: &mut JITState,
    asm: &mut Assembler,
    function: &Function,
    cd: *const rb_call_data,
    state: &FrameState,
    reason: SendFallbackReason,
) -> lir::Opnd {
    gen_incr_send_fallback_counter(asm, reason);
    gen_trace_send_fallback(asm, &reason);

    gen_prepare_fallback_call(jit, asm, function, state);
    asm_comment!(asm, "call #{} with dynamic dispatch", ruby_call_method_name(cd));
    unsafe extern "C" {
        fn rb_vm_opt_send_without_block(ec: EcPtr, cfp: CfpPtr, cd: VALUE) -> VALUE;
    }
    asm_ccall!(
        asm,
        rb_vm_opt_send_without_block,
        EC, CFP, Opnd::const_ptr(cd)
    )
}

/// Push an interpreter frame for an inlined callee. This is the same as the frame push
/// portion of gen_send_iseq_direct, but without the native call to the callee. Control
/// falls through to the next instruction (the inlined callee body).
pub(super) fn gen_push_inline_frame(
    jit: &mut JITState,
    asm: &mut Assembler,
    function: &Function,
    cme: *const rb_callable_method_entry_t,
    iseq: IseqPtr,
    recv: Opnd,
    num_args: u16,
    state: &FrameState,
    blockiseq: Option<IseqPtr>,
) {
    let local_size = unsafe { get_iseq_body_local_table_size(iseq) }.to_usize();
    let stack_growth = state.stack_size() + local_size + unsafe { get_iseq_body_stack_max(iseq) }.to_usize();
    gen_stack_overflow_check(jit, asm, function, state, stack_growth);

    // Save cfp->pc and cfp->sp for the caller frame.
    // Cannot use gen_prepare_non_leaf_call because we need special SP math.
    let stack_size = state.stack().len() - num_args.to_usize() - 1; // -1 for receiver
    gen_write_jit_frame(asm, state, 0);
    gen_save_sp(asm, stack_size);

    gen_spill_locals(jit, asm, state);

    // This mirrors vm_caller_setup_arg_block() for the `blockiseq != NULL` case.
    // The HIR specialization guards ensure we will only reach here for literal blocks,
    // not &block forwarding, &:foo, etc. These are rejected in `type_specialize` by
    // `unspecializable_call_type`.
    let block_handler = blockiseq.map(|b| gen_block_handler_specval(asm, b));

    let callee_is_bmethod = VM_METHOD_TYPE_BMETHOD == unsafe { get_cme_def_type(cme) };

    let (frame_type, specval) = if callee_is_bmethod {
        // Extract EP from the Proc instance
        let procv = unsafe { rb_get_def_bmethod_proc((*cme).def) };
        let proc = unsafe { rb_jit_get_proc_ptr(procv) };
        let proc_block = unsafe { (*proc).block.as_ref() };
        let capture = unsafe { proc_block.as_.captured.as_ref() };
        let bmethod_frame_type = VM_FRAME_MAGIC_BLOCK | VM_FRAME_FLAG_BMETHOD | VM_FRAME_FLAG_LAMBDA;
        // Tag the captured EP like VM_GUARDED_PREV_EP() in vm_call_iseq_bmethod()
        let bmethod_specval = (capture.ep.addr() | 1).into();
        (bmethod_frame_type, bmethod_specval)
    } else {
        let specval = block_handler.unwrap_or_else(|| VM_BLOCK_HANDLER_NONE.into());
        (VM_FRAME_MAGIC_METHOD | VM_ENV_FLAG_LOCAL, specval)
    };

    gen_push_frame(asm, num_args.to_usize(), state, ControlFrame {
        recv,
        iseq: Some(iseq),
        cme,
        frame_type,
        specval,
        write_block_code: iseq_may_write_block_code(iseq),
    });

    // Publish the inlined callee's entry JITFrame before the inlined body runs.
    // Frame walking functions such as rb_profile_frames can inspect the new
    // CFP between this frame push and the first inlined gen_write_jit_frame, so
    // cfp->jit_return must already reference a valid JITFrame slot. Leaving it
    // stale or uninitialized is unsafe because CFP_ZJIT_FRAME has no independent
    // way to tell whether it points at a valid JITFrame slot.
    //
    // We install a pre-baked JITFrame for the callee's entry by writing its address into
    // the callee's own JITFrame slot and pointing the callee's cfp->jit_return at that
    // slot, matching the protocol established by gen_entry_point + gen_write_jit_frame.
    // The callee runs one level deeper than the caller, so it uses the slot for
    // `state.depth + 1` (state is the caller's FrameState). Giving each inlining depth a
    // distinct slot keeps the caller's and callee's cfp->jit_return from aliasing the same
    // native stack location, which would otherwise make rb_zjit_materialize_frames copy
    // one frame's PC/ISEQ into every aliased CFP on the chain. CFP_ZJIT_FRAME in zjit.h
    // reads the JITFrame via ((VALUE *)cfp->jit_return)[-1], so the field must be the
    // slot's address, not the JITFrame pointer itself. Once the inlined body runs its
    // first gen_write_jit_frame, that call overwrites the same slot with a JITFrame
    // carrying the current PC, just as the non-inlined path does.
    //
    // cfp->sp is left stale at frame push, matching the non-inlined gen_push_frame, which
    // also skips the cfp->sp write for ISEQ frames. The first gen_save_sp call inside the
    // inlined body (reached via gen_prepare_call_with_gc or gen_prepare_leaf_call_with_gc
    // before any GC-triggering operation) installs the correct value. Side-exits write
    // cfp->sp themselves via compile_exit_save_state in lir.rs before returning Qundef.
    fn cfp_opnd(offset: i32) -> Opnd {
        Opnd::mem(64, CFP, offset - (RUBY_SIZEOF_CONTROL_FRAME as i32))
    }
    let callee_depth = state.depth + 1;
    let callee_entry_pc = unsafe { rb_iseq_pc_at_idx(iseq, 0) };
    let callee_entry_frame = JITFrame::new_iseq(callee_entry_pc, iseq, 0);
    asm_comment!(asm, "install entry JITFrame for inlined callee");
    asm.mov(Opnd::mem(64, NATIVE_BASE_PTR, jit_frame_slot_offset(callee_depth)), Opnd::const_ptr(callee_entry_frame));
    let callee_jit_return = cfp_jit_return_for_depth(asm, callee_depth);
    asm.mov(cfp_opnd(RUBY_OFFSET_CFP_JIT_RETURN), callee_jit_return);

    // The callee's hidden `kw_bits` local does not need a runtime store here:
    // the inliner aliases the local to a `Const::Value` carrying the
    // compile-time bitmask, so `checkkeyword` lowers to a constant
    // `FixnumBitCheck` rather than a memory load. On a side exit out of the
    // inlined body, FrameState materialization writes the local back to the
    // callee frame from that constant, and on the no-side-exit path nothing
    // reads the slot before `gen_pop_inline_frame` tears down the frame.
    // (The non-inlined `gen_send_iseq_direct` path still emits its own store
    // because the callee's separate JIT entry reads it from memory.)

    let sp_offset = (state.stack().len() + local_size - num_args.to_usize() + VM_ENV_DATA_SIZE.to_usize()) * SIZEOF_VALUE;
    asm_comment!(asm, "switch to inlined callee SP");
    let new_sp = asm.add(SP, sp_offset.into());
    asm.mov(SP, new_sp);

    asm_comment!(asm, "switch to inlined callee CFP");
    let new_cfp = asm.sub(CFP, RUBY_SIZEOF_CONTROL_FRAME.into());
    asm.mov(CFP, new_cfp);
    asm.store(Opnd::mem(64, EC, RUBY_OFFSET_EC_CFP as i32), CFP);
}

/// Pop the interpreter frame for an inlined callee, restoring the caller's SP and CFP.
pub(super) fn gen_pop_inline_frame(
    asm: &mut Assembler,
    iseq: IseqPtr,
    argc: usize,
    state: &FrameState,
) {
    let local_size = unsafe { get_iseq_body_local_table_size(iseq) }.to_usize();
    let sp_offset = (state.stack().len() + local_size - argc + VM_ENV_DATA_SIZE.to_usize()) * SIZEOF_VALUE;

    asm_comment!(asm, "restore caller SP after inline");
    asm.sub_into(SP, sp_offset.into());

    asm_comment!(asm, "restore caller CFP after inline");
    asm.add_into(CFP, RUBY_SIZEOF_CONTROL_FRAME.into());
    asm.store(Opnd::mem(64, EC, RUBY_OFFSET_EC_CFP as i32), CFP);
}

/// Compile a direct call to an ISEQ method.
/// If `block_handler` is provided, it's used as the specval for the new frame (for forwarding blocks).
/// Otherwise, `VM_BLOCK_HANDLER_NONE` is used.
pub(super) fn gen_send_iseq_direct(
    cb: &mut CodeBlock,
    jit: &mut JITState,
    asm: &mut Assembler,
    function: &Function,
    cme: *const rb_callable_method_entry_t,
    iseq: IseqPtr,
    recv: Opnd,
    args: Vec<Opnd>,
    kw_bits: u32,
    jit_entry_idx: u16,
    state: &FrameState,
    block: Option<BlockHandler>,
) -> lir::Opnd {
    gen_incr_counter(asm, Counter::iseq_optimized_send_count);

    let local_size = unsafe { get_iseq_body_local_table_size(iseq) }.to_usize();
    let stack_growth = state.stack_size() + local_size + unsafe { get_iseq_body_stack_max(iseq) }.to_usize();
    gen_stack_overflow_check(jit, asm, function, state, stack_growth);

    // Save cfp->pc and cfp->sp for the caller frame
    // Can't use gen_prepare_non_leaf_call because we need special SP math.
    let stack_size = state.stack().len() - args.len() - 1; // -1 for receiver
    let stack_map = build_stack_map(jit, function, &state.with_stack_size(stack_size));
    let jit_frame = gen_write_jit_frame(asm, state, stack_map.len());
    gen_save_sp(asm, stack_size);

    gen_spill_locals(jit, asm, state);
    asm.stack_map(stack_map, jit_frame, state.depth);

    // This mirrors vm_caller_setup_arg_block() in for the `blockiseq != NULL` case.
    // The HIR specialization guards ensure we will only reach here for literal blocks,
    // not &block forwarding, &:foo, etc. Thise are rejected in `type_specialize` by
    // `unspecializable_call_type`.
    let block_handler = block.map(|bh| match bh { BlockHandler::BlockIseq(b) => gen_block_handler_specval(asm, b), BlockHandler::BlockArg => unreachable!("BlockArg in gen_send_iseq_direct") });

    let callee_is_bmethod = VM_METHOD_TYPE_BMETHOD == unsafe { get_cme_def_type(cme) };

    let (frame_type, specval) = if callee_is_bmethod {
        // Extract EP from the Proc instance
        let procv = unsafe { rb_get_def_bmethod_proc((*cme).def) };
        let proc = unsafe { rb_jit_get_proc_ptr(procv) };
        let proc_block = unsafe { (*proc).block.as_ref() };
        let capture = unsafe { proc_block.as_.captured.as_ref() };
        let bmethod_frame_type = VM_FRAME_MAGIC_BLOCK | VM_FRAME_FLAG_BMETHOD | VM_FRAME_FLAG_LAMBDA;
        // Tag the captured EP like VM_GUARDED_PREV_EP() in vm_call_iseq_bmethod()
        let bmethod_specval = (capture.ep.addr() | 1).into();
        (bmethod_frame_type, bmethod_specval)
    } else {
        let specval = block_handler.unwrap_or_else(|| VM_BLOCK_HANDLER_NONE.into());
        (VM_FRAME_MAGIC_METHOD | VM_ENV_FLAG_LOCAL, specval)
    };

    // Set up the new frame
    // TODO: Lazily materialize caller frames on side exits or when needed
    gen_push_frame(asm, args.len(), state, ControlFrame {
        recv,
        iseq: Some(iseq),
        cme,
        frame_type,
        specval,
        write_block_code: iseq_may_write_block_code(iseq),
    });

    // Write "keyword_bits" to the callee's frame if the callee accepts keywords.
    // This is a synthetic local/parameter that the callee reads via checkkeyword to determine
    // which optional keyword arguments need their defaults evaluated.
    // We write this to the local table slot at bits_start so that:
    // 1. The interpreter can read it via checkkeyword if we side-exit
    // 2. The JIT entry can read it from the callee frame slot
    if unsafe { rb_get_iseq_flags_has_kw(iseq) } {
        let keyword = unsafe { rb_get_iseq_body_param_keyword(iseq) };
        let bits_start = unsafe { (*keyword).bits_start } as usize;
        let unspecified_bits = VALUE::fixnum_from_usize(kw_bits as usize);
        let bits_offset = (state.stack().len() - args.len() + bits_start) * SIZEOF_VALUE;
        asm_comment!(asm, "write keyword bits to callee frame");
        asm.store(Opnd::mem(64, SP, bits_offset as i32), unspecified_bits.into());
    }

    asm_comment!(asm, "switch to new SP register");
    let sp_offset = (state.stack().len() + local_size - args.len() + VM_ENV_DATA_SIZE.to_usize()) * SIZEOF_VALUE;
    let new_sp = asm.add(SP, sp_offset.into());
    asm.mov(SP, new_sp);

    asm_comment!(asm, "switch to new CFP");
    let new_cfp = asm.sub(CFP, RUBY_SIZEOF_CONTROL_FRAME.into());
    asm.mov(CFP, new_cfp); // will be published at `ec->cfp` after callee's entrypoint

    let params = unsafe { iseq.params() };

    // For &block, the JIT entrypoint expects the block_handler as an argument
    // This HIR param is not actually used, things read from specval from the VM frame today.
    // TODO: Remove unused param from HIR, or pass specval through c_args.
    // See https://github.com/ruby/ruby/pull/15911#discussion_r2710544982
    let needs_block = params.flags.has_block() != 0;

    // Set up arguments
    let mut c_args = Vec::with_capacity({
        // This is a heuristic to avoid re-allocation, not necessary for correctness
        1 /* recv */ + args.len() + if needs_block { 1 } else { 0 }
    });
    c_args.push(recv);
    c_args.extend(&args);
    if needs_block {
        if callee_is_bmethod {
            // For bmethods, specval is the captured EP, not the block handler.
            // The block param needs nil (no block) or a Proc value.
            assert!(block_handler.is_none(), "at the moment, HIR builder never emits a direct send for a to-bmethod send-with-literal-block");
            c_args.push(Qnil.into());
        } else {
            c_args.push(specval);
        }
    }

    // Make a method call. The target address will be rewritten once compiled.
    let iseq_call = IseqCall::new(iseq, jit_entry_idx, args.len().try_into().expect("checked in HIR"));
    let dummy_ptr = cb.get_write_ptr().raw_ptr(cb);
    jit.iseq_calls.push(iseq_call.clone());
    let ret = asm.ccall_with_iseq_call(dummy_ptr, c_args, &iseq_call);

    // If a callee side-exits, i.e. returns Qundef, propagate the return value to the caller.
    // The caller will side-exit the callee into the interpreter.
    // TODO: Let side exit code pop all JIT frames to optimize away this cmp + je.
    asm_comment!(asm, "side-exit if callee side-exits");
    asm.cmp(ret, Qundef.into());
    // Restore the C stack pointer on exit
    asm.je(jit, ZJITState::get_exit_trampoline().into());

    asm_comment!(asm, "restore SP register for the caller");
    let new_sp = asm.sub(SP, sp_offset.into());
    asm.mov(SP, new_sp);

    ret
}

/// Compile for invokeblock
pub(super) fn gen_invokeblock(
    jit: &mut JITState,
    asm: &mut Assembler,
    function: &Function,
    cd: *const rb_call_data,
    state: &FrameState,
    reason: SendFallbackReason,
) -> lir::Opnd {
    gen_incr_send_fallback_counter(asm, reason);
    gen_trace_send_fallback(asm, &reason);

    gen_prepare_fallback_call(jit, asm, function, state);

    asm_comment!(asm, "call invokeblock");
    unsafe extern "C" {
        fn rb_vm_invokeblock(ec: EcPtr, cfp: CfpPtr, cd: VALUE) -> VALUE;
    }
    asm_ccall!(
        asm,
        rb_vm_invokeblock,
        EC, CFP, Opnd::const_ptr(cd)
    )
}

/// Compile invokeblock for IFUNC block handlers.
/// Calls rb_vm_yield_with_cfunc directly.
pub(super) fn gen_invokeblock_ifunc(
    jit: &mut JITState,
    asm: &mut Assembler,
    function: &Function,
    cd: *const rb_call_data,
    block_handler: Opnd,
    args: Vec<Opnd>,
    state: &FrameState,
) -> lir::Opnd {
    let _ = cd; // cd is not needed for the direct call

    gen_prepare_fallback_call(jit, asm, function, state);

    // Push args to memory so we can pass argv pointer
    let argv_ptr = gen_push_opnds(jit, asm, &args);

    // Untag the block handler to get the captured block pointer
    // captured = block_handler & ~0x3
    asm_comment!(asm, "untag block handler to get captured block");
    let captured = asm.and(block_handler, Opnd::Imm(!0x3i64));

    asm_comment!(asm, "call rb_vm_yield_with_cfunc");
    unsafe extern "C" {
        fn rb_vm_yield_with_cfunc(
            ec: EcPtr,
            captured: VALUE,
            argc: i32,
            argv: *const VALUE,
        ) -> VALUE;
    }
    asm_ccall!(asm, rb_vm_yield_with_cfunc, EC, captured, (args.len() as i64).into(), argv_ptr)
}

pub(super) fn gen_invokeproc(
    jit: &mut JITState,
    asm: &mut Assembler,
    function: &Function,
    recv: Opnd,
    args: Vec<Opnd>,
    kw_splat: bool,
    state: &FrameState,
) -> lir::Opnd {
    gen_prepare_fallback_call(jit, asm, function, state);

    asm_comment!(asm, "call invokeproc");

    let argv_ptr = gen_push_opnds(jit, asm, &args);
    let kw_splat_opnd = Opnd::Imm(i64::from(kw_splat));
    asm_ccall!(
        asm,
        rb_optimized_call,
        recv,
        EC,
        args.len().into(),
        argv_ptr,
        kw_splat_opnd,
        VM_BLOCK_HANDLER_NONE.into()
    )
}

/// Compile `yield`. Inlines the block ISEQ frame like `invokeblock` instead of calling vm_yield.
/// The block handler is read from the enclosing frame's LEP (`level` hops up), guarded to be the
/// comptime-known ISEQ block, and its frame is pushed here before jumping to the block's JIT entry.
/// On a guard miss, side-exit and recompile. The HIR gate ensures the block is simple + lead-only
/// + non-throwing.
pub(super) fn gen_invoke_block_iseq_direct(
    cb: &mut CodeBlock,
    jit: &mut JITState,
    asm: &mut Assembler,
    function: &Function,
    block_iseq: IseqPtr,
    captured: Opnd,
    args: Vec<Opnd>,
    state: &FrameState,
) -> lir::Opnd {
    gen_incr_counter(asm, Counter::block_iseq_direct_optimized_send_count);

    let local_size = unsafe { get_iseq_body_local_table_size(block_iseq) }.to_usize();
    let stack_growth = state.stack_size() + local_size + unsafe { get_iseq_body_stack_max(block_iseq) }.to_usize();
    gen_stack_overflow_check(jit, asm, function, state, stack_growth);

    // `captured` is the guarded `struct rb_captured_block *` (block handler with the ISEQ tag
    // masked off). The HIR builder loaded it from the LEP and guarded the tag + iseq identity.
    // TODO: During inlining, captured->self can be known. It should be put into HIR.
    let captured_self = asm.load(Opnd::mem(64, captured, 0)); // captured->self
    // TODO: During inlining, captured->ep can sometimes also be known.
    let captured_ep = asm.load(Opnd::mem(64, captured, SIZEOF_VALUE_I32)); // captured->ep
    // specval = VM_GUARDED_PREV_EP(captured->ep) = captured->ep | 0x01
    let specval = asm.or(captured_ep, Opnd::Imm(0x1));

    let stack_size = state.stack().len() - args.len();
    let stack_map = build_stack_map(jit, function, &state.with_stack_size(stack_size));
    let jit_frame = gen_write_jit_frame(asm, state, stack_map.len());
    gen_save_sp(asm, stack_size);

    gen_spill_locals(jit, asm, state);
    asm.stack_map(stack_map, jit_frame, state.depth);

    gen_push_frame(asm, args.len(), state, ControlFrame {
        recv: captured_self,
        iseq: Some(block_iseq),
        cme: std::ptr::null(),
        frame_type: VM_FRAME_MAGIC_BLOCK,
        specval,
        write_block_code: iseq_may_write_block_code(block_iseq),
    });

    asm_comment!(asm, "switch to new SP register");
    let sp_offset = (stack_size + local_size + VM_ENV_DATA_SIZE.to_usize()) * SIZEOF_VALUE;
    let new_sp = asm.add(SP, sp_offset.into());
    asm.mov(SP, new_sp);

    asm_comment!(asm, "switch to new CFP");
    let new_cfp = asm.sub(CFP, RUBY_SIZEOF_CONTROL_FRAME.into());
    asm.mov(CFP, new_cfp);
    asm.store(Opnd::mem(64, EC, RUBY_OFFSET_EC_CFP), CFP);

    // JIT-to-JIT convention: self as c_args[0], then positional args. The block is
    // gated to simple + lead-only + exact arity, so there are no optionals/kw/block.
    let mut c_args = Vec::with_capacity(1 + args.len());
    c_args.push(captured_self);
    c_args.extend(&args);

    let iseq_call = IseqCall::new(block_iseq, 0, args.len().try_into().expect("checked in HIR"));
    let dummy_ptr = cb.get_write_ptr().raw_ptr(cb);
    jit.iseq_calls.push(iseq_call.clone());
    let ret = asm.ccall_with_iseq_call(dummy_ptr, c_args, &iseq_call);

    // If the callee side-exits (returns Qundef), propagate to the caller.
    asm_comment!(asm, "side-exit if callee side-exits");
    asm.cmp(ret, Qundef.into());
    asm.je(jit, ZJITState::get_exit_trampoline().into());

    asm_comment!(asm, "restore SP register for the caller");
    let new_sp = asm.sub(SP, sp_offset.into());
    asm.mov(SP, new_sp);

    ret
}

/// Compile a dynamic dispatch for `super`
pub(super) fn gen_invokesuper(
    jit: &mut JITState,
    asm: &mut Assembler,
    function: &Function,
    cd: *const rb_call_data,
    blockiseq: IseqPtr,
    state: &FrameState,
    reason: SendFallbackReason,
) -> lir::Opnd {
    gen_incr_send_fallback_counter(asm, reason);
    gen_trace_send_fallback(asm, &reason);

    gen_prepare_fallback_call(jit, asm, function, state);
    asm_comment!(asm, "call super with dynamic dispatch");
    unsafe extern "C" {
        fn rb_vm_invokesuper(ec: EcPtr, cfp: CfpPtr, cd: VALUE, blockiseq: IseqPtr) -> VALUE;
    }
    asm_ccall!(
        asm,
        rb_vm_invokesuper,
        EC, CFP, Opnd::const_ptr(cd), VALUE::from(blockiseq).into()
    )
}

/// Compile a dynamic dispatch for `super` with `...`
pub(super) fn gen_invokesuperforward(
    jit: &mut JITState,
    asm: &mut Assembler,
    function: &Function,
    cd: *const rb_call_data,
    blockiseq: IseqPtr,
    state: &FrameState,
    reason: SendFallbackReason,
) -> lir::Opnd {
    gen_incr_send_fallback_counter(asm, reason);
    gen_trace_send_fallback(asm, &reason);

    gen_prepare_fallback_call(jit, asm, function, state);
    asm_comment!(asm, "call super with dynamic dispatch (forwarding)");
    unsafe extern "C" {
        fn rb_vm_invokesuperforward(ec: EcPtr, cfp: CfpPtr, cd: VALUE, blockiseq: IseqPtr) -> VALUE;
    }
    asm_ccall!(
        asm,
        rb_vm_invokesuperforward,
        EC, CFP, Opnd::const_ptr(cd), VALUE::from(blockiseq).into()
    )
}
