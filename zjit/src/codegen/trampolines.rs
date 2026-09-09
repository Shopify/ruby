//! Process-wide entry, exit, and function-stub trampolines.

use crate::asm::CodeBlock;
use crate::backend::current::ALLOC_REGS;
use crate::backend::lir::{self, asm_ccall, asm_comment, Assembler, C_ARG_OPNDS, CFP, EC, Opnd, SP, Target};
use crate::cruby::{CfpPtr, EcPtr, Qundef, VALUE_BITS, RUBY_OFFSET_CFP_BLOCK_CODE, RUBY_OFFSET_CFP_JIT_RETURN, RUBY_OFFSET_CFP_SP};
use crate::stats::{CompileError, Counter::exit_compile_error};
use crate::virtualmem::CodePtr;
use super::{gen_incr_counter};
use super::function_stub_hit;
use crate::perf;

/// Compile a shared JIT entry trampoline.
pub(crate) fn gen_entry_trampoline(cb: &mut CodeBlock) -> Result<CodePtr, CompileError> {
    // Set up registers for CFP, EC, SP, and basic block arguments
    let mut asm = Assembler::new();
    asm.new_block_without_id("gen_entry_trampoline");
    gen_entry_prologue(&mut asm);

    // Jump to the first block using a call instruction. This trampoline is used
    // as rb_zjit_func_t in jit_exec(), which takes (EC, CFP, rb_jit_func_t).
    // So C_ARG_OPNDS[2] is rb_zjit_func_t, which is (EC, CFP) -> VALUE.
    let out = asm.ccall_reg(C_ARG_OPNDS[2], VALUE_BITS);

    // Restore registers for CFP, EC, and SP after use
    asm_comment!(asm, "return to the interpreter");
    asm.frame_teardown(lir::JIT_PRESERVED_REGS);
    asm.cret(out);

    let (code_ptr, gc_offsets) = asm.compile(cb)?;
    assert!(gc_offsets.is_empty());
    perf::register_current_code_range(cb, "entry trampoline", code_ptr);
    Ok(code_ptr)
}

/// Compile an interpreter entry block to be inserted into an ISEQ.
fn gen_entry_prologue(asm: &mut Assembler) {
    asm_comment!(asm, "ZJIT entry trampoline");
    // Save the registers we'll use for CFP, EP, SP
    asm.frame_setup(lir::JIT_PRESERVED_REGS);

    // EC and CFP are passed as arguments
    asm.mov(EC, C_ARG_OPNDS[0]);
    asm.mov(CFP, C_ARG_OPNDS[1]);

    // Load the current SP from the CFP into REG_SP
    asm.mov(SP, Opnd::mem(64, CFP, RUBY_OFFSET_CFP_SP));
}

/// Generate a trampoline that is used when a function stub is called.
/// See [super::gen_function_stub] for how it is used.
pub(crate) fn gen_function_stub_hit_trampoline(cb: &mut CodeBlock) -> Result<CodePtr, CompileError> {
    let (mut asm, scratch_reg) = Assembler::new_with_scratch_reg();
    asm.new_block_without_id("function_stub_hit_trampoline");
    asm_comment!(asm, "function_stub_hit trampoline");

    asm.cpop_into(scratch_reg);

    // Maintain alignment for x86_64, and set up a frame for arm64 properly
    asm.frame_setup(&[]);

    asm_comment!(asm, "preserve argument registers");

    for pair in ALLOC_REGS.chunks(2) {
        match *pair {
            [reg0, reg1] => {
                asm.cpush_pair(Opnd::Reg(reg0), Opnd::Reg(reg1));
            }
            [reg] => {
                asm.cpush(Opnd::Reg(reg));
            }
            _ => unreachable!("chunks(2)")
        }
    }
    if cfg!(target_arch = "x86_64") && ALLOC_REGS.len() % 2 == 1 {
        asm.cpush(Opnd::Reg(ALLOC_REGS[0])); // maintain alignment for x86_64
    }

    // We can't directly pass the scratch register in to the ccall because
    // we're going to have parallel move automatically handle coping registers
    // in to the C calling convention and the parallel move algorithm needs
    // a scratch register to break any cycles.  If we use the scratch register
    // as a C call parameter, then parallel move wouldn't be able to break
    // cycles without clobbering something
    asm.mov(C_ARG_OPNDS[0], scratch_reg);
    // Compile the stubbed ISEQ
    let jump_addr = asm_ccall!(asm, function_stub_hit, C_ARG_OPNDS[0], CFP, SP, EC);
    asm.mov(scratch_reg, jump_addr);

    asm_comment!(asm, "restore argument registers");
    if cfg!(target_arch = "x86_64") && ALLOC_REGS.len() % 2 == 1 {
        asm.cpop_into(Opnd::Reg(ALLOC_REGS[0]));
    }

    for pair in ALLOC_REGS.chunks(2).rev() {
        match *pair {
            [reg] => {
                asm.cpop_into(Opnd::Reg(reg));
            }
            [reg0, reg1] => {
                asm.cpop_pair_into(Opnd::Reg(reg1), Opnd::Reg(reg0));
            }
            _ => unreachable!("chunks(2)")
        }
    }

    // Discard the current frame since the JIT function will set it up again
    asm.frame_teardown(&[]);

    // Jump to scratch_reg so that cpop_into() doesn't clobber it
    asm.jmp_opnd(scratch_reg);

    asm.compile(cb).map(|(code_ptr, gc_offsets)| {
        assert_eq!(gc_offsets.len(), 0);
        perf::register_current_code_range(cb, "function_stub_hit trampoline", code_ptr);
        code_ptr
    })
}

/// Generate a trampoline that is used when a function exits without restoring PC and the stack.
pub(crate) fn gen_exit_trampoline(cb: &mut CodeBlock) -> Result<CodePtr, CompileError> {
    let mut asm = Assembler::new();
    asm.new_block_without_id("exit_trampoline");

    asm_comment!(asm, "side-exit trampoline");
    asm.frame_teardown(&[]); // matching the setup in gen_entry_point()
    asm.cret(Qundef.into());

    asm.compile(cb).map(|(code_ptr, gc_offsets)| {
        assert_eq!(gc_offsets.len(), 0);
        perf::register_current_code_range(cb, "exit trampoline", code_ptr);
        code_ptr
    })
}

/// Generate a trampoline that materializes ZJIT frames before unwinding native frames.
pub(crate) fn gen_materialize_exit_trampoline(cb: &mut CodeBlock, exit_trampoline: CodePtr) -> Result<CodePtr, CompileError> {
    unsafe extern "C" {
        fn rb_zjit_materialize_frames(ec: EcPtr, cfp: CfpPtr);
    }

    let mut asm = Assembler::new();
    asm.new_block_without_id("materialize_exit_trampoline");

    asm_comment!(asm, "clear JITFrame materialized by exit code");
    asm.store(Opnd::mem(64, CFP, RUBY_OFFSET_CFP_JIT_RETURN), 0.into());
    // Clear cfp->block_code since it may have been left uninitialized by JITFrame mechanisms.
    // Zero is the right value because we're dealing with the top most frame.
    // Non-zero values are only set before pushing a frame.
    asm.store(Opnd::mem(64, CFP, RUBY_OFFSET_CFP_BLOCK_CODE), 0.into());

    asm_comment!(asm, "materialize ZJIT frames");
    asm_ccall!(asm, rb_zjit_materialize_frames, EC, CFP);
    asm.jmp(Target::CodePtr(exit_trampoline));

    asm.compile(cb).map(|(code_ptr, gc_offsets)| {
        assert_eq!(gc_offsets.len(), 0);
        perf::register_current_code_range(cb, "materialize_exit trampoline", code_ptr);
        code_ptr
    })
}

/// Generate a trampoline that increments exit_compilation_failure and jumps to materialize_exit_trampoline.
pub(crate) fn gen_materialize_exit_trampoline_with_counter(cb: &mut CodeBlock, materialize_exit_trampoline: CodePtr) -> Result<CodePtr, CompileError> {
    let mut asm = Assembler::new();
    asm.new_block_without_id("materialize_exit_trampoline_with_counter");

    asm_comment!(asm, "function stub exit trampoline");
    gen_incr_counter(&mut asm, exit_compile_error);
    asm.jmp(Target::CodePtr(materialize_exit_trampoline));

    asm.compile(cb).map(|(code_ptr, gc_offsets)| {
        assert_eq!(gc_offsets.len(), 0);
        perf::register_current_code_range(cb, "materialize_exit_with_counter trampoline", code_ptr);
        code_ptr
    })
}
