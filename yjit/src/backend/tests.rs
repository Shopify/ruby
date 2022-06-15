#![cfg(test)]

use crate::asm::{CodeBlock};
use crate::virtualmem::{CodePtr};
use crate::backend::ir::*;
use crate::cruby::*;
use crate::core::*;
use InsnOpnd::*;

// Test that this function type checks
fn gen_dup(
    ctx: &mut Context,
    asm: &mut Assembler,
) {
    let dup_val = ctx.ir_stack_pop(0);
    let (mapping, tmp_type) = ctx.get_opnd_mapping(StackOpnd(0));

    let loc0 = ctx.ir_stack_push_mapping((mapping, tmp_type));
    asm.mov(loc0, dup_val);
}

fn guard_object_is_heap(
    asm: &mut Assembler,
    object_opnd: Opnd,
    ctx: &mut Context,
    side_exit: CodePtr,
) {
    asm.comment("guard object is heap");

    // Test that the object is not an immediate
    asm.test(object_opnd.clone(), Opnd::UImm(RUBY_IMMEDIATE_MASK as u64));
    asm.jnz(Target::CodePtr(side_exit));

    // Test that the object is not false or nil
    asm.cmp(object_opnd.clone(), Opnd::UImm(Qnil.into()));
    asm.jbe(Target::CodePtr(side_exit));
}

#[test]
fn test_add() {
    let mut asm = Assembler::new();
    let out = asm.add(SP, Opnd::UImm(1));
    asm.add(out, Opnd::UImm(2));
}

#[test]
fn test_split_loads() {
    let mut asm = Assembler::new();

    let regs = Assembler::get_alloc_regs();

    asm.add(
        Opnd::mem(64, Opnd::Reg(regs[0]), 0),
        Opnd::mem(64, Opnd::Reg(regs[1]), 0)
    );

    let result = asm.split_loads();
    assert_eq!(result.insns.len(), 2);
    assert_eq!(result.insns[0].op, Op::Load);
}

#[test]
fn test_alloc_regs() {
    let mut asm = Assembler::new();

    // Get the first output that we're going to reuse later.
    let out1 = asm.add(EC, Opnd::UImm(1));

    // Pad some instructions in to make sure it can handle that.
    asm.add(EC, Opnd::UImm(2));

    // Get the second output we're going to reuse.
    let out2 = asm.add(EC, Opnd::UImm(3));

    // Pad another instruction.
    asm.add(EC, Opnd::UImm(4));

    // Reuse both the previously captured outputs.
    asm.add(out1, out2);

    // Now get a third output to make sure that the pool has registers to
    // allocate now that the previous ones have been returned.
    let out3 = asm.add(EC, Opnd::UImm(5));
    asm.add(out3, Opnd::UImm(6));

    // Here we're going to allocate the registers.
    let result = asm.alloc_regs(Assembler::get_alloc_regs());

    // Now we're going to verify that the out field has been appropriately
    // updated for each of the instructions that needs it.
    let regs = Assembler::get_alloc_regs();
    assert_eq!(result.insns[0].out, Opnd::Reg(regs[0]));
    assert_eq!(result.insns[2].out, Opnd::Reg(regs[1]));
    assert_eq!(result.insns[5].out, Opnd::Reg(regs[0]));
}

fn setup_asm() -> (Assembler, CodeBlock) {
    return (
        Assembler::new(),
        CodeBlock::new_dummy(1024)
    );
}

// Test full codegen pipeline
#[test]
fn test_compile()
{
    let (mut asm, mut cb) = setup_asm();
    let regs = Assembler::get_alloc_regs();

    let out = asm.add(Opnd::Reg(regs[0]), Opnd::UImm(2));
    asm.add(out, Opnd::UImm(2));

    asm.compile_with_num_regs(&mut cb, 1);
}

// Test memory-to-memory move
#[test]
fn test_mov_mem2mem()
{
    let (mut asm, mut cb) = setup_asm();

    asm.comment("check that comments work too");
    asm.mov(Opnd::mem(64, SP, 0), Opnd::mem(64, SP, 8));

    asm.compile_with_num_regs(&mut cb, 1);
}

// Test load of register into new register
#[test]
fn test_load_reg()
{
    let (mut asm, mut cb) = setup_asm();

    let out = asm.load(SP);
    asm.mov(Opnd::mem(64, SP, 0), out);

    asm.compile_with_num_regs(&mut cb, 1);
}

// Multiple registers needed and register reuse
#[test]
fn test_reuse_reg()
{
    let (mut asm, mut cb) = setup_asm();

    let v0 = asm.add(Opnd::mem(64, SP, 0), Opnd::UImm(1));
    let v1 = asm.add(Opnd::mem(64, SP, 8), Opnd::UImm(1));
    let v2 = asm.add(v0, Opnd::UImm(1));
    asm.add(v0, v2);

    asm.compile_with_num_regs(&mut cb, 2);
}

// 64-bit values can't be written directly to memory,
// need to be split into one or more register movs first
#[test]
fn test_store_u64()
{
    let (mut asm, mut cb) = setup_asm();
    asm.store(Opnd::mem(64, SP, 0), u64::MAX.into());

    asm.compile_with_num_regs(&mut cb, 1);
}

// Use instruction output as base register for memory operand
#[test]
fn test_base_insn_out()
{
    let (mut asm, mut cb) = setup_asm();

    // Load the pointer into a register
    let ptr_reg = asm.load(Opnd::const_ptr(0 as *const u8));
    let counter_opnd = Opnd::mem(64, ptr_reg, 0);

    // Increment and store the updated value
    asm.incr_counter(counter_opnd, 1.into() );

    asm.compile_with_num_regs(&mut cb, 1);
}

#[test]
fn test_c_call()
{
    extern "sysv64" fn dummy_c_fun(v0: usize, v1: usize)
    {
    }

    let (mut asm, mut cb) = setup_asm();

    asm.ccall(
        dummy_c_fun as *const u8,
        vec![Opnd::mem(64, SP, 0), Opnd::UImm(1)]
    );

    asm.compile_with_num_regs(&mut cb, 2);
}

#[test]
fn test_lea_ret()
{
    let (mut asm, mut cb) = setup_asm();

    let addr = asm.lea(Opnd::mem(64, SP, 0));
    asm.cret(addr);

    asm.compile_with_num_regs(&mut cb, 1);
}

#[test]
fn test_jcc_label()
{
    let (mut asm, mut cb) = setup_asm();

    let label = asm.new_label("foo");
    asm.cmp(EC, EC);
    asm.je(label);
    asm.write_label(label);

    asm.compile_with_num_regs(&mut cb, 1);
}
