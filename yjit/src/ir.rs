#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

use std::convert::From;
use crate::cruby::{VALUE};
use crate::asm::{CodePtr};
use crate::asm::x86_64::{X86Opnd, X86Imm, X86UImm, X86Reg, X86Mem, RegType};
use crate::core::{Context, Type, TempMapping};




/*
// Minimally, we might want to specify how many operands and branch targets an insn has
// Branch targets are not interchangeable with other operand types. We distinguish
// between branch and regular instructions.
//
// TODO: should mark instructions that produce no output operand
//
make_ops! {
    (Comment, 1, 0),
    ...

    // Call is variadic, might need to be special-cased
}
*/









/// Instruction opcodes
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Op
{
    // Add a comment into the IR at the point that this instruction is added. It
    // won't have any impact on that actual compiled code, but it will impact
    // the output of ir_print_insns. Accepts as its only operand an EIR_IMM
    // operand (typically generated by ir_str_ptr).
    Comment,

    // Add a label into the IR at the point that this instruction is added.
    Label,

    // Add two operands together, and return the result as a new operand. This
    // operand can then be used as the operand on another instruction. It
    // accepts two operands, which can be of any type
    //
    // Under the hood when allocating registers, the IR will determine the most
    // efficient way to get these values into memory. For example, if both
    // operands are immediates, then it will load the first one into a register
    // first with a mov instruction and then add them together. If one of them
    // is a register, however, it will just perform a single add instruction.
    Add,

    // This is the same as the OP_ADD instruction, except for subtraction.
    Sub,

    // This is the same as the OP_ADD instruction, except that it performs the
    // binary AND operation.
    And,

    // Perform the NOT operation on an individual operand, and return the result
    // as a new operand. This operand can then be used as the operand on another
    // instruction.
    Not,

    //
    // Low-level instructions
    //

    // A low-level mov instruction. It accepts two operands.
    Mov,

    // Bitwise AND test instruction
    Test,

    // Jump if not zero
    Jnz,

    /*
    // The following are conditional jump instructions. They all accept as their
    // first operand an EIR_LABEL_NAME, which is used as the target of the jump.
    //
    // The OP_JUMP_EQ instruction accepts two additional operands, to be
    // compared for equality. If they're equal, then the generated code jumps to
    // the target label. If they're not, then it continues on to the next
    // instruction.
    JumpEq,

    // The OP_JUMP_NE instruction is very similar to the OP_JUMP_EQ instruction,
    // except it compares for inequality instead.
    JumpNe,

    // Checks the overflow flag and conditionally jumps to the target if it is
    // currently set.
    JumpOvf,

    // A low-level call instruction for calling a function by a pointer. It
    // accepts one operand of type EIR_IMM that should be a pointer to the
    // function. Usually this is done by first casting the function to a void*,
    // as in: ir_const_ptr((void *)&my_function)).
    Call,

    // Calls a function by a pointer and returns an operand that contains the
    // result of the function. Accepts as its operands a pointer to a function
    // of type EIR_IMM (usually generated from ir_const_ptr) and a variable
    // number of arguments to the function being called.
    //
    // This is the higher-level instruction that should be used when you want to
    // call a function with arguments, as opposed to OP_CALL which is
    // lower-level and just calls a function without moving arguments into
    // registers for you.
    CCall,

    // Returns from the function being generated immediately. This is different
    // from OP_RETVAL in that it does nothing with the return value register
    // (whatever is in there is what will get returned). Accepts no operands.
    Ret,

    // First, moves a value into the return value register. Then, returns from
    // the generated function. Accepts as its only operand the value that should
    // be returned from the generated function.
    RetVal,

    // A low-level cmp instruction. It accepts two operands. The first it
    // expects to be a register. The second can be anything. Most of the time
    // this instruction shouldn't be used by the developer since other
    // instructions break down to this one.
    Cmp,

    // A conditional move instruction that should be preceeded at some point by
    // an OP_CMP instruction that would have set the requisite comparison flags.
    // Accepts 2 operands, both of which are expected to be of the EIR_REG type.
    //
    // If the comparison indicates the left compared value is greater than or
    // equal to the right compared value, then the conditional move is executed,
    // otherwise we just continue on to the next instruction.
    //
    // This is considered a low-level instruction, and the OP_SELECT_* variants
    // should be preferred if possible.
    CMovGE,

    // The same as OP_CMOV_GE, except the comparison is greater than.
    CMovGT,

    // The same as OP_CMOV_GE, except the comparison is less than or equal.
    CMovLE,

    // The same as OP_CMOV_GE, except the comparison is less than.
    CMovLT,

    // Selects between two different values based on a comparison of two other
    // values. Accepts 4 operands. The first two are the basis of the
    // comparison. The second two are the "then" case and the "else" case. You
    // can effectively think of this instruction as a ternary operation, where
    // the first two values are being compared.
    //
    // OP_SELECT_GE performs the described ternary using a greater than or equal
    // comparison, that is if the first operand is greater than or equal to the
    // second operand.
    SelectGE,

    // The same as OP_SELECT_GE, except the comparison is greater than.
    SelectGT,

    // The same as OP_SELECT_GE, except the comparison is less than or equal.
    SelectLE,

    // The same as OP_SELECT_GE, except the comparison is less than.
    SelectLT,

    // For later:
    // These encode Ruby true/false semantics
    // Can be used to enable op fusion of Ruby compare + branch.
    // OP_JUMP_TRUE, // (opnd, target)
    // OP_JUMP_FALSE, // (opnd, target)

    // For later:
    // OP_GUARD_HEAP, // (opnd, target)
    // OP_GUARD_IMM, // (opnd, target)
    // OP_GUARD_FIXNUM, // (opnd, target)

    // For later:
    // OP_COUNTER_INC, (counter_name)

    // For later:
    // OP_LEA,
    // OP_TEST,
    */
}










// Register value used by IR operands
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Reg
{
    // Register number/index
    reg_no: u8,

    // Size in bits
    num_bits: u8,

    // Special register flag EC/CFP/SP/SELF
    special: bool,
}

// Memory location
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Mem
{
    // Base register
    base: Reg,

    // Offset relative to the base pointer
    disp: i32,

    // Size in bits
    num_bits: u8,
}

/// Operand to an IR instruction
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Opnd
{
    None,               // For insns with no output

    Stack(u16),         // Value on the temp stack (idx)
    Local(u16),         // Local variable (idx, do we need depth too?)
    Value(VALUE),       // Immediate Ruby value, may be GC'd, movable
    InsnOut(usize),     // Output of a preceding instruction in this block
    String(String),     // String constant, used for comments

    // Low-level operands, for lowering
    Imm(i64),           // Raw signed immediate
    UImm(u64),          // Raw unsigned immediate
    Mem(Mem),           // Memory location (num_bits, base_ptr, const_offset)
    Reg(Reg),           // Machine register (num_bits, idx)
}

impl Opnd
{
    // Convenience constructor for memory operands
    pub fn mem(num_bits: u8, base: Opnd, disp: i32) -> Self {
        match base {
            Opnd::Reg(base_reg) => {
                assert!(base_reg.num_bits == 64 && !base_reg.special);
                Opnd::Mem(Mem {
                    num_bits: num_bits,
                    base: base_reg,
                    disp: disp,
                })
            },
            _ => unreachable!()
        }
    }
}

// Special register constants
pub const EC    : Opnd = Opnd::Reg(Reg { reg_no: 0, num_bits: 64, special: true });
pub const CFP   : Opnd = Opnd::Reg(Reg { reg_no: 1, num_bits: 64, special: true });
pub const SP    : Opnd = Opnd::Reg(Reg { reg_no: 2, num_bits: 64, special: true });
pub const SELF  : Opnd = Opnd::Reg(Reg { reg_no: 3, num_bits: 64, special: true });

/// Method to convert from an X86Opnd to an IR Opnd
impl From<X86Opnd> for Opnd {
    fn from(opnd: X86Opnd) -> Self {
        match opnd {
            X86Opnd::None => Opnd::None,
            X86Opnd::UImm(X86UImm{ value, .. }) => Opnd::UImm(value),
            X86Opnd::Imm(X86Imm{ value, .. }) => Opnd::Imm(value),

            // General-purpose register
            X86Opnd::Reg(X86Reg{ num_bits, reg_no, reg_type: RegType::GP }) => {
                Opnd::Reg(Reg {
                    reg_no,
                    num_bits,
                    special: false,
                })
            }

            // Memory operand with displacement
            X86Opnd::Mem(X86Mem{ num_bits, base_reg_no, disp, idx_reg_no: None, scale_exp: 0 }) => {
                let base_reg = Reg { num_bits: 64, reg_no: base_reg_no, special: false };

                Opnd::Mem(Mem {
                    base: base_reg,
                    disp,
                    num_bits
                })
            }

            _ => panic!("unsupported x86 operand type")
        }
    }
}






/// Branch target (something that we can jump to)
/// for branch instructions
#[derive(Clone, PartialEq, Eq, Debug)]
enum Target
{
    CodePtr(CodePtr),   // Pointer to a piece of code (e.g. side-exit)
    LabelName(String),  // A label without an index in the output
    LabelIdx(usize),      // A label that has been indexed
}

/// YJIT IR instruction
pub struct Insn
{
    // Opcode for the instruction
    op: Op,

    // List of input operands/values
    opnds: Vec<Opnd>,

    // List of branch targets (branch instructions only)
    target: Option<Target>,

    // Position in the generated machine code
    // Useful for comments and for patching jumps
    pos: Option<CodePtr>,
}

/// Object into which we assemble instructions to be
/// optimized and lowered
struct Assembler
{
    insns: Vec<Insn>
}

impl Assembler
{
    fn new() -> Assembler {
        Assembler {
            insns: Vec::default()
        }
    }

    fn push_insn(&mut self, op: Op, opnds: Vec<Opnd>, target: Option<Target>) -> Opnd
    {
        let insn_idx = self.insns.len();

        let insn = Insn {
            op: op,
            opnds: opnds,
            target: target,
            pos: None
        };
        self.insns.push(insn);

        // Return an operand for the output of this instruction
        Opnd::InsnOut(insn_idx)
    }

    // Add a label at the current position
    fn label(&mut self, name: &str) -> Target
    {
        let insn_idx = self.insns.len();

        let insn = Insn {
            op: Op::Label,
            opnds: vec![],
            target: None,
            pos: None
        };
        self.insns.push(insn);

        Target::LabelIdx(insn_idx)
    }

    fn alloc_regs(&mut self)
    {
        // ???
    }

    // Optimize and compile the stored instructions
    fn compile()
    {
        // Peephole optimizations
        // Register allocation
        // Generic lowering pass
        // Platform-specific lowering
    }
}

impl Assembler
{
    // Add a comment, no output operand
    fn comment(&mut self, text: &str)
    {
        self.push_insn(Op::Add, vec![ Opnd::String(text.to_owned()) ], None);
    }

    // Low-level, no output operand
    fn test(&mut self, opnd0: Opnd, opnd1: Opnd)
    {
        self.push_insn(Op::Add, vec![opnd0, opnd1], None);
    }

    // Low-level, no output operand
    fn mov(&mut self, opnd0: Opnd, opnd1: Opnd)
    {
        self.push_insn(Op::Add, vec![opnd0, opnd1], None);
    }

    // Jump if not zero
    fn jnz(&mut self, target: Target)
    {
        self.push_insn(Op::Jnz, vec![], Some(target));
    }
}

macro_rules! def_push_insn_2_opnd {
    ($op_name:ident, $opcode:expr) => {
        impl Assembler
        {
            fn $op_name(&mut self, opnd0: Opnd, opnd1: Opnd) -> Opnd
            {
                self.push_insn($opcode, vec![opnd0, opnd1], None)
            }
        }
    };
}

def_push_insn_2_opnd!(add, Op::Add);
def_push_insn_2_opnd!(sub, Op::Sub);
def_push_insn_2_opnd!(and, Op::And);

// NOTE: these methods are temporary and will likely move
// to context.rs later
// They are just wrappers to convert from X86Opnd into the IR Opnd type
impl Context
{
    pub fn ir_stack_pop(&mut self, n: usize) -> Opnd {
        self.stack_pop(n).into()
    }

    pub fn ir_stack_push(&mut self, val_type: Type) -> Opnd {
        self.stack_push(val_type).into()
    }

    pub fn ir_stack_push_mapping(&mut self, (mapping, temp_type): (TempMapping, Type)) -> Opnd {
        self.stack_push_mapping((mapping, temp_type)).into()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
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
        asm.test(object_opnd, Opnd::UImm(RUBY_IMMEDIATE_MASK as u64));
        asm.jnz(Target::CodePtr(side_exit));

        // Test that the object is not false or nil
        //cmp(cb, object_opnd, uimm_opnd(Qnil.into()));
        //jbe_ptr(cb, side_exit);
    }

    #[test]
    fn test_add() {
        let mut asm = Assembler::new();
        let out = asm.add(SP, Opnd::UImm(1));
        asm.add(out, Opnd::UImm(2));
    }
}
