#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

use std::cell::Cell;
use std::fmt;
use std::convert::From;
use std::mem::take;
use crate::cruby::{VALUE};
use crate::virtualmem::{CodePtr};
use crate::asm::{CodeBlock, uimm_num_bits, imm_num_bits};
use crate::core::{Context, Type, TempMapping, InsnOpnd};

#[cfg(target_arch = "x86_64")]
use crate::backend::x86_64::*;

#[cfg(target_arch = "aarch64")]
use crate::backend::arm64::*;

pub const EC: Opnd = _EC;
pub const CFP: Opnd = _CFP;
pub const SP: Opnd = _SP;

pub const C_ARG_OPNDS: [Opnd; 6] = _C_ARG_OPNDS;
pub const C_RET_OPND: Opnd = _C_RET_OPND;

// Memory operand base
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum MemBase
{
    Reg(u8),
    InsnOut(usize),
}

// Memory location
#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Mem
{
    // Base register number or instruction index
    pub(super) base: MemBase,

    // Offset relative to the base pointer
    pub(super) disp: i32,

    // Size in bits
    pub(super) num_bits: u8,
}

impl fmt::Debug for Mem {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "Mem{}[{:?}", self.num_bits, self.base)?;
        if self.disp != 0 {
            let sign = if self.disp > 0 { '+' } else { '-' };
            write!(fmt, " {sign} {}", self.disp)?;
        }

        write!(fmt, "]")
    }
}

/// Operand to an IR instruction
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Opnd
{
    None,               // For insns with no output

    // Immediate Ruby value, may be GC'd, movable
    Value(VALUE),

    // Output of a preceding instruction in this block
    InsnOut{ idx: usize, num_bits: u8 },

    // Low-level operands, for lowering
    Imm(i64),           // Raw signed immediate
    UImm(u64),          // Raw unsigned immediate
    Mem(Mem),           // Memory location
    Reg(Reg),           // Machine register
}

impl fmt::Debug for Opnd {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        use Opnd::*;
        match self {
            Self::None => write!(fmt, "None"),
            Value(val) => write!(fmt, "Value({val:?})"),
            InsnOut { idx, num_bits } => write!(fmt, "Out{num_bits}({idx})"),
            Imm(signed) => write!(fmt, "{signed:x}_i64"),
            UImm(unsigned) => write!(fmt, "{unsigned:x}_u64"),
            // Say Mem and Reg only once
            Mem(mem) => write!(fmt, "{mem:?}"),
            Reg(reg) => write!(fmt, "{reg:?}"),
        }
    }
}

impl Opnd
{
    /// Convenience constructor for memory operands
    pub fn mem(num_bits: u8, base: Opnd, disp: i32) -> Self {
        match base {
            Opnd::Reg(base_reg) => {
                assert!(base_reg.num_bits == 64);
                Opnd::Mem(Mem {
                    base: MemBase::Reg(base_reg.reg_no),
                    disp: disp,
                    num_bits: num_bits,
                })
            },

            Opnd::InsnOut{idx, num_bits } => {
                assert!(num_bits == 64);
                Opnd::Mem(Mem {
                    base: MemBase::InsnOut(idx),
                    disp: disp,
                    num_bits: num_bits,
                })
            },

            _ => unreachable!("memory operand with non-register base")
        }
    }

    /// Constructor for constant pointer operand
    pub fn const_ptr(ptr: *const u8) -> Self {
        Opnd::UImm(ptr as u64)
    }

    pub fn is_some(&self) -> bool {
        match *self {
            Opnd::None => false,
            _ => true,
        }
    }

    /// Unwrap a register operand
    pub fn unwrap_reg(&self) -> Reg {
        match self {
            Opnd::Reg(reg) => *reg,
            _ => unreachable!("trying to unwrap {:?} into reg", self)
        }
    }

    pub fn num_bits(&self) -> Option<u8> {
        match *self {
            Opnd::Reg(Reg { num_bits, .. }) => Some(num_bits),
            Opnd::Mem(Mem { num_bits, .. }) => Some(num_bits),
            Opnd::InsnOut { num_bits, .. } => Some(num_bits),
            _ => None
        }
    }

    /// Get the size in bits for register/memory operands
    pub fn rm_num_bits(&self) -> u8 {
        self.num_bits().unwrap()
    }

    /// Maps the indices from a previous list of instructions to a new list of
    /// instructions.
    pub fn map_index(self, indices: &Vec<usize>) -> Opnd {
        match self {
            Opnd::InsnOut { idx, num_bits } => {
                Opnd::InsnOut { idx: indices[idx], num_bits }
            }
            Opnd::Mem(Mem { base: MemBase::InsnOut(idx), disp, num_bits }) => {
                Opnd::Mem(Mem { base: MemBase::InsnOut(indices[idx]), disp, num_bits })
            },
            _ => self
        }
    }
}

impl From<usize> for Opnd {
    fn from(value: usize) -> Self {
        Opnd::UImm(value.try_into().unwrap())
    }
}

impl From<u64> for Opnd {
    fn from(value: u64) -> Self {
        Opnd::UImm(value.try_into().unwrap())
    }
}

impl From<i64> for Opnd {
    fn from(value: i64) -> Self {
        Opnd::Imm(value)
    }
}

impl From<i32> for Opnd {
    fn from(value: i32) -> Self {
        Opnd::Imm(value.try_into().unwrap())
    }
}

impl From<u32> for Opnd {
    fn from(value: u32) -> Self {
        Opnd::UImm(value as u64)
    }
}

impl From<VALUE> for Opnd {
    fn from(value: VALUE) -> Self {
        Opnd::Value(value)
    }
}

/// Branch target (something that we can jump to)
/// for branch instructions
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Target
{
    CodePtr(CodePtr),   // Pointer to a piece of YJIT-generated code (e.g. side-exit)
    FunPtr(*const u8),  // Pointer to a C function
    Label(usize),       // A label within the generated code
}

impl Target
{
    pub fn unwrap_fun_ptr(&self) -> *const u8 {
        match self {
            Target::FunPtr(ptr) => *ptr,
            _ => unreachable!("trying to unwrap {:?} into fun ptr", self)
        }
    }

    pub fn unwrap_label_idx(&self) -> usize {
        match self {
            Target::Label(idx) => *idx,
            _ => unreachable!("trying to unwrap {:?} into label", self)
        }
    }

    pub fn unwrap_code_ptr(&self) -> CodePtr {
        match self {
            Target::CodePtr(ptr) => *ptr,
            _ => unreachable!("trying to unwrap {:?} into code ptr", self)
        }
    }
}

impl From<CodePtr> for Target {
    fn from(code_ptr: CodePtr) -> Self {
        Target::CodePtr(code_ptr)
    }
}

type PosMarkerFn = Box<dyn Fn(CodePtr)>;

/// YJIT IR instruction
pub enum Insn {
    /// Add two operands together, and return the result as a new operand.
    Add { left: Opnd, right: Opnd, out: Opnd },

    /// This is the same as the OP_ADD instruction, except that it performs the
    /// binary AND operation.
    And { left: Opnd, right: Opnd, out: Opnd },

    /// Bake a string directly into the instruction stream.
    BakeString(String),

    // Trigger a debugger breakpoint
    Breakpoint,

    /// Add a comment into the IR at the point that this instruction is added.
    /// It won't have any impact on that actual compiled code.
    Comment(String),

    /// Compare two operands
    Cmp { left: Opnd, right: Opnd },

    /// Pop a register from the C stack
    CPop { out: Opnd },

    /// Pop all of the caller-save registers and the flags from the C stack
    CPopAll,

    /// Pop a register from the C stack and store it into another register
    CPopInto(Opnd),

    /// Push a register onto the C stack
    CPush(Opnd),

    /// Push all of the caller-save registers and the flags to the C stack
    CPushAll,

    // C function call with N arguments (variadic)
    CCall { opnds: Vec<Opnd>, target: Target, out: Opnd },

    // C function return
    CRet(Opnd),

    /// Conditionally select if equal
    CSelE { truthy: Opnd, falsy: Opnd, out: Opnd },

    /// Conditionally select if greater
    CSelG { truthy: Opnd, falsy: Opnd, out: Opnd },

    /// Conditionally select if greater or equal
    CSelGE { truthy: Opnd, falsy: Opnd, out: Opnd },

    /// Conditionally select if less
    CSelL { truthy: Opnd, falsy: Opnd, out: Opnd },

    /// Conditionally select if less or equal
    CSelLE { truthy: Opnd, falsy: Opnd, out: Opnd },

    /// Conditionally select if not equal
    CSelNE { truthy: Opnd, falsy: Opnd, out: Opnd },

    /// Conditionally select if not zero
    CSelNZ { truthy: Opnd, falsy: Opnd, out: Opnd },

    /// Conditionally select if zero
    CSelZ { truthy: Opnd, falsy: Opnd, out: Opnd },

    /// Set up the frame stack as necessary per the architecture.
    FrameSetup,

    /// Tear down the frame stack as necessary per the architecture.
    FrameTeardown,

    // Atomically increment a counter
    // Input: memory operand, increment value
    // Produces no output
    IncrCounter { mem: Opnd, value: Opnd },

    /// Jump if below or equal
    Jbe(Target),

    /// Jump if equal
    Je(Target),

    // Unconditional jump to a branch target
    Jmp(Target),

    // Unconditional jump which takes a reg/mem address operand
    JmpOpnd(Opnd),

    /// Jump if not equal
    Jne(Target),

    /// Jump if not zero
    Jnz(Target),

    /// Jump if overflow
    Jo(Target),

    /// Jump if zero
    Jz(Target),

    // Add a label into the IR at the point that this instruction is added.
    Label(Target),

    // Load effective address relative to the current instruction pointer. It
    // accepts a single signed immediate operand.
    LeaLabel { target: Target, out: Opnd },

    // Load effective address
    Lea { opnd: Opnd, out: Opnd },

    /// Take a specific register. Signal the register allocator to not use it.
    LiveReg { opnd: Opnd, out: Opnd },

    // A low-level instruction that loads a value into a register.
    Load { opnd: Opnd, out: Opnd },

    // A low-level instruction that loads a value into a register and
    // sign-extends it to a 64-bit value.
    LoadSExt { opnd: Opnd, out: Opnd },

    /// Shift a value left by a certain amount.
    LShift { opnd: Opnd, shift: Opnd, out: Opnd },

    // A low-level mov instruction. It accepts two operands.
    Mov { dest: Opnd, src: Opnd },

    // Perform the NOT operation on an individual operand, and return the result
    // as a new operand. This operand can then be used as the operand on another
    // instruction.
    Not { opnd: Opnd, out: Opnd },

    // This is the same as the OP_ADD instruction, except that it performs the
    // binary OR operation.
    Or { left: Opnd, right: Opnd, out: Opnd },

    // Mark a position in the generated code
    PosMarker(PosMarkerFn),

    /// Shift a value right by a certain amount (signed).
    RShift { opnd: Opnd, shift: Opnd, out: Opnd },

    // Low-level instruction to store a value to memory.
    Store { dest: Opnd, src: Opnd },

    // This is the same as the OP_ADD instruction, except for subtraction.
    Sub { left: Opnd, right: Opnd, out: Opnd },

    // Bitwise AND test instruction
    Test { left: Opnd, right: Opnd },

    /// Shift a value right by a certain amount (unsigned).
    URShift { opnd: Opnd, shift: Opnd, out: Opnd }
}

impl Insn {
    pub fn get_out_opnd(&self) -> Option<&Opnd> {
        match self {
            Insn::Add { out, .. } | Insn::And { out, .. } | Insn::CCall { out, .. } | Insn::CPop { out } | Insn::CSelE { out, .. } | Insn::CSelG { out, .. } | Insn::CSelGE { out, .. } | Insn::CSelL { out, .. } | Insn::CSelLE { out, .. } | Insn::CSelNE { out, .. } | Insn::CSelNZ { out, .. } | Insn::CSelZ { out, .. } | Insn::LeaLabel { out, .. } | Insn::Lea { out, .. } | Insn::LiveReg { out, .. } | Insn::Load { out, .. } | Insn::LoadSExt { out, .. } | Insn::LShift { out, .. } | Insn::Not { out, .. } | Insn::Or { out, .. } | Insn::RShift { out, .. } | Insn::Sub { out, .. } | Insn::URShift { out, .. } => Some(out),
            _ => None
        }
    }

    pub fn get_out_opnd_num_bits(&self) -> Option<u8> {
        fn merge_num_bits(left: Option<u8>, right: Option<u8>) -> Option<u8> {
            if let Some(left_num_bits) = left {
                if let Some(right_num_bits) = right {
                    assert_eq!(left_num_bits, right_num_bits, "operands of incompatible sizes");
                }

                Some(left_num_bits)
            } else {
                right
            }
        }

        match self {
            Insn::CPop { out } |
            Insn::LeaLabel { out, .. } => {
                Some(64)
            },
            Insn::Lea { opnd, out } |
            Insn::LiveReg { opnd, out } |
            Insn::Load { opnd, out } |
            Insn::LoadSExt { opnd, out } |
            Insn::Not { opnd, out } => {
                opnd.num_bits()
            },
            Insn::Add { left: opnd0 @ _, right: opnd1 @ _, out } |
            Insn::And { left: opnd0 @ _, right: opnd1 @ _, out } |
            Insn::CSelE { truthy: opnd0 @ _, falsy: opnd1 @ _, out } |
            Insn::CSelG { truthy: opnd0 @ _, falsy: opnd1 @ _, out } |
            Insn::CSelGE { truthy: opnd0 @ _, falsy: opnd1 @ _, out } |
            Insn::CSelL { truthy: opnd0 @ _, falsy: opnd1 @ _, out } |
            Insn::CSelLE { truthy: opnd0 @ _, falsy: opnd1 @ _, out } |
            Insn::CSelNE { truthy: opnd0 @ _, falsy: opnd1 @ _, out } |
            Insn::CSelNZ { truthy: opnd0 @ _, falsy: opnd1 @ _, out } |
            Insn::CSelZ { truthy: opnd0 @ _, falsy: opnd1 @ _, out } |
            Insn::LShift { opnd: opnd0 @ _, shift: opnd1 @ _, out } |
            Insn::Or { left: opnd0 @ _, right: opnd1 @ _, out } |
            Insn::RShift { opnd: opnd0 @ _, shift: opnd1 @ _, out } |
            Insn::Sub { left: opnd0 @ _, right: opnd1 @ _, out } |
            Insn::URShift { opnd: opnd0 @ _, shift: opnd1 @ _, out } => {
                merge_num_bits(opnd0.num_bits(), opnd1.num_bits())
            },
            Insn::CCall { opnds, .. } => {
                opnds.iter().map(|opnd| opnd.num_bits()).reduce(|accum, num_bits| {
                    merge_num_bits(accum, num_bits)
                }).and_then(|num_bits| { num_bits })
            },
            _ => None,
        }
    }

    pub fn map_opnd_indices(&mut self, indices: &Vec<usize>) {
        match self {
            Insn::BakeString(_) |
            Insn::Breakpoint |
            Insn::Comment(_) |
            Insn::CPop { .. } |
            Insn::CPopAll |
            Insn::CPushAll |
            Insn::FrameSetup |
            Insn::FrameTeardown |
            Insn::Jbe(_) |
            Insn::Je(_) |
            Insn::Jmp(_) |
            Insn::Jne(_) |
            Insn::Jnz(_) |
            Insn::Jo(_) |
            Insn::Jz(_) |
            Insn::Label(_) |
            Insn::LeaLabel { .. } |
            Insn::PosMarker(_) => {},
            Insn::CPopInto(opnd) |
            Insn::CPush(opnd) |
            Insn::CRet(opnd) |
            Insn::JmpOpnd(opnd) |
            Insn::Lea { opnd, .. } |
            Insn::LiveReg { opnd, .. } |
            Insn::Load { opnd, .. } |
            Insn::LoadSExt { opnd, .. } |
            Insn::Not { opnd, .. } => {
                *opnd = opnd.map_index(indices);
            }
            Insn::Add { left: opnd0 @ _, right: opnd1 @ _, .. } |
            Insn::And { left: opnd0 @ _, right: opnd1 @ _, .. } |
            Insn::Cmp { left: opnd0 @ _, right: opnd1 @ _ } |
            Insn::CSelE { truthy: opnd0 @ _, falsy: opnd1 @ _, .. } |
            Insn::CSelG { truthy: opnd0 @ _, falsy: opnd1 @ _, .. } |
            Insn::CSelGE { truthy: opnd0 @ _, falsy: opnd1 @ _, .. } |
            Insn::CSelL { truthy: opnd0 @ _, falsy: opnd1 @ _, .. } |
            Insn::CSelLE { truthy: opnd0 @ _, falsy: opnd1 @ _, .. } |
            Insn::CSelNE { truthy: opnd0 @ _, falsy: opnd1 @ _, .. } |
            Insn::CSelNZ { truthy: opnd0 @ _, falsy: opnd1 @ _, .. } |
            Insn::CSelZ { truthy: opnd0 @ _, falsy: opnd1 @ _, .. } |
            Insn::IncrCounter { mem: opnd0 @ _, value: opnd1 @ _ } |
            Insn::LShift { opnd: opnd0 @ _, shift: opnd1 @ _, .. } |
            Insn::Mov { dest: opnd0 @ _, src: opnd1 @ _ } |
            Insn::Or { left: opnd0 @ _, right: opnd1 @ _, .. } |
            Insn::RShift { opnd: opnd0 @ _, shift: opnd1 @ _, .. } |
            Insn::Store { dest: opnd0 @ _, src: opnd1 @ _ } |
            Insn::Sub { left: opnd0 @ _, right: opnd1 @ _, .. } |
            Insn::Test { left: opnd0 @ _, right: opnd1 @ _ } |
            Insn::URShift { opnd: opnd0 @ _, shift: opnd1 @ _, .. } => {
                *opnd0 = opnd0.map_index(indices);
                *opnd1 = opnd1.map_index(indices);
            }
            Insn::CCall { opnds, .. } => {
                for opnd in opnds {
                    *opnd = opnd.map_index(indices);
                }
            }
        }
    }

    pub fn map_opnd_out(&mut self, asm: &Assembler) {
        // Replace InsnOut operands by their corresponding register
        fn map_opnd(asm: &Assembler, opnd: &mut Opnd) {
            match *opnd {
                Opnd::InsnOut { idx, .. } => {
                    *opnd = *asm.insns[idx].get_out_opnd().unwrap();
                },
                Opnd::Mem(Mem { base: MemBase::InsnOut(idx), disp, num_bits }) => {
                    let out_reg = asm.insns[idx].get_out_opnd().unwrap().unwrap_reg();
                    let base = MemBase::Reg(out_reg.reg_no);
                    *opnd = Opnd::Mem(Mem { base, disp, num_bits })
                },
                _ => {}
            };
        }

        match self {
            Insn::BakeString(_) |
            Insn::Breakpoint |
            Insn::Comment(_) |
            Insn::CPop { .. } |
            Insn::CPopAll |
            Insn::CPushAll |
            Insn::FrameSetup |
            Insn::FrameTeardown |
            Insn::Jbe(_) |
            Insn::Je(_) |
            Insn::Jmp(_) |
            Insn::Jne(_) |
            Insn::Jnz(_) |
            Insn::Jo(_) |
            Insn::Jz(_) |
            Insn::Label(_) |
            Insn::LeaLabel { .. } |
            Insn::PosMarker(_) => {},
            Insn::CPopInto(opnd) |
            Insn::CPush(opnd) |
            Insn::CRet(opnd) |
            Insn::JmpOpnd(opnd) |
            Insn::Lea { opnd, .. } |
            Insn::LiveReg { opnd, .. } |
            Insn::Load { opnd, .. } |
            Insn::LoadSExt { opnd, .. } |
            Insn::Not { opnd, .. } => {
                map_opnd(asm, opnd);
            }
            Insn::Add { left: opnd0 @ _, right: opnd1 @ _, .. } |
            Insn::And { left: opnd0 @ _, right: opnd1 @ _, .. } |
            Insn::Cmp { left: opnd0 @ _, right: opnd1 @ _ } |
            Insn::CSelE { truthy: opnd0 @ _, falsy: opnd1 @ _, .. } |
            Insn::CSelG { truthy: opnd0 @ _, falsy: opnd1 @ _, .. } |
            Insn::CSelGE { truthy: opnd0 @ _, falsy: opnd1 @ _, .. } |
            Insn::CSelL { truthy: opnd0 @ _, falsy: opnd1 @ _, .. } |
            Insn::CSelLE { truthy: opnd0 @ _, falsy: opnd1 @ _, .. } |
            Insn::CSelNE { truthy: opnd0 @ _, falsy: opnd1 @ _, .. } |
            Insn::CSelNZ { truthy: opnd0 @ _, falsy: opnd1 @ _, .. } |
            Insn::CSelZ { truthy: opnd0 @ _, falsy: opnd1 @ _, .. } |
            Insn::IncrCounter { mem: opnd0 @ _, value: opnd1 @ _ } |
            Insn::LShift { opnd: opnd0 @ _, shift: opnd1 @ _, .. } |
            Insn::Mov { dest: opnd0 @ _, src: opnd1 @ _ } |
            Insn::Or { left: opnd0 @ _, right: opnd1 @ _, .. } |
            Insn::RShift { opnd: opnd0 @ _, shift: opnd1 @ _, .. } |
            Insn::Store { dest: opnd0 @ _, src: opnd1 @ _ } |
            Insn::Sub { left: opnd0 @ _, right: opnd1 @ _, .. } |
            Insn::Test { left: opnd0 @ _, right: opnd1 @ _ } |
            Insn::URShift { opnd: opnd0 @ _, shift: opnd1 @ _, .. } => {
                map_opnd(asm, opnd0);
                map_opnd(asm, opnd1);
            }
            Insn::CCall { opnds, .. } => {
                for opnd in opnds {
                    map_opnd(asm, opnd);
                }
            }
        }
    }

    pub fn set_out_opnd(&mut self, opnd: Opnd) {
        match self {
            Insn::Add { out, .. } | Insn::And { out, .. } | Insn::CCall { out, .. } | Insn::CPop { out } | Insn::CSelE { out, .. } | Insn::CSelG { out, .. } | Insn::CSelGE { out, .. } | Insn::CSelL { out, .. } | Insn::CSelLE { out, .. } | Insn::CSelNE { out, .. } | Insn::CSelNZ { out, .. } | Insn::CSelZ { out, .. } | Insn::LeaLabel { out, .. } | Insn::Lea { out, .. } | Insn::LiveReg { out, .. } | Insn::Load { out, .. } | Insn::LoadSExt { out, .. } | Insn::LShift { out, .. } | Insn::Not { out, .. } | Insn::Or { out, .. } | Insn::RShift { out, .. } | Insn::Sub { out, .. } | Insn::URShift { out, .. } => {
                *out = opnd;
            },
            _ => {}
        }
    }

    pub fn set_out_opnd_idx(&mut self, idx: usize) -> Option<Opnd> {
        if let Some(num_bits) = self.get_out_opnd_num_bits() {
            let out_opnd = Opnd::InsnOut { idx, num_bits };
            self.set_out_opnd(out_opnd);
            Some(out_opnd)
        } else {
            None
        }
    }

    pub fn get_opnds<'a>(&'a self) -> InsnOpndIterator<'a> {
        InsnOpndIterator::new(self)
    }
}

pub struct InsnOpndIterator<'a> {
    insn: &'a Insn,
    idx: usize
}

impl<'a> InsnOpndIterator<'a> {
    fn new(insn: &'a Insn) -> Self {
        InsnOpndIterator { insn, idx: 0 }
    }
}

impl<'a> Iterator for InsnOpndIterator<'a> {
    type Item = &'a Opnd;

    fn next(&mut self) -> Option<Self::Item> {
        match self.insn {
            Insn::BakeString(_) |
            Insn::Breakpoint |
            Insn::Comment(_) |
            Insn::CPop { .. } |
            Insn::CPopAll |
            Insn::CPushAll |
            Insn::FrameSetup |
            Insn::FrameTeardown |
            Insn::Jbe(_) |
            Insn::Je(_) |
            Insn::Jmp(_) |
            Insn::Jne(_) |
            Insn::Jnz(_) |
            Insn::Jo(_) |
            Insn::Jz(_) |
            Insn::Label(_) |
            Insn::LeaLabel { .. } |
            Insn::PosMarker(_) => None,
            Insn::CPopInto(opnd) |
            Insn::CPush(opnd) |
            Insn::CRet(opnd) |
            Insn::JmpOpnd(opnd) |
            Insn::Lea { opnd, .. } |
            Insn::LiveReg { opnd, .. } |
            Insn::Load { opnd, .. } |
            Insn::LoadSExt { opnd, .. } |
            Insn::Not { opnd, .. } => {
                match self.idx {
                    0 => {
                        self.idx += 1;
                        Some(opnd)
                    },
                    _ => None
                }
            }
            Insn::Add { left: opnd0 @ _, right: opnd1 @ _, .. } |
            Insn::And { left: opnd0 @ _, right: opnd1 @ _, .. } |
            Insn::Cmp { left: opnd0 @ _, right: opnd1 @ _ } |
            Insn::CSelE { truthy: opnd0 @ _, falsy: opnd1 @ _, .. } |
            Insn::CSelG { truthy: opnd0 @ _, falsy: opnd1 @ _, .. } |
            Insn::CSelGE { truthy: opnd0 @ _, falsy: opnd1 @ _, .. } |
            Insn::CSelL { truthy: opnd0 @ _, falsy: opnd1 @ _, .. } |
            Insn::CSelLE { truthy: opnd0 @ _, falsy: opnd1 @ _, .. } |
            Insn::CSelNE { truthy: opnd0 @ _, falsy: opnd1 @ _, .. } |
            Insn::CSelNZ { truthy: opnd0 @ _, falsy: opnd1 @ _, .. } |
            Insn::CSelZ { truthy: opnd0 @ _, falsy: opnd1 @ _, .. } |
            Insn::IncrCounter { mem: opnd0 @ _, value: opnd1 @ _ } |
            Insn::LShift { opnd: opnd0 @ _, shift: opnd1 @ _, .. } |
            Insn::Mov { dest: opnd0 @ _, src: opnd1 @ _ } |
            Insn::Or { left: opnd0 @ _, right: opnd1 @ _, .. } |
            Insn::RShift { opnd: opnd0 @ _, shift: opnd1 @ _, .. } |
            Insn::Store { dest: opnd0 @ _, src: opnd1 @ _ } |
            Insn::Sub { left: opnd0 @ _, right: opnd1 @ _, .. } |
            Insn::Test { left: opnd0 @ _, right: opnd1 @ _ } |
            Insn::URShift { opnd: opnd0 @ _, shift: opnd1 @ _, .. } => {
                match self.idx {
                    0 => {
                        self.idx += 1;
                        Some(opnd0)
                    }
                    1 => {
                        self.idx += 1;
                        Some(opnd1)
                    }
                    _ => None
                }
            }
            Insn::CCall { opnds, .. } => {
                if self.idx < opnds.len() {
                    let opnd = &opnds[self.idx];
                    self.idx += 1;
                    Some(opnd)
                } else {
                    None
                }
            }
        }
    }
}

impl fmt::Debug for Insn {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Insn::Add { left, right, out } => {
                write!(fmt, "Add({left:?}, {right:?}) -> {out:?}")
            },
            Insn::And { left, right, out } => {
                write!(fmt, "And({left:?}, {right:?}) -> {out:?}")
            },
            Insn::BakeString(text) => {
                write!(fmt, "BakeString({text:?})")
            },
            Insn::Breakpoint => {
                write!(fmt, "Breakpoint()")
            },
            Insn::Cmp { left, right } => {
                write!(fmt, "Cmp({left:?}, {right:?})")
            },
            Insn::Comment(text) => {
                write!(fmt, "Comment({text:?})")
            },
            Insn::CPop { out } => {
                write!(fmt, "CPop() -> {out:?}")
            },
            Insn::CPopAll => {
                write!(fmt, "CPopAll()")
            },
            Insn::CPopInto(opnd) => {
                write!(fmt, "CPopInto({opnd:?})")
            },
            Insn::CPush(opnd) => {
                write!(fmt, "CPush({opnd:?})")
            },
            Insn::CPushAll => {
                write!(fmt, "CPushAll()")
            },
            Insn::CCall { opnds, target, out } => {
                write!(fmt, "CCall({opnds:?}) target={target:?} -> {out:?}")
            },
            Insn::CRet(opnd) => {
                write!(fmt, "CRet({opnd:?})")
            },
            Insn::CSelE { truthy, falsy, out } => {
                write!(fmt, "CSelE({truthy:?}, {falsy:?}) -> {out:?}")
            },
            Insn::CSelG { truthy, falsy, out } => {
                write!(fmt, "CSelG({truthy:?}, {falsy:?}) -> {out:?}")
            },
            Insn::CSelGE { truthy, falsy, out } => {
                write!(fmt, "CSelGE({truthy:?}, {falsy:?}) -> {out:?}")
            },
            Insn::CSelL { truthy, falsy, out } => {
                write!(fmt, "CSelL({truthy:?}, {falsy:?}) -> {out:?}")
            },
            Insn::CSelLE { truthy, falsy, out } => {
                write!(fmt, "CSelLE({truthy:?}, {falsy:?}) -> {out:?}")
            },
            Insn::CSelNE { truthy, falsy, out } => {
                write!(fmt, "CSelNE({truthy:?}, {falsy:?}) -> {out:?}")
            },
            Insn::CSelNZ { truthy, falsy, out } => {
                write!(fmt, "CSelNZ({truthy:?}, {falsy:?}) -> {out:?}")
            },
            Insn::CSelZ { truthy, falsy, out } => {
                write!(fmt, "CSelZ({truthy:?}, {falsy:?}) -> {out:?}")
            },
            Insn::FrameSetup => {
                write!(fmt, "FrameSetup()")
            },
            Insn::FrameTeardown => {
                write!(fmt, "FrameTeardown()")
            },
            Insn::IncrCounter { mem, value } => {
                write!(fmt, "IncrCounter({mem:?}, {value:?})")
            },
            Insn::Jbe(target) => {
                write!(fmt, "Jbe() target={target:?}")
            },
            Insn::Je(target) => {
                write!(fmt, "Je() target={target:?}")
            },
            Insn::Jmp(target) => {
                write!(fmt, "Jmp() target={target:?}")
            },
            Insn::JmpOpnd(opnd) => {
                write!(fmt, "JmpOpnd({opnd:?})")
            },
            Insn::Jne(target) => {
                write!(fmt, "Jne() target={target:?}")
            },
            Insn::Jnz(target) => {
                write!(fmt, "Jnz() target={target:?}")
            },
            Insn::Jo(target) => {
                write!(fmt, "Jo() target={target:?}")
            },
            Insn::Jz(target) => {
                write!(fmt, "Jz() target={target:?}")
            },
            Insn::Label(target) => {
                write!(fmt, "Label() target={target:?}")
            },
            Insn::LeaLabel { target, out } => {
                write!(fmt, "LeaLabel() target={target:?} -> {out:?}")
            },
            Insn::Lea { opnd, out } => {
                write!(fmt, "Lea({opnd:?}) -> {out:?}")
            },
            Insn::LiveReg { opnd, out } => {
                write!(fmt, "LiveReg({opnd:?}) -> {out:?}")
            },
            Insn::Load { opnd, out } => {
                write!(fmt, "Load({opnd:?}) -> {out:?}")
            },
            Insn::LoadSExt { opnd, out } => {
                write!(fmt, "LoadSExt({opnd:?}) -> {out:?}")
            },
            Insn::LShift { opnd, shift, out } => {
                write!(fmt, "LShift({opnd:?}, {shift:?}) -> {out:?}")
            },
            Insn::Mov { dest, src } => {
                write!(fmt, "Mov({dest:?}, {src:?})")
            },
            Insn::Not { opnd, out } => {
                write!(fmt, "Not({opnd:?}) -> {out:?}")
            },
            Insn::Or { left, right, out } => {
                write!(fmt, "Or({left:?}, {right:?}) -> {out:?}")
            },
            Insn::PosMarker(_) => {
                write!(fmt, "PosMarker()")
            },  
            Insn::RShift { opnd, shift, out } => {
                write!(fmt, "RShift({opnd:?}, {shift:?}) -> {out:?}")
            },
            Insn::Store { dest, src } => {
                write!(fmt, "Store({dest:?}, {src:?})")
            },
            Insn::Sub { left, right, out } => {
                write!(fmt, "Sub({left:?}, {right:?}) -> {out:?}")
            },
            Insn::Test { left, right } => {
                write!(fmt, "Test({left:?}, {right:?})")
            },
            Insn::URShift { opnd, shift, out } => {
                write!(fmt, "URShift({opnd:?}, {shift:?}) -> {out:?}")
            }
        }
    }
}

/// Object into which we assemble instructions to be
/// optimized and lowered
pub struct Assembler
{
    pub(super) insns: Vec<Insn>,

    /// Parallel vec with insns
    /// Index of the last insn using the output of this insn
    pub(super) live_ranges: Vec<usize>,

    /// Names of labels
    pub(super) label_names: Vec<String>,
}

impl Assembler
{
    pub fn new() -> Self {
        Self::new_with_label_names(Vec::default())
    }

    pub fn new_with_label_names(label_names: Vec<String>) -> Self {
        Self {
            insns: Vec::default(),
            live_ranges: Vec::default(),
            label_names
        }
    }

    /// Append an instruction to the list
    pub(super) fn push_insn(&mut self, mut insn: Insn) -> Opnd {
        // Index of this instruction
        let insn_idx = self.insns.len();

        // If we find any InsnOut from previous instructions, we're going to
        // update the live range of the previous instruction to point to this
        // one.
        for opnd in insn.get_opnds() {
            match opnd {
                Opnd::InsnOut { idx, .. } => {
                    assert!(*idx < self.insns.len());
                    self.live_ranges[*idx] = insn_idx;
                }
                Opnd::Mem(Mem { base: MemBase::InsnOut(idx), .. }) => {
                    assert!(*idx < self.insns.len());
                    self.live_ranges[*idx] = insn_idx;
                }
                _ => {}
            }
        }

        let out_opnd = insn.set_out_opnd_idx(insn_idx);
        self.insns.push(insn);
        self.live_ranges.push(insn_idx);

        // Return an operand for the output of this instruction
        out_opnd.unwrap_or(Opnd::InsnOut { idx: insn_idx, num_bits: 64 })
    }

    /// Create a new label instance that we can jump to
    pub fn new_label(&mut self, name: &str) -> Target
    {
        assert!(!name.contains(" "), "use underscores in label names, not spaces");

        let label_idx = self.label_names.len();
        self.label_names.push(name.to_string());
        Target::Label(label_idx)
    }

    /// Add a label at the current position
    pub fn write_label(&mut self, label: Target)
    {
        assert!(label.unwrap_label_idx() < self.label_names.len());
        self.insns.push(Insn::Label(label));
        self.live_ranges.push(self.insns.len());
    }

    /// Sets the out field on the various instructions that require allocated
    /// registers because their output is used as the operand on a subsequent
    /// instruction. This is our implementation of the linear scan algorithm.
    pub(super) fn alloc_regs(mut self, regs: Vec<Reg>) -> Assembler
    {
        //dbg!(&self);

        // First, create the pool of registers.
        let mut pool: u32 = 0;

        // Mutate the pool bitmap to indicate that the register at that index
        // has been allocated and is live.
        fn alloc_reg(pool: &mut u32, regs: &Vec<Reg>) -> Reg {
            for (index, reg) in regs.iter().enumerate() {
                if (*pool & (1 << index)) == 0 {
                    *pool |= 1 << index;
                    return *reg;
                }
            }

            unreachable!("Register spill not supported");
        }

        // Allocate a specific register
        fn take_reg(pool: &mut u32, regs: &Vec<Reg>, reg: &Reg) -> Reg {
            let reg_index = regs.iter().position(|elem| elem.reg_no == reg.reg_no);

            if let Some(reg_index) = reg_index {
                assert_eq!(*pool & (1 << reg_index), 0, "register already allocated");
                *pool |= 1 << reg_index;
            }

            return *reg;
        }

        // Mutate the pool bitmap to indicate that the given register is being
        // returned as it is no longer used by the instruction that previously
        // held it.
        fn dealloc_reg(pool: &mut u32, regs: &Vec<Reg>, reg: &Reg) {
            let reg_index = regs.iter().position(|elem| elem.reg_no == reg.reg_no);

            if let Some(reg_index) = reg_index {
                *pool &= !(1 << reg_index);
            }
        }

        let live_ranges: Vec<usize> = take(&mut self.live_ranges);
        let mut asm = Assembler::new_with_label_names(take(&mut self.label_names));
        let mut iterator = self.into_draining_iter();

        while let Some((index, mut insn)) = iterator.next_unmapped() {
            // Check if this is the last instruction that uses an operand that
            // spans more than one instruction. In that case, return the
            // allocated register to the pool.
            for opnd in insn.get_opnds() {
                match opnd {
                    Opnd::InsnOut{idx, .. } |
                    Opnd::Mem( Mem { base: MemBase::InsnOut(idx), .. }) => {
                        // Since we have an InsnOut, we know it spans more that one
                        // instruction.
                        let start_index = *idx;
                        assert!(start_index < index);

                        // We're going to check if this is the last instruction that
                        // uses this operand. If it is, we can return the allocated
                        // register to the pool.
                        if live_ranges[start_index] == index {
                            if let Opnd::Reg(reg) = asm.insns[start_index].get_out_opnd().unwrap() {
                                dealloc_reg(&mut pool, &regs, &reg);
                            } else {
                                unreachable!("no register allocated for insn {:?}", insn);
                            }
                        }
                    }

                    _ => {}
                }
            }

            // C return values need to be mapped to the C return register
            if let Insn::CCall { .. } = insn {
                assert_eq!(pool, 0, "register lives past C function call");
            }

            // If this instruction is used by another instruction,
            // we need to allocate a register to it
            let mut out_reg = Opnd::None;
            if live_ranges[index] != index {

                // C return values need to be mapped to the C return register
                if let Insn::CCall { .. } = insn {
                    out_reg = Opnd::Reg(take_reg(&mut pool, &regs, &C_RET_REG))
                }

                // If this instruction's first operand maps to a register and
                // this is the last use of the register, reuse the register
                // We do this to improve register allocation on x86
                // e.g. out  = add(reg0, reg1)
                //      reg0 = add(reg0, reg1)
                else if let Some(opnd) = insn.get_opnds().take(1).next() {
                    if let Opnd::InsnOut { idx, .. } = opnd {
                        if live_ranges[*idx] == index {
                            if let Opnd::Reg(reg) = asm.insns[*idx].get_out_opnd().unwrap() {
                                out_reg = Opnd::Reg(take_reg(&mut pool, &regs, &reg))
                            }
                        }
                    }
                }

                // Allocate a new register for this instruction
                if out_reg == Opnd::None {
                    out_reg = if let Insn::LiveReg { opnd, .. } = insn {
                        // Allocate a specific register
                        let reg = opnd.unwrap_reg();
                        Opnd::Reg(take_reg(&mut pool, &regs, &reg))
                    } else {
                        Opnd::Reg(alloc_reg(&mut pool, &regs))
                    }
                }
            }

            // Replace InsnOut operands by their corresponding register
            insn.map_opnd_out(&asm);
            asm.push_insn(insn);

            // Set the output register for this instruction
            let num_insns = asm.insns.len();
            let mut new_insn = &mut asm.insns[num_insns - 1];

            if let Opnd::Reg(reg) = out_reg {
                let num_out_bits = if let Some(opnd) = new_insn.get_out_opnd() {
                    if let Some(num_bits) = opnd.num_bits() {
                        num_bits
                    } else {
                        64
                    }
                } else {
                    64
                };

                out_reg = Opnd::Reg(reg.sub_reg(num_out_bits))
            }

            new_insn.set_out_opnd(out_reg);
        }

        assert_eq!(pool, 0, "Expected all registers to be returned to the pool");
        asm
    }

    /// Compile the instructions down to machine code
    /// NOTE: should compile return a list of block labels to enable
    ///       compiling multiple blocks at a time?
    pub fn compile(self, cb: &mut CodeBlock) -> Vec<u32>
    {
        let alloc_regs = Self::get_alloc_regs();
        self.compile_with_regs(cb, alloc_regs)
    }

    /// Compile with a limited number of registers
    pub fn compile_with_num_regs(self, cb: &mut CodeBlock, num_regs: usize) -> Vec<u32>
    {
        let mut alloc_regs = Self::get_alloc_regs();
        let alloc_regs = alloc_regs.drain(0..num_regs).collect();
        self.compile_with_regs(cb, alloc_regs)
    }

    /// Consume the assembler by creating a new draining iterator.
    pub fn into_draining_iter(self) -> AssemblerDrainingIterator {
        AssemblerDrainingIterator::new(self)
    }

    /// Consume the assembler by creating a new lookback iterator.
    pub fn into_lookback_iter(self) -> AssemblerLookbackIterator {
        AssemblerLookbackIterator::new(self)
    }
}

/// A struct that allows iterating through an assembler's instructions and
/// consuming them as it iterates.
pub struct AssemblerDrainingIterator {
    insns: std::vec::IntoIter<Insn>,
    index: usize,
    indices: Vec<usize>
}

impl AssemblerDrainingIterator {
    fn new(asm: Assembler) -> Self {
        Self {
            insns: asm.insns.into_iter(),
            index: 0,
            indices: Vec::default()
        }
    }

    /// When you're working with two lists of instructions, you need to make
    /// sure you do some bookkeeping to align the indices contained within the
    /// operands of the two lists.
    ///
    /// This function accepts the assembler that is being built and tracks the
    /// end of the current list of instructions in order to maintain that
    /// alignment.
    pub fn map_insn_index(&mut self, asm: &mut Assembler) {
        self.indices.push(asm.insns.len() - 1);
    }

    /// Map an operand by using this iterator's list of mapped indices.
    pub fn map_opnd(&self, opnd: Opnd) -> Opnd {
        opnd.map_index(&self.indices)
    }

    /// Returns the next instruction in the list with the indices corresponding
    /// to the next list of instructions.
    pub fn next_mapped(&mut self) -> Option<(usize, Insn)> {
        self.next_unmapped().map(|(index, mut insn)| {
            insn.map_opnd_indices(&self.indices);
            (index, insn)
        })
    }

    /// Returns the next instruction in the list with the indices corresponding
    /// to the previous list of instructions.
    pub fn next_unmapped(&mut self) -> Option<(usize, Insn)> {
        let index = self.index;
        self.index += 1;
        self.insns.next().map(|insn| (index, insn))
    }
}

/// A struct that allows iterating through references to an assembler's
/// instructions without consuming them.
pub struct AssemblerLookbackIterator {
    asm: Assembler,
    index: Cell<usize>
}

impl AssemblerLookbackIterator {
    fn new(asm: Assembler) -> Self {
        Self { asm, index: Cell::new(0) }
    }

    /// Fetches a reference to an instruction at a specific index.
    pub fn get(&self, index: usize) -> Option<&Insn> {
        self.asm.insns.get(index)
    }

    /// Fetches a reference to an instruction in the list relative to the
    /// current cursor location of this iterator.
    pub fn get_relative(&self, difference: i32) -> Option<&Insn> {
        let index: Result<i32, _> = self.index.get().try_into();
        let relative: Result<usize, _> = index.and_then(|value| (value + difference - 1).try_into());
        relative.ok().and_then(|value| self.asm.insns.get(value))
    }

    /// Fetches the previous instruction relative to the current cursor location
    /// of this iterator.
    pub fn get_previous(&self) -> Option<&Insn> {
        self.get_relative(-1)
    }

    /// Fetches the next instruction relative to the current cursor location of
    /// this iterator.
    pub fn get_next(&self) -> Option<&Insn> {
        self.get_relative(1)
    }

    /// Returns the next instruction in the list with the indices corresponding
    /// to the previous list of instructions.
    pub fn next_unmapped(&self) -> Option<(usize, &Insn)> {
        let index = self.index.get();
        self.index.set(index + 1);
        self.asm.insns.get(index).map(|insn| (index, insn))
    }
}

impl fmt::Debug for Assembler {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "Assembler\n")?;

        for (idx, insn) in self.insns.iter().enumerate() {
            write!(fmt, "    {idx:03} {insn:?}\n")?;
        }

        Ok(())
    }
}

impl Assembler {
    #[must_use]
    pub fn add(&mut self, left: Opnd, right: Opnd) -> Opnd {
        self.push_insn(Insn::Add { left, right, out: Opnd::None })
    }

    #[must_use]
    pub fn and(&mut self, left: Opnd, right: Opnd) -> Opnd {
        self.push_insn(Insn::And { left, right, out: Opnd::None })
    }

    /// Bake a string at the current position
    pub fn bake_string(&mut self, text: &str) {
        self.push_insn(Insn::BakeString(text.to_string()));
    }

    pub fn breakpoint(&mut self) {
        self.push_insn(Insn::Breakpoint);
    }

    #[must_use]
    pub fn ccall(&mut self, fptr: *const u8, opnds: Vec<Opnd>) -> Opnd {
        let target = Target::FunPtr(fptr);
        self.push_insn(Insn::CCall { opnds, target, out: Opnd::None })
    }

    pub fn cmp(&mut self, left: Opnd, right: Opnd) {
        self.push_insn(Insn::Cmp { left, right });
    }

    /// Add a comment at the current position
    pub fn comment(&mut self, text: &str) {
        self.push_insn(Insn::Comment(text.to_string()));
    }

    pub fn cpop(&mut self) -> Opnd {
        self.push_insn(Insn::CPop { out: Opnd::None })
    }

    pub fn cpop_all(&mut self) {
        self.push_insn(Insn::CPopAll);
    }

    pub fn cpop_into(&mut self, opnd: Opnd) {
        self.push_insn(Insn::CPopInto(opnd));
    }

    pub fn cpush(&mut self, opnd: Opnd) {
        self.push_insn(Insn::CPush(opnd));
    }

    pub fn cpush_all(&mut self) {
        self.push_insn(Insn::CPushAll);
    }

    pub fn cret(&mut self, opnd: Opnd) {
        self.push_insn(Insn::CRet(opnd));
    }

    #[must_use]
    pub fn csel_e(&mut self, truthy: Opnd, falsy: Opnd) -> Opnd {
        self.push_insn(Insn::CSelE { truthy, falsy, out: Opnd::None })
    }

    #[must_use]
    pub fn csel_g(&mut self, truthy: Opnd, falsy: Opnd) -> Opnd {
        self.push_insn(Insn::CSelG { truthy, falsy, out: Opnd::None })
    }

    #[must_use]
    pub fn csel_ge(&mut self, truthy: Opnd, falsy: Opnd) -> Opnd {
        self.push_insn(Insn::CSelGE { truthy, falsy, out: Opnd::None })
    }

    #[must_use]
    pub fn csel_l(&mut self, truthy: Opnd, falsy: Opnd) -> Opnd {
        self.push_insn(Insn::CSelL { truthy, falsy, out: Opnd::None })
    }

    #[must_use]
    pub fn csel_le(&mut self, truthy: Opnd, falsy: Opnd) -> Opnd {
        self.push_insn(Insn::CSelLE { truthy, falsy, out: Opnd::None })
    }

    #[must_use]
    pub fn csel_ne(&mut self, truthy: Opnd, falsy: Opnd) -> Opnd {
        self.push_insn(Insn::CSelNE { truthy, falsy, out: Opnd::None })
    }

    #[must_use]
    pub fn csel_nz(&mut self, truthy: Opnd, falsy: Opnd) -> Opnd {
        self.push_insn(Insn::CSelNZ { truthy, falsy, out: Opnd::None })
    }

    #[must_use]
    pub fn csel_z(&mut self, truthy: Opnd, falsy: Opnd) -> Opnd {
        self.push_insn(Insn::CSelZ { truthy, falsy, out: Opnd::None })
    }

    pub fn frame_setup(&mut self) {
        self.push_insn(Insn::FrameSetup);
    }

    pub fn frame_teardown(&mut self) {
        self.push_insn(Insn::FrameTeardown);
    }

    pub fn incr_counter(&mut self, mem: Opnd, value: Opnd) {
        self.push_insn(Insn::IncrCounter { mem, value });
    }

    pub fn jbe(&mut self, target: Target) {
        self.push_insn(Insn::Jbe(target));
    }

    pub fn je(&mut self, target: Target) {
        self.push_insn(Insn::Je(target));
    }

    pub fn jmp(&mut self, target: Target) {
        self.push_insn(Insn::Jmp(target));
    }

    pub fn jmp_opnd(&mut self, opnd: Opnd) {
        self.push_insn(Insn::JmpOpnd(opnd));
    }

    pub fn jne(&mut self, target: Target) {
        self.push_insn(Insn::Jne(target));
    }

    pub fn jnz(&mut self, target: Target) {
        self.push_insn(Insn::Jnz(target));
    }

    pub fn jo(&mut self, target: Target) {
        self.push_insn(Insn::Jo(target));
    }

    pub fn jz(&mut self, target: Target) {
        self.push_insn(Insn::Jz(target));
    }

    #[must_use]
    pub fn lea(&mut self, opnd: Opnd) -> Opnd {
        self.push_insn(Insn::Lea { opnd, out: Opnd::None })
    }

    /// Load an address relative to the given label.
    #[must_use]
    pub fn lea_label(&mut self, target: Target) -> Opnd {
        self.push_insn(Insn::LeaLabel { target, out: Opnd::None })
    }

    #[must_use]
    pub fn live_reg_opnd(&mut self, opnd: Opnd) -> Opnd {
        self.push_insn(Insn::LiveReg { opnd, out: Opnd::None })
    }

    #[must_use]
    pub fn load(&mut self, opnd: Opnd) -> Opnd {
        self.push_insn(Insn::Load { opnd, out: Opnd::None })
    }

    #[must_use]
    pub fn load_sext(&mut self, opnd: Opnd) -> Opnd {
        self.push_insn(Insn::LoadSExt { opnd, out: Opnd::None })
    }

    #[must_use]
    pub fn lshift(&mut self, opnd: Opnd, shift: Opnd) -> Opnd {
        self.push_insn(Insn::LShift { opnd, shift, out: Opnd::None })
    }

    pub fn mov(&mut self, dest: Opnd, src: Opnd) {
        self.push_insn(Insn::Mov { dest, src });
    }

    pub fn not(&mut self, opnd: Opnd) -> Opnd {
        self.push_insn(Insn::Not { opnd, out: Opnd::None })
    }

    #[must_use]
    pub fn or(&mut self, left: Opnd, right: Opnd) -> Opnd {
        self.push_insn(Insn::Or { left, right, out: Opnd::None })
    }

    //pub fn pos_marker<F: FnMut(CodePtr)>(&mut self, marker_fn: F)
    pub fn pos_marker(&mut self, marker_fn: impl Fn(CodePtr) + 'static) {
        self.push_insn(Insn::PosMarker(Box::new(marker_fn)));
    }

    #[must_use]
    pub fn rshift(&mut self, opnd: Opnd, shift: Opnd) -> Opnd {
        self.push_insn(Insn::RShift { opnd, shift, out: Opnd::None })
    }

    pub fn store(&mut self, dest: Opnd, src: Opnd) {
        self.push_insn(Insn::Store { dest, src });
    }

    #[must_use]
    pub fn sub(&mut self, left: Opnd, right: Opnd) -> Opnd {
        self.push_insn(Insn::Sub { left, right, out: Opnd::None })
    }

    pub fn test(&mut self, left: Opnd, right: Opnd) {
        self.push_insn(Insn::Test { left, right });
    }

    #[must_use]
    pub fn urshift(&mut self, opnd: Opnd, shift: Opnd) -> Opnd {
        self.push_insn(Insn::URShift { opnd, shift, out: Opnd::None })
    }
}
