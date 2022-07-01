#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

use crate::asm::{CodeBlock};
use crate::asm::arm64::*;
use crate::codegen::{JITState};
use crate::cruby::*;
use crate::backend::ir::*;
use crate::virtualmem::CodePtr;

// Use the arm64 register type for this platform
pub type Reg = A64Reg;

// Callee-saved registers
pub const _CFP: Opnd = Opnd::Reg(X9_REG);
pub const _EC: Opnd = Opnd::Reg(X10_REG);
pub const _SP: Opnd = Opnd::Reg(X11_REG);

// C argument registers on this platform
pub const _C_ARG_OPNDS: [Opnd; 6] = [
    Opnd::Reg(X0_REG),
    Opnd::Reg(X1_REG),
    Opnd::Reg(X2_REG),
    Opnd::Reg(X3_REG),
    Opnd::Reg(X4_REG),
    Opnd::Reg(X5_REG)
];

// C return value register on this platform
pub const C_RET_REG: Reg = X0_REG;
pub const _C_RET_OPND: Opnd = Opnd::Reg(X0_REG);

// These constants define the way we work with Arm64's stack pointer. The stack
// pointer always needs to be aligned to a 16-byte boundary.
pub const SP_REG: A64Opnd = X31;
pub const SP_STEP: A64Opnd = A64Opnd::UImm(16);

/// Map Opnd to A64Opnd
impl From<Opnd> for A64Opnd {
    fn from(opnd: Opnd) -> Self {
        match opnd {
            Opnd::UImm(value) => A64Opnd::new_uimm(value),
            Opnd::Imm(value) => A64Opnd::new_imm(value),
            Opnd::Reg(reg) => A64Opnd::Reg(reg),
            Opnd::Mem(Mem { base: MemBase::Reg(reg_no), num_bits, disp }) => {
                A64Opnd::new_mem(num_bits, A64Opnd::Reg(A64Reg { num_bits, reg_no }), disp)
            },
            Opnd::Mem(Mem { base: MemBase::InsnOut(_), .. }) => {
                panic!("attempted to lower an Opnd::Mem with a MemBase::InsnOut base")
            },
            Opnd::InsnOut { .. } => panic!("attempted to lower an Opnd::InsnOut"),
            Opnd::None => panic!("attempted to lower an Opnd::None"),
            Opnd::Value(_) => panic!("attempted to lower an Opnd::Value"),
        }
    }
}

/// When we're writing out jumps to labels, first we skip past a certain number
/// of bytes. Then, when the offset between the label reference and the label
/// itself can be calculated (after we've emitted the rest of the code), we go
/// back in and patch the jump by reencoding the instruction.
///
/// On Arm64, if the jump is short enough we can use just one instruction and
/// use the offset as an immediate. If the jump is too long, we need to first
/// load it into a register, then use the branch register instruction. Similarly
/// with conditional jumps, if it's short enough we can use a b.cond
/// instruction, otherwise we need to load the destination into a register and
/// then do some conditional jumping around to get the jump to happen.
///
/// Because of this branching logic, you don't necessarily know how many
/// instructions are going to be encoded for any given jump. However, we _have_
/// to know this information if we're jumping to a label because we need to
/// write out a consistent number of instructions. In this case, we can write
/// however many nop instructions we need to pad the shorter clauses such that
/// the total number of instructions is consistent.
///
/// In cases where we're jumping to a known destination, we don't need to bother
/// padding anything. However, if we're jumping to a label, we will. This enum
/// represents those different cases.
enum ClausePadding {
    /// We're jumping to a label, so pad with necessary nop instructions.
    Padding,

    /// We're jumping to a known destination, so don't bother padding.
    NoPadding,
}

impl Assembler
{
    /// Get the list of registers from which we can allocate on this platform
    pub fn get_alloc_regs() -> Vec<Reg>
    {
        vec![
            X12_REG,
            X13_REG
        ]
    }

    /// Split platform-specific instructions
    /// The transformations done here are meant to make our lives simpler in later
    /// stages of the compilation pipeline.
    /// Here we may want to make sure that all instructions (except load and store)
    /// have no memory operands.
    fn arm64_split(mut self) -> Assembler
    {
        self.forward_pass(|asm, index, op, opnds, target| {
            match op {
                Op::Add | Op::Sub => {
                    // Check if one of the operands is a register. If it is,
                    // then we'll make that the first operand.
                    match (opnds[0], opnds[1]) {
                        (Opnd::Mem(_), Opnd::Mem(_)) => {
                            let opnd0 = asm.load(opnds[0]);
                            let opnd1 = asm.load(opnds[1]);
                            asm.push_insn(op, vec![opnd0, opnd1], target);
                        },
                        (mem_opnd @ Opnd::Mem(_), other_opnd) |
                        (other_opnd, mem_opnd @ Opnd::Mem(_)) => {
                            let opnd0 = asm.load(mem_opnd);
                            asm.push_insn(op, vec![opnd0, other_opnd], target);
                        },
                        _ => {
                            asm.push_insn(op, opnds, target);
                        }
                    }
                },
                Op::IncrCounter => {
                    // Every operand to the IncrCounter instruction need to be a
                    // register once it gets there. So here we're going to load
                    // anything that isn't a register first.
                    let new_opnds: Vec<Opnd> = opnds.into_iter().map(|opnd| {
                        match opnd {
                            Opnd::Mem(_) | Opnd::Imm(_) | Opnd::UImm(_) => asm.load(opnd),
                            _ => opnd,
                        }
                    }).collect();

                    asm.incr_counter(new_opnds[0], new_opnds[1]);
                },
                Op::Mov => {
                    // The value that is being moved must be either a register
                    // or an immediate that can be encoded as a bitmask
                    // immediate. Otherwise, we'll need to split the move into
                    // multiple instructions.
                    let value = match opnds[1] {
                        Opnd::Reg(_) | Opnd::InsnOut { .. } => opnds[1],
                        Opnd::Mem(_) | Opnd::Imm(_) => asm.load(opnds[1]),
                        Opnd::UImm(uimm) => {
                            if let Ok(encoded) = BitmaskImmediate::try_from(uimm) {
                                opnds[1]
                            } else {
                                asm.load(opnds[1])
                            }
                        },
                        _ => unreachable!()
                    };

                    /// If we're attempting to load into a memory operand, then
                    /// we'll switch over to the store instruction. Otherwise
                    /// we'll use the normal mov instruction.
                    match opnds[0] {
                        Opnd::Mem(_) => asm.store(opnds[0], value),
                        _ => asm.mov(opnds[0], value)
                    };
                },
                Op::Not => {
                    // The value that is being negated must be in a register, so
                    // if we get anything else we need to load it first.
                    let opnd0 = match opnds[0] {
                        Opnd::Mem(_) => asm.load(opnds[0]),
                        _ => opnds[0]
                    };

                    asm.not(opnd0);
                },
                Op::Store => {
                    // The value being stored must be in a register, so if it's
                    // not already one we'll load it first.
                    let opnd1 = match opnds[1] {
                        Opnd::Reg(_) | Opnd::InsnOut { .. } => opnds[1],
                        _ => asm.load(opnds[1])
                    };

                    asm.store(opnds[0], opnd1);
                },
                _ => {
                    asm.push_insn(op, opnds, target);
                }
            };
        })
    }

    /// Emit platform-specific machine code
    /// Returns a list of GC offsets
    pub fn arm64_emit(&mut self, cb: &mut CodeBlock) -> Vec<u32>
    {
        /// Emit a jump instruction to the given target address. If it's within
        /// the range where we can use the branch link instruction, then we'll
        /// use that. Otherwise, we'll load the address into a register and use
        /// the branch register instruction.
        fn emit_jump(cb: &mut CodeBlock, src_addr: i64, dst_addr: i64, padding: ClausePadding) {
            let offset = dst_addr - src_addr;

            if bl_offset_fits_bits(offset) {
                // Encode how far we're jumping in # of instructions
                bl(cb, A64Opnd::new_imm(offset / 4));

                // We want instructions that are using registers and
                // instructions that are using immediates to be the same size in
                // memory so that we can come back and patch without having to
                // shift stuff around. So here we'll add enough nop instructions
                // to make that happen. It works out well for us here since the
                // code would have jumped away already anyway.
                if matches!(padding, ClausePadding::Padding) {
                    nop(cb);
                    nop(cb);
                }
            } else {
                // Since this is too far to jump directly, we'll load
                // the value into a register and jump to it
                mov(cb, X30, A64Opnd::new_uimm(src_addr as u64));
                mov(cb, X29, A64Opnd::new_uimm(dst_addr as u64));
                br(cb, X29);
            }
        }

        /// Emit a conditional jump instruction to a specific target. This is
        /// called when lowering a direct jump instruction or when lowering a
        /// ccall instruction. It delegates appropriate work to the emit_jump
        /// function depending on the kind of target given.
        fn emit_target_jump(cb: &mut CodeBlock, target: Target) {
            match target {
                Target::CodePtr(dst_ptr) => {
                    let src_addr = cb.get_write_ptr().into_i64() + 4;
                    emit_jump(cb, src_addr, dst_ptr.into_i64(), ClausePadding::NoPadding);
                },
                Target::Label(label_idx) => {
                    cb.label_ref(label_idx, 12, |cb, src_addr, dst_addr| {
                        emit_jump(cb, src_addr, dst_addr, ClausePadding::Padding);
                    });
                },
                Target::FunPtr(fun_ptr) => {
                    let src_addr = cb.get_write_ptr().into_i64() + 4;
                    emit_jump(cb, src_addr, fun_ptr as i64, ClausePadding::NoPadding);
                }
            };
        }

        /// Emit a conditional jump instruction. If the offset between the
        /// instructions fits into the b.cond instruction, we'll use that.
        /// Otherwise, we'll use a combination of a b.cond, a direct jump
        /// forward, and loading the destination into a register and branching
        /// to it.
        fn emit_conditional_jump(cb: &mut CodeBlock, condition: Condition, src_addr: i64, dst_addr: i64, padding: ClausePadding) {
            let offset = dst_addr - src_addr;
            let fits_direct = bl_offset_fits_bits(offset);

            // If the jump offset fits into the conditional jump as an immediate
            // value and it's properly aligned, then we can use the b.cond
            // instruction directly. Otherwise, we need to load the address into
            // a register and use the branch register instruction.
            if bcond_offset_fits_bits(offset) {
                bcond(cb, condition, A64Opnd::new_imm(dst_addr - src_addr));

                // Sometimes we need to align both clauses of this conditional
                // because we're going to come back and patch the memory later.
                // In that case we'll add enough nop instructions that both
                // clauses are the same size in memory.
                if matches!(padding, ClausePadding::Padding) {
                    nop(cb);
                    nop(cb);
                    nop(cb);
                    nop(cb);
                }
            } else {
                // If the condition is met, then we'll skip past the next
                // instruction, put the address in a register, and jump to it.
                bcond(cb, condition, A64Opnd::new_imm(4));

                // If the offset fits into a direct jump, then we'll use that
                // and the number of instructions will be shorter. Otherwise
                // we'll use the branch register instruction.
                if fits_direct {
                    if matches!(padding, ClausePadding::Padding) {
                        // If we get to this instruction, then the condition
                        // wasn't met, in which case we'll jump past the next
                        // instruction that performs the direct jump.
                        bl(cb, A64Opnd::new_imm(12));

                        // Here we'll perform the direct jump to the target,
                        // then encode some nops to keep it aligned.
                        bl(cb, A64Opnd::new_imm(offset / 4));
                        nop(cb);
                        nop(cb);
                    } else {
                        // If we get to this instruction, then the condition
                        // wasn't met, in which case we'll jump past the next
                        // instruction that performs the direct jump.
                        bl(cb, A64Opnd::new_imm(4));

                        // Here we'll perform the direct jump to the target.
                        bl(cb, A64Opnd::new_imm(offset / 4));
                    }
                } else {
                    // If we get to this instruction, then the condition wasn't
                    // met, in which case we'll jump past the next instruction
                    // that perform the direct jump.
                    bl(cb, A64Opnd::new_imm(12));
                    mov(cb, X30, A64Opnd::new_uimm(src_addr as u64));
                    mov(cb, X29, A64Opnd::new_uimm(dst_addr as u64));
                    br(cb, X29);
                }
            }
        }

        /// Emit a conditional jump instruction to a specific target. This is
        /// called when lowering any of the conditional jump instructions. It
        /// delegates appropriate work to the emit_conditional_jump function
        /// depending on the kind of target given.
        fn emit_target_conditional_jump(cb: &mut CodeBlock, condition: Condition, target: Target) {
            match target {
                Target::CodePtr(dst_ptr) => {
                    let src_addr = cb.get_write_ptr().into_i64() + 4;
                    emit_conditional_jump(cb, condition, src_addr, dst_ptr.into_i64(), ClausePadding::NoPadding);
                },
                Target::Label(label_idx) => {
                    cb.label_ref(label_idx, 20, |cb, src_addr, dst_addr| {
                        emit_conditional_jump(cb, condition, src_addr, dst_addr, ClausePadding::Padding);
                    });
                },
                Target::FunPtr(_) => unreachable!()
            };
        }

        // dbg!(&self.insns);

        // List of GC offsets
        let mut gc_offsets: Vec<u32> = Vec::new();

        // For each instruction
        for insn in &self.insns {
            match insn.op {
                Op::Comment => {
                    if cfg!(feature = "asm_comments") {
                        cb.add_comment(&insn.text.as_ref().unwrap());
                    }
                },
                Op::Label => {
                    cb.write_label(insn.target.unwrap().unwrap_label_idx());
                },
                Op::Add => {
                    add(cb, insn.out.into(), insn.opnds[0].into(), insn.opnds[1].into());
                },
                Op::Sub => {
                    sub(cb, insn.out.into(), insn.opnds[0].into(), insn.opnds[1].into());
                },
                Op::And => {
                    and(cb, insn.out.into(), insn.opnds[0].into(), insn.opnds[1].into());
                },
                Op::Not => {
                    mvn(cb, insn.out.into(), insn.opnds[0].into());
                },
                Op::Store => {
                    // This order may be surprising but it is correct. The way
                    // the Arm64 assembler works, the register that is going to
                    // be stored is first and the address is second. However in
                    // our IR we have the address first and the register second.
                    stur(cb, insn.opnds[1].into(), insn.opnds[0].into());
                },
                Op::Load => {
                    mov(cb, insn.out.into(), insn.opnds[0].into());

                    // This assumes only load instructions can contain
                    // references to GC'd Value operands. If the value being
                    // loaded is a heap object, we'll report that back out to
                    // the gc_offsets list.
                    if let Opnd::Value(val) = insn.opnds[0] {
                        if !val.special_const_p() {
                            // The pointer immediate is encoded as the last part of the mov written out
                            let ptr_offset: u32 = (cb.get_write_pos() as u32) - (SIZEOF_VALUE as u32);
                            gc_offsets.push(ptr_offset);
                        }
                    }
                },
                Op::Mov => {
                    mov(cb, insn.opnds[0].into(), insn.opnds[1].into());
                },
                Op::Lea => {
                    ldur(cb, insn.out.into(), insn.opnds[0].into());
                },
                Op::CPush => {
                    add(cb, SP_REG, SP_REG, SP_STEP);
                    mov(cb, A64Opnd::new_mem(64, SP_REG, 0), insn.opnds[0].into());
                },
                Op::CPop => {
                    mov(cb, insn.out.into(), A64Opnd::new_mem(64, SP_REG, 0));
                    sub(cb, SP_REG, SP_REG, SP_STEP);
                },
                Op::CCall => {
                    // Temporary
                    assert!(insn.opnds.len() < C_ARG_REGS.len());

                    // For each operand
                    for (idx, opnd) in insn.opnds.iter().enumerate() {
                        mov(cb, C_ARG_REGS[idx], insn.opnds[idx].into());
                    }

                    emit_target_jump(cb, insn.target.unwrap());
                },
                Op::CRet => {
                    // TODO: bias allocation towards return register
                    if insn.opnds[0] != Opnd::Reg(C_RET_REG) {
                        mov(cb, C_RET_OPND.into(), insn.opnds[0].into());
                    }

                    ret(cb, A64Opnd::None);
                },
                Op::Cmp => {
                    cmp(cb, insn.opnds[0].into(), insn.opnds[1].into());
                },
                Op::Test => {
                    tst(cb, insn.opnds[0].into(), insn.opnds[1].into());
                },
                Op::JmpOpnd => {
                    br(cb, insn.opnds[0].into());
                },
                Op::Jmp => {
                    emit_target_jump(cb, insn.target.unwrap());
                },
                Op::Je => {
                    emit_target_conditional_jump(cb, Condition::EQ, insn.target.unwrap());
                },
                Op::Jbe => {
                    emit_target_conditional_jump(cb, Condition::LS, insn.target.unwrap());
                },
                Op::Jz => {
                    emit_target_conditional_jump(cb, Condition::EQ, insn.target.unwrap());
                },
                Op::Jnz => {
                    emit_target_conditional_jump(cb, Condition::NE, insn.target.unwrap());
                },
                Op::Jo => {
                    emit_target_conditional_jump(cb, Condition::VS, insn.target.unwrap());
                },
                Op::IncrCounter => {
                    ldaddal(cb, insn.opnds[0].into(), insn.opnds[0].into(), insn.opnds[1].into());
                },
                Op::Breakpoint => {
                    brk(cb, A64Opnd::None);
                }
            };
        }

        gc_offsets
    }

    /// Optimize and compile the stored instructions
    pub fn compile_with_regs(self, cb: &mut CodeBlock, regs: Vec<Reg>) -> Vec<u32>
    {
        let mut asm = self.arm64_split().split_loads().alloc_regs(regs);

        // Create label instances in the code block
        for (idx, name) in asm.label_names.iter().enumerate() {
            let label_idx = cb.new_label(name.to_string());
            assert!(label_idx == idx);
        }

        let gc_offsets = asm.arm64_emit(cb);
        cb.link_labels();

        gc_offsets
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn setup_asm() -> (Assembler, CodeBlock) {
        (Assembler::new(), CodeBlock::new_dummy(1024))
    }

    #[test]
    fn test_emit_add() {
        let (mut asm, mut cb) = setup_asm();

        let opnd = asm.add(Opnd::Reg(X0_REG), Opnd::Reg(X1_REG));
        asm.store(Opnd::mem(64, Opnd::Reg(X2_REG), 0), opnd);
        asm.compile_with_regs(&mut cb, vec![X3_REG]);

        let insns = cb.get_ptr(0).raw_ptr() as *const u32;
        assert_eq!(0x8b010003, unsafe { *insns });
    }
}
