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
            Opnd::InsnOut { .. } => panic!("InsnOut operand made it past register allocation"),
            Opnd::UImm(value) => A64Opnd::new_uimm(value),
            Opnd::Imm(value) => A64Opnd::new_imm(value),
            Opnd::Reg(reg) => A64Opnd::Reg(reg),
            Opnd::Mem(Mem { base: MemBase::Reg(reg_no), num_bits, disp }) => {
                A64Opnd::new_mem(num_bits, A64Opnd::Reg(A64Reg { num_bits, reg_no }), disp)
            }
            _ => panic!("unsupported arm64 operand type")
        }
    }
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

                    asm.push_insn(op, new_opnds, target);
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
                        _ => panic!("Unsupported operand type")
                    };

                    asm.push_insn(op, vec![opnds[0], value], target);
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
        fn emit_jump(cb: &mut CodeBlock, src_addr: i64, dst_addr: i64) {
            let offset = dst_addr - src_addr;

            if offset < 128 * 1024 * 1024 && offset >= -128 * 1024 * 1024 {
                // Encode how far we're jumping in # of instructions
                bl(cb, A64Opnd::new_imm(offset / 4));
            } else {
                // Since this is too far to jump directly, we'll load
                // the value into a register and jump to it
                mov(cb, X30, A64Opnd::new_uimm(src_addr as u64));
                mov(cb, X29, A64Opnd::new_uimm(dst_addr as u64));
                br(cb, X29);
            }
        }

        /// Emit a conditional jump instruction from the current address to a
        /// target address.
        fn emit_conditional_jump(cb: &mut CodeBlock, condition: Condition, dst_ptr: CodePtr) {
            let src_addr = cb.get_write_ptr().into_i64() + 4;
            let dst_addr = dst_ptr.into_i64();
            bcond(cb, condition, A64Opnd::new_imm(dst_addr - src_addr));
        }

        // NOTE: dear Kevin,
        // for arm, you may want to reserve 1 or 2 caller-save registers
        // to use as scracth registers (during the last phase of the codegen)
        // These registers will not be allocated to anything by the register
        // allocator, they're just useful because arm is slightly trickier
        // than x86 to generate code for.
        // For example, if you want to jump far away, you may want to store
        // the jump target address in a register first.

        //dbg!(&self.insns);

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
                    // This assumes only load instructions can contain references to GC'd Value operands
                    // mov(cb, insn.out.into(), insn.opnds[0].into());

                    // // If the value being loaded is a heap object
                    // if let Opnd::Value(val) = insn.opnds[0] {
                    //     if !val.special_const_p() {
                    //         // The pointer immediate is encoded as the last part of the mov written out
                    //         let ptr_offset: u32 = (cb.get_write_pos() as u32) - (SIZEOF_VALUE as u32);
                    //         gc_offsets.push(ptr_offset);
                    //     }
                    // }
                    todo!();
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

                    // Calculate how far we're jumping in # of bytes
                    let src_addr = cb.get_write_ptr().into_i64() + 4;
                    let dst_addr = insn.target.unwrap().unwrap_fun_ptr() as i64;
                    emit_jump(cb, src_addr, dst_addr);
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
                    match insn.target.unwrap() {
                        Target::CodePtr(dst_ptr) => {
                            let src_addr = cb.get_write_ptr().into_i64() + 4;
                            let dst_addr = dst_ptr.into_i64();
                            emit_jump(cb, src_addr, dst_addr);
                        },
                        Target::Label(label_idx) => {
                            // We're going to write a label reference that knows
                            // when it writes the actual offset it needs to
                            // shift itself a couple of bits in to maintain the
                            // correct encoding.
                            cb.label_ref_with_shift(label_idx, Some(32 - 26));

                            // Here we're going to write a branch instruction
                            // with a 0 offset so that later it can be patched
                            // when we link all of the labels together.
                            bl(cb, A64Opnd::new_uimm(0));
                        },
                        Target::FunPtr(_) => unreachable!()
                    };
                },
                Op::Je => {
                    match insn.target.unwrap() {
                        Target::CodePtr(dst_ptr) => {
                            emit_conditional_jump(cb, Condition::EQ, dst_ptr);
                        },
                        Target::Label(_) => todo!(),
                        _ => unreachable!()
                    };
                },
                Op::Jbe => {
                    match insn.target.unwrap() {
                        Target::CodePtr(_) => todo!(),
                        Target::Label(_) => todo!(),
                        _ => unreachable!()
                    };
                },
                Op::Jz => {
                    match insn.target.unwrap() {
                        Target::CodePtr(_) => todo!(),
                        Target::Label(_) => todo!(),
                        _ => unreachable!()
                    };
                },
                Op::Jnz => {
                    match insn.target.unwrap() {
                        Target::CodePtr(_) => todo!(),
                        Target::Label(_) => todo!(),
                        _ => unreachable!()
                    };
                },
                Op::Jo => {
                    match insn.target.unwrap() {
                        Target::CodePtr(dst_ptr) => {
                            emit_conditional_jump(cb, Condition::VS, dst_ptr);
                        },
                        Target::Label(_) => todo!(),
                        _ => unreachable!()
                    };
                },
                Op::IncrCounter => {
                    ldaddal(cb, insn.opnds[0].into(), insn.opnds[0].into(), insn.opnds[1].into())
                },
                Op::Breakpoint => {
                    brk(cb, A64Opnd::None)
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
