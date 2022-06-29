#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

use crate::asm::{CodeBlock};
use crate::asm::arm64::*;
use crate::codegen::{JITState};
use crate::cruby::*;
use crate::backend::ir::*;

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

/// Map Opnd to A64Opnd
impl From<Opnd> for A64Opnd {
    fn from(opnd: Opnd) -> Self {
        match opnd {
            Opnd::UImm(value) => A64Opnd::new_uimm(value),
            Opnd::Imm(value) => A64Opnd::new_imm(value),
            Opnd::Reg(reg) => A64Opnd::Reg(reg),
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
                    let new_opnds: Vec<Opnd> = opnds.into_iter().map(|opnd| {
                        match opnd {
                            Opnd::Reg(_) | Opnd::InsnOut { .. } => opnd,
                            Opnd::Mem(_) | Opnd::Imm(_) | Opnd::UImm(_) => asm.load(opnd),
                            _ => panic!("Unsupported operand type")
                        }
                    }).collect();

                    asm.push_insn(op, new_opnds, target);
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

                // Write the label at the current position
                Op::Label => {
                    cb.write_label(insn.target.unwrap().unwrap_label_idx());
                },

                Op::Add => {
                    add(cb, insn.opnds[0].into(), insn.opnds[0].into(), insn.opnds[1].into())
                },

                Op::Sub => {
                    sub(cb, insn.opnds[0].into(), insn.opnds[0].into(), insn.opnds[1].into())
                },

                Op::And => {
                    and(cb, insn.opnds[0].into(), insn.opnds[0].into(), insn.opnds[1].into())
                },

                Op::Not => {
                    // not(cb, insn.opnds[0].into())
                    todo!();
                },

                Op::Store => {
                    // mov(cb, insn.opnds[0].into(), insn.opnds[1].into())
                    todo!();
                },

                // This assumes only load instructions can contain references to GC'd Value operands
                Op::Load => {
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
                    // mov(cb, insn.opnds[0].into(), insn.opnds[1].into())
                    todo!();
                },

                // Load effective address
                Op::Lea => {
                    // lea(cb, insn.out.into(), insn.opnds[0].into())
                    todo!();
                },

                // Push and pop to the C stack
                Op::CPush => {
                    // push(cb, insn.opnds[0].into())
                    todo!();
                },

                Op::CPop => {
                    // pop(cb, insn.opnds[0].into())
                    todo!();
                },

                // C function call
                Op::CCall => {
                    // Temporary
                    // assert!(insn.opnds.len() < C_ARG_REGS.len());

                    // For each operand
                    // for (idx, opnd) in insn.opnds.iter().enumerate() {
                    //     mov(cb, C_ARG_REGS[idx], insn.opnds[idx].into());
                    // }

                    todo!();
                },

                Op::CRet => {
                    // TODO: bias allocation towards return register
                    // if insn.opnds[0] != Opnd::Reg(C_RET_REG) {
                    //     mov(cb, RAX, insn.opnds[0].into());
                    // }

                    // ret(cb);

                    todo!();
                }

                // Compare
                Op::Cmp => {
                    cmp(cb, insn.opnds[0].into(), insn.opnds[1].into())
                },

                // Test and set flags
                Op::Test => {
                    tst(cb, insn.opnds[0].into(), insn.opnds[1].into())
                },

                Op::JmpOpnd => {
                    // jmp_rm(cb, insn.opnds[0].into())
                    todo!();
                },

                // Conditional jump to a label
                Op::Jmp => {
                    // match insn.target.unwrap() {
                    //     Target::CodePtr(code_ptr) => jmp_ptr(cb, code_ptr),
                    //     Target::Label(label_idx) => jmp_label(cb, label_idx),
                    //     _ => unreachable!()
                    // }
                    todo!();
                }

                Op::Je => {
                    // match insn.target.unwrap() {
                    //     Target::CodePtr(code_ptr) => je_ptr(cb, code_ptr),
                    //     Target::Label(label_idx) => je_label(cb, label_idx),
                    //     _ => unreachable!()
                    // }
                    todo!();
                }

                Op::Jz => {
                    // match insn.target.unwrap() {
                    //     Target::CodePtr(code_ptr) => jz_ptr(cb, code_ptr),
                    //     Target::Label(label_idx) => jz_label(cb, label_idx),
                    //     _ => unreachable!()
                    // }
                    todo!();
                }

                Op::Jnz => {
                    // match insn.target.unwrap() {
                    //     Target::CodePtr(code_ptr) => jnz_ptr(cb, code_ptr),
                    //     Target::Label(label_idx) => jnz_label(cb, label_idx),
                    //     _ => unreachable!()
                    // }
                    todo!();
                }

                Op::Jo => {
                    // match insn.target.unwrap() {
                    //     Target::CodePtr(code_ptr) => jo_ptr(cb, code_ptr),
                    //     Target::Label(label_idx) => jo_label(cb, label_idx),
                    //     _ => unreachable!()
                    // }
                    todo!();
                }

                // Atomically increment a counter at a given memory location
                Op::IncrCounter => {
                    ldaddal(cb, insn.opnds[0].into(), insn.opnds[0].into(), insn.opnds[1].into())
                },

                Op::Breakpoint => {
                    // int3(cb)
                    todo!();
                },

                _ => panic!("unsupported instruction passed to arm64 backend: {:?}", insn.op)
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
