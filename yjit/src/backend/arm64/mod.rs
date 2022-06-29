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
                    stur(cb, insn.opnds[0].into(), insn.opnds[1].into());
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
                    add(cb, X31, X31, A64Opnd::new_uimm(16));
                    mov(cb, A64Opnd::new_mem(64, X31, 0), insn.opnds[0].into());
                },
                Op::CPop => {
                    mov(cb, insn.out.into(), A64Opnd::new_mem(64, X31, 0));
                    sub(cb, X31, X31, A64Opnd::new_uimm(16));
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
                    let dst_addr = insn.target.unwrap().unwrap_fun_ptr();
                    let delta = (dst_addr as i64) - src_addr;

                    if delta < 128 * 1024 * 1024 && delta >= -128 * 1024 * 1024 {
                        // Encode how far we're jumping in # of instructions
                        bl(cb, A64Opnd::new_imm(delta / 4));
                    } else {
                        // Since this is too far to jump directly, we'll load
                        // the value into a register and jump to it
                        mov(cb, X30, A64Opnd::new_uimm(src_addr as u64));
                        mov(cb, X29, A64Opnd::new_uimm(dst_addr as u64));
                        br(cb, X29);
                    }
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
                    br(cb, insn.opnds[0].into())
                },
                Op::Jmp => {
                    // match insn.target.unwrap() {
                    //     Target::CodePtr(code_ptr) => jmp_ptr(cb, code_ptr),
                    //     Target::Label(label_idx) => jmp_label(cb, label_idx),
                    //     _ => unreachable!()
                    // }
                    todo!();
                },
                Op::Je => {
                    // match insn.target.unwrap() {
                    //     Target::CodePtr(code_ptr) => je_ptr(cb, code_ptr),
                    //     Target::Label(label_idx) => je_label(cb, label_idx),
                    //     _ => unreachable!()
                    // }
                    todo!();
                },
                Op::Jbe => {
                    todo!();
                },
                Op::Jz => {
                    // match insn.target.unwrap() {
                    //     Target::CodePtr(code_ptr) => jz_ptr(cb, code_ptr),
                    //     Target::Label(label_idx) => jz_label(cb, label_idx),
                    //     _ => unreachable!()
                    // }
                    todo!();
                },
                Op::Jnz => {
                    // match insn.target.unwrap() {
                    //     Target::CodePtr(code_ptr) => jnz_ptr(cb, code_ptr),
                    //     Target::Label(label_idx) => jnz_label(cb, label_idx),
                    //     _ => unreachable!()
                    // }
                    todo!();
                },
                Op::Jo => {
                    // match insn.target.unwrap() {
                    //     Target::CodePtr(code_ptr) => jo_ptr(cb, code_ptr),
                    //     Target::Label(label_idx) => jo_label(cb, label_idx),
                    //     _ => unreachable!()
                    // }
                    todo!();
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
