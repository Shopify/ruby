#![cfg(test)]

use crate::asm::x86_64::*;
use std::fmt;

/// Produce hex string output from the bytes in a code block
impl<'a> fmt::LowerHex for super::CodeBlock {
    fn fmt(&self, fmtr: &mut fmt::Formatter) -> fmt::Result {
        for byte in 0..self.write_pos {
            fmtr.write_fmt(format_args!("{:02x}", self.mem_block[byte]))?;
        }
        Ok(())
    }
}

/// Check that the bytes for an instruction sequence match a hex string
fn check_bytes<R>(bytes: &str, run: R) where R: FnOnce(&mut super::CodeBlock) {
    let mut cb = super::CodeBlock::new();
    run(&mut cb);
    assert_eq!(format!("{:x}", cb), bytes);
}

#[test]
fn test_add() {
    check_bytes("80c103", |cb| add(cb, CL, imm_opnd(3)));
    check_bytes("00d9", |cb| add(cb, CL, BL));
    check_bytes("4000e1", |cb| add(cb, CL, SPL));
    check_bytes("6601d9", |cb| add(cb, CX, BX));
    check_bytes("4801d8", |cb| add(cb, RAX, RBX));
    check_bytes("01d1", |cb| add(cb, ECX, EDX));
    check_bytes("4c01f2", |cb| add(cb, RDX, R14));
    check_bytes("480110", |cb| add(cb, mem_opnd(64, RAX, 0), RDX));
    check_bytes("480310", |cb| add(cb, RDX, mem_opnd(64, RAX, 0)));
    check_bytes("48035008", |cb| add(cb, RDX, mem_opnd(64, RAX, 8)));
    check_bytes("480390ff000000", |cb| add(cb, RDX, mem_opnd(64, RAX, 255)));
    check_bytes("4881407fff000000", |cb| add(cb, mem_opnd(64, RAX, 127), imm_opnd(255)));
    check_bytes("0110", |cb| add(cb, mem_opnd(32, RAX, 0), EDX));
    check_bytes("4883c408", |cb| add(cb, RSP, imm_opnd(8)));
    check_bytes("83c108", |cb| add(cb, ECX, imm_opnd(8)));
    check_bytes("81c1ff000000", |cb| add(cb, ECX, imm_opnd(255)));
}

#[test]
fn test_and() {
    check_bytes("4421e5", |cb| and(cb, EBP, R12D));
    check_bytes("48832008", |cb| and(cb, mem_opnd(64, RAX, 0), imm_opnd(0x08)));
}

#[test]
fn test_call_label() {
    check_bytes("e8fbffffff", |cb| {
        let label_idx = cb.new_label("fn".to_owned());
        call_label(cb, label_idx);
        cb.link_labels();
    });
}

#[test]
fn test_call_reg() {
    check_bytes("ffd0", |cb| call(cb, RAX));
}

#[test]
fn test_call_mem() {
    check_bytes("ff542408", |cb| call(cb, mem_opnd(64, RSP, 8)));
}

#[test]
fn test_cmovcc() {
    check_bytes("0f4ff7", |cb| cmovg(cb, ESI, EDI));
    check_bytes("0f4f750c", |cb| cmovg(cb, ESI, mem_opnd(32, RBP, 12)));
    check_bytes("0f4cc1", |cb| cmovl(cb, EAX, ECX));
    check_bytes("480f4cdd", |cb| cmovl(cb, RBX, RBP));
    check_bytes("0f4e742404", |cb| cmovle(cb, ESI, mem_opnd(32, RSP, 4)));
}

#[test]
fn test_cmp() {
    check_bytes("38d1", |cb| cmp(cb, CL, DL));
    check_bytes("39f9", |cb| cmp(cb, ECX, EDI));
    check_bytes("493b1424", |cb| cmp(cb, RDX, mem_opnd(64, R12, 0)));
    check_bytes("4883f802", |cb| cmp(cb, RAX, imm_opnd(2)));
}

#[test]
fn test_cqo() {
    check_bytes("4899", |cb| cqo(cb));
}

#[test]
fn test_jge_label() {
    check_bytes("0f8dfaffffff", |cb| {
        let label_idx = cb.new_label("loop".to_owned());
        jge_label(cb, label_idx);
        cb.link_labels();
    });
}

#[test]
fn test_jmp_label() {
    check_bytes("e9fbffffff", |cb| {
        let label_idx = cb.new_label("loop".to_owned());
        jmp_label(cb, label_idx);
        cb.link_labels();
    });
}

#[test]
fn test_jmp_rm() {
    check_bytes("41ffe4", |cb| jmp_rm(cb, R12));
}

#[test]
fn test_jo_label() {
    check_bytes("0f80faffffff", |cb| {
        let label_idx = cb.new_label("loop".to_owned());
        jo_label(cb, label_idx);
        cb.link_labels();
    });
}

#[test]
fn test_lea() {
    check_bytes("488d5108", |cb| lea(cb, RDX, mem_opnd(64, RCX, 8)));

    // TODO(kevin)
    // check_bytes("488d0500000000", |cb| lea(cb, RAX, mem_opnd(8, RIP, 0)));
    // check_bytes("488d0505000000", |cb| lea(cb, RAX, mem_opnd(8, RIP, 5)));
    // check_bytes("488d3d05000000", |cb| lea(cb, RDI, mem_opnd(8, RIP, 5)));
}

#[test]
fn test_mov() {
    check_bytes("b807000000", |cb| mov(cb, EAX, imm_opnd(7)));
    check_bytes("b8fdffffff", |cb| mov(cb, EAX, imm_opnd(-3)));
    check_bytes("41bf03000000", |cb| mov(cb, R15, imm_opnd(3)));
    check_bytes("89d8", |cb| mov(cb, EAX, EBX));
    check_bytes("89c8", |cb| mov(cb, EAX, ECX));
    check_bytes("8b9380000000", |cb| mov(cb, EDX, mem_opnd(32, RBX, 128)));
    check_bytes("488b442404", |cb| mov(cb, RAX, mem_opnd(64, RSP, 4)));

    // Test `mov rax, 3` => `mov eax, 3` optimization
    check_bytes("41b834000000", |cb| mov(cb, R8, imm_opnd(0x34)));
    check_bytes("49b80000008000000000", |cb| mov(cb, R8, imm_opnd(0x80000000)));
    check_bytes("49b8ffffffffffffffff", |cb| mov(cb, R8, imm_opnd(-1)));

    check_bytes("b834000000", |cb| mov(cb, RAX, imm_opnd(0x34)));
    check_bytes("48b80000008000000000", |cb| mov(cb, RAX, imm_opnd(0x80000000)));
    check_bytes("48b8ccffffffffffffff", |cb| mov(cb, RAX, imm_opnd(-52))); // yasm thinks this could use a dword immediate instead of qword
    check_bytes("48b8ffffffffffffffff", |cb| mov(cb, RAX, imm_opnd(-1))); // yasm thinks this could use a dword immediate instead of qword
    check_bytes("4488c9", |cb| mov(cb, CL, R9B));
    check_bytes("4889c3", |cb| mov(cb, RBX, RAX));
    check_bytes("4889df", |cb| mov(cb, RDI, RBX));
    check_bytes("40b60b", |cb| mov(cb, SIL, imm_opnd(11)));

    check_bytes("c60424fd", |cb| mov(cb, mem_opnd(8, RSP, 0), imm_opnd(-3)));
    check_bytes("48c7470801000000", |cb| mov(cb, mem_opnd(64, RDI, 8), imm_opnd(1)));
    //check_bytes("67c7400411000000", |cb| mov(cb, mem_opnd(32, EAX, 4), imm_opnd(0x34))); // We don't distinguish between EAX and RAX here - that's probably fine?
    check_bytes("c7400411000000", |cb| mov(cb, mem_opnd(32, RAX, 4), imm_opnd(17)));
    check_bytes("41895814", |cb| mov(cb, mem_opnd(32, R8, 20), EBX));
    check_bytes("4d8913", |cb| mov(cb, mem_opnd(64, R11, 0), R10));
    check_bytes("48c742f8f4ffffff", |cb| mov(cb, mem_opnd(64, RDX, -8), imm_opnd(-12)));
}

#[test]
fn test_mov_unsigned() {
    // MOV AL, moffs8
    check_bytes("b001", |cb| mov(cb, AL, uimm_opnd(1)));
    check_bytes("b0ff", |cb| mov(cb, AL, uimm_opnd(u8::MAX.into())));

    // MOV AX, moffs16
    check_bytes("66b80100", |cb| mov(cb, AX, uimm_opnd(1)));
    check_bytes("66b8ffff", |cb| mov(cb, AX, uimm_opnd(u16::MAX.into())));

    // MOV EAX, moffs32
    check_bytes("b801000000", |cb| mov(cb, EAX, uimm_opnd(1)));
    check_bytes("b8ffffffff", |cb| mov(cb, EAX, uimm_opnd(u32::MAX.into())));

    // MOV RAX, moffs64, will move down into EAX since it fits into 32 bits
    check_bytes("b801000000", |cb| mov(cb, RAX, uimm_opnd(1)));
    check_bytes("b8ffffffff", |cb| mov(cb, RAX, uimm_opnd(u32::MAX.into())));

    // MOV RAX, moffs64, will not move down into EAX since it does not fit into 32 bits
    check_bytes("48b80000000001000000", |cb| mov(cb, RAX, uimm_opnd(u32::MAX as u64 + 1)));
    check_bytes("48b8ffffffffffffffff", |cb| mov(cb, RAX, uimm_opnd(u64::MAX.into())));

    // MOV r8, imm8
    check_bytes("41b001", |cb| mov(cb, R8B, uimm_opnd(1)));
    check_bytes("41b0ff", |cb| mov(cb, R8B, uimm_opnd(u8::MAX.into())));

    // MOV r16, imm16
    check_bytes("6641b80100", |cb| mov(cb, R8W, uimm_opnd(1)));
    check_bytes("6641b8ffff", |cb| mov(cb, R8W, uimm_opnd(u16::MAX.into())));

    // MOV r32, imm32
    check_bytes("41b801000000", |cb| mov(cb, R8D, uimm_opnd(1)));
    check_bytes("41b8ffffffff", |cb| mov(cb, R8D, uimm_opnd(u32::MAX.into())));

    // MOV r64, imm64, will move down into 32 bit since it fits into 32 bits
    check_bytes("41b801000000", |cb| mov(cb, R8, uimm_opnd(1)));

    // MOV r64, imm64, will not move down into 32 bit since it does not fit into 32 bits
    check_bytes("49b8ffffffffffffffff", |cb| mov(cb, R8, uimm_opnd(u64::MAX)));
}

#[test]
fn test_movsx() {
    check_bytes("660fbec0", |cb| movsx(cb, AX, AL));
    check_bytes("0fbed0", |cb| movsx(cb, EDX, AL));
    check_bytes("480fbec3", |cb| movsx(cb, RAX, BL));
    check_bytes("0fbfc8", |cb| movsx(cb, ECX, AX));
    check_bytes("4c0fbed9", |cb| movsx(cb, R11, CL));
    check_bytes("4c6354240c", |cb| movsx(cb, R10, mem_opnd(32, RSP, 12)));
    check_bytes("480fbe0424", |cb| movsx(cb, RAX, mem_opnd(8, RSP, 0)));
    check_bytes("490fbf5504", |cb| movsx(cb, RDX, mem_opnd(16, R13, 4)));
}

#[test]
fn test_nop() {
    check_bytes("90", |cb| nop(cb, 1));
    // TODO: we should test some multibyte nop encodings
}

#[test]
fn test_not() {
    check_bytes("66f7d0", |cb| not(cb, AX));
    check_bytes("f7d0", |cb| not(cb, EAX));
    check_bytes("49f71424", |cb| not(cb, mem_opnd(64, R12, 0)));
    check_bytes("f794242d010000", |cb| not(cb, mem_opnd(32, RSP, 301)));
    check_bytes("f71424", |cb| not(cb, mem_opnd(32, RSP, 0)));
    check_bytes("f7542403", |cb| not(cb, mem_opnd(32, RSP, 3)));
    check_bytes("f75500", |cb| not(cb, mem_opnd(32, RBP, 0)));
    check_bytes("f7550d", |cb| not(cb, mem_opnd(32, RBP, 13)));
    check_bytes("48f7d0", |cb| not(cb, RAX));
    check_bytes("49f7d3", |cb| not(cb, R11));
    check_bytes("f710", |cb| not(cb, mem_opnd(32, RAX, 0)));
    check_bytes("f716", |cb| not(cb, mem_opnd(32, RSI, 0)));
    check_bytes("f717", |cb| not(cb, mem_opnd(32, RDI, 0)));
    check_bytes("f75237", |cb| not(cb, mem_opnd(32, RDX, 55)));
    check_bytes("f79239050000", |cb| not(cb, mem_opnd(32, RDX, 1337)));
    check_bytes("f752c9", |cb| not(cb, mem_opnd(32, RDX, -55)));
    check_bytes("f792d5fdffff", |cb| not(cb, mem_opnd(32, RDX, -555)));
}

#[test]
fn test_or() {
    check_bytes("09f2", |cb| or(cb, EDX, ESI));
}

#[test]
fn test_pop() {
    check_bytes("58", |cb| pop(cb, RAX));
    check_bytes("5b", |cb| pop(cb, RBX));
    check_bytes("5c", |cb| pop(cb, RSP));
    check_bytes("5d", |cb| pop(cb, RBP));
    check_bytes("415c", |cb| pop(cb, R12));
    check_bytes("8f00", |cb| pop(cb, mem_opnd(64, RAX, 0)));
    check_bytes("418f00", |cb| pop(cb, mem_opnd(64, R8, 0)));
    check_bytes("418f4003", |cb| pop(cb, mem_opnd(64, R8, 3)));
    check_bytes("8f44c803", |cb| pop(cb, mem_opnd_sib(64, RAX, RCX, 8, 3)));
    check_bytes("418f44c803", |cb| pop(cb, mem_opnd_sib(64, R8, RCX, 8, 3)));
}

#[test]
fn test_push() {
    check_bytes("50", |cb| push(cb, RAX));
    check_bytes("53", |cb| push(cb, RBX));
    check_bytes("4154", |cb| push(cb, R12));
    check_bytes("ff30", |cb| push(cb, mem_opnd(64, RAX, 0)));
    check_bytes("41ff30", |cb| push(cb, mem_opnd(64, R8, 0)));
    check_bytes("41ff7003", |cb| push(cb, mem_opnd(64, R8, 3)));
    check_bytes("ff74c803", |cb| push(cb, mem_opnd_sib(64, RAX, RCX, 8, 3)));
    check_bytes("41ff74c803", |cb| push(cb, mem_opnd_sib(64, R8, RCX, 8, 3)));
}

#[test]
fn test_ret() {
    check_bytes("c3", |cb| ret(cb));
}

#[test]
fn test_sal() {
    check_bytes("66d1e1", |cb| sal(cb, CX, imm_opnd(1)));
    check_bytes("d1e1", |cb| sal(cb, ECX, imm_opnd(1)));
    check_bytes("c1e505", |cb| sal(cb, EBP, imm_opnd(5)));
    check_bytes("d1642444", |cb| sal(cb, mem_opnd(32, RSP, 68), imm_opnd(1)));
}

#[test]
fn test_sar() {
    check_bytes("d1fa", |cb| sar(cb, EDX, imm_opnd(1)));
}

#[test]
fn test_shr() {
    check_bytes("49c1ee07", |cb| shr(cb, R14, imm_opnd(7)));
}

#[test]
fn test_sub() {
    check_bytes("83e801", |cb| sub(cb, EAX, imm_opnd(1)));
    check_bytes("4883e802", |cb| sub(cb, RAX, imm_opnd(2)));
}

#[test]
fn test_test() {
    check_bytes("84c0", |cb| test(cb, AL, AL));
    check_bytes("6685c0", |cb| test(cb, AX, AX));
    check_bytes("f6c108", |cb| test(cb, CL, uimm_opnd(8)));
    check_bytes("f6c207", |cb| test(cb, DL, uimm_opnd(7)));
    check_bytes("f6c108", |cb| test(cb, RCX, uimm_opnd(8)));
    check_bytes("f6420808", |cb| test(cb, mem_opnd(8, RDX, 8), uimm_opnd(8)));
    check_bytes("f64208ff", |cb| test(cb, mem_opnd(8, RDX, 8), uimm_opnd(255)));
    check_bytes("66f7c2ffff", |cb| test(cb, DX, uimm_opnd(0xffff)));
    check_bytes("66f74208ffff", |cb| test(cb, mem_opnd(16, RDX, 8), uimm_opnd(0xffff)));
    check_bytes("f60601", |cb| test(cb, mem_opnd(8, RSI, 0), uimm_opnd(1)));
    check_bytes("f6461001", |cb| test(cb, mem_opnd(8, RSI, 16), uimm_opnd(1)));
    check_bytes("f646f001", |cb| test(cb, mem_opnd(8, RSI, -16), uimm_opnd(1)));
    check_bytes("854640", |cb| test(cb, mem_opnd(32, RSI, 64), EAX));
    check_bytes("4885472a", |cb| test(cb, mem_opnd(64, RDI, 42), RAX));
    check_bytes("4885c0", |cb| test(cb, RAX, RAX));
    check_bytes("4885f0", |cb| test(cb, RAX, RSI));

    // TODO(kevin)
    // check_bytes("48f74640f7ffffff", |cb| test(cb, mem_opnd(64, RSI, 64), imm_opnd(!0x08)));
}

#[test]
fn test_xchg() {
    check_bytes("4891", |cb| xchg(cb, RAX, RCX));
    check_bytes("4995", |cb| xchg(cb, RAX, R13));
    check_bytes("4887d9", |cb| xchg(cb, RCX, RBX));
    check_bytes("4d87f9", |cb| xchg(cb, R9, R15));
}

#[test]
fn test_xor() {
    check_bytes("31c0", |cb| xor(cb, EAX, EAX));
}

#[test]
#[cfg(feature = "disassembly")]
fn basic_capstone_usage() -> Result<(), capstone::Error> {
    // Test drive Capstone with simple input
    extern crate capstone;
    use capstone::prelude::*;
    let cs = Capstone::new()
        .x86()
        .mode(arch::x86::ArchMode::Mode64)
        .syntax(arch::x86::ArchSyntax::Intel)
        .build()?;

    let insns = cs.disasm_all(&[0xCC], 0x1000)?;

    match insns.as_ref() {
        [insn] => {
            assert_eq!(Some("int3"), insn.mnemonic());
            Ok(())
        }
        _ => Err(capstone::Error::CustomError(
            "expected to disassemble to int3",
        )),
    }
}

/*
#[cfg(test)]
mod tests {
    use crate::asm::x64::*;

    impl Assembler {
        fn byte_string(&self) -> String {
            self.encoded()
                .into_iter()
                .map(|byte| format!("{:02x}", byte))
                .collect::<Vec<_>>()
                .join(" ")
        }
    }

    macro_rules! test_encoding {
        ($bytes:literal $($disasm:literal, $mnemonic:ident ($args:expr))+) => {{
            let mut asm = Assembler::new();

            $( asm.$mnemonic($args); )*

            assert_eq!($bytes, asm.byte_string());

            // In case we have a disassembler, compare against a disassembly expectation
            #[cfg(feature = "disassembly")]
            {
                extern crate capstone;
                use capstone::prelude::*;
                let cs = Capstone::new()
                    .x86()
                    .mode(arch::x86::ArchMode::Mode64)
                    .syntax(arch::x86::ArchSyntax::Intel)
                    .build()
                    .expect("Failed to create Capstone object");

                let insns = cs
                    .disasm_all(asm.encoded(), 0x1000)
                    .expect("Failed to disassemble");

                let mut insn_idx = 0;
                $(
                    match insns.as_ref().get(insn_idx).map(|insn| (insn.mnemonic(), insn.op_str())) {
                        Some((Some(mnemonic), op_str)) => {
                            let mut capstone_disasm = mnemonic.to_owned();
                            if let Some(op_str) = op_str {
                                capstone_disasm.push_str(" ");
                                capstone_disasm.push_str(op_str);
                            }
                            assert_eq!($disasm, capstone_disasm, "instruction_index={}", insn_idx);
                        },
                        _ => panic!("Failed to disassemble to a instruction at instruction_index={}", insn_idx),
                    };
                    insn_idx += 1;
                    let _ = insn_idx; // Address unused warning from the last iteration
                )*

            }
        }};
    }

    /*
    #[test]
    fn reg_to_reg_movs() {
        let mut asm = Assembler::new();

        // 64b
        asm.mov(RAX.into(), RBX.into());
        asm.mov(R8.into(), RBX.into());
        asm.mov(RDI.into(), R14.into());
        asm.mov(R13.into(), R15.into());

        // 32b
        asm.mov(EBP.into(), EDI.into());
        asm.mov(R8D.into(), EBX.into());
        asm.mov(EBP.into(), R9D.into());
        asm.mov(R8D.into(), R11D.into());

        // 16b (panics at the moment)
        // asm.mov(AX.into(), CX.into());

        let bytes = asm.byte_string();
        assert_eq!(
            "48 8b c3 4c 8b c3 49 8b fe 4d 8b ef 8b ef 44 8b c3 41 8b e9 45 8b c3",
            bytes
        );
    }

    #[test]
    fn sar() {
        let mut asm = Assembler::new();

        // 64b
        asm.sar(RAX.into(), 1.into());
        asm.sar(R9.into(), 1.into());

        // 32b
        asm.sar(RDI.into(), 1.into());
        asm.sar(R10D.into(), 1.into());

        // TODO: write panic tests

        assert_eq!("48 d1 f8 49 d1 f9 48 d1 ff 41 d1 fa", asm.byte_string());
    }
    */

    #[test]
    fn shl_and_sal() {
        test_encoding!(
            "48 c1 e0 02 48 d1 e1 49 d1 e7 49 c1 e3 03 49 d1 e4 49 c1 e4 02 49 d1 e5 49 c1 e5 03"
            "shl rax, 2",  shl((RAX, 2))
            "shl rcx, 1",  shl((RCX, 1))

            "shl r15, 1",  shl((R15, 1))
            "shl r11, 3",  shl((R11, 3))

            "shl r12, 1",  shl((R12, 1))
            "shl r12, 2",  shl((R12, 2))

            "shl r13, 1",  shl((R13, 1))
            "shl r13, 3",  shl((R13, 3))
        );
    }

    #[test]
    fn test() {
        // reg64, imm32
        test_encoding!(
            "48 a9 ff ff ff 7f 49 f7 c3 fe ca ab 0f 48 f7 c7 02 35 54 f0 49 f7 c0 ff ff ff ff"
            "test rax, 0x7fffffff",  test((RAX, i32::MAX))
            "test r11, 0xfabcafe",   test((R11, 0xFABCAFE))
            "test rdi, -0xfabcafe",  test((RDI, -0xFABCAFE))
            "test r8, -1",           test((R8, -1))
        );

        // reg32, imm32
        test_encoding!(
            "f7 c7 ff ff ff ff 41 f7 c1 fe ca ab 0f f7 c7 ef be ad de 41 f7 c1 ff ff ff ff"
            "test edi, 0xffffffff", test((EDI, u32::MAX))
            "test r9d, 0xfabcafe", test((R9D, 0xFABCAFE))
            "test edi, 0xdeadbeef", test((EDI, 0xDEADBEEF))
            "test r9d, 0xffffffff", test((R9D, u32::MAX))
        );

        // reg64, reg64
        test_encoding!(
            "48 85 d0 4c 85 d9 49 85 dc 4d 85 f7"
            "test rax, rdx", test((RAX, RDX))
            "test rcx, r11", test((RCX, R11))
            "test r12, rbx", test((R12, RBX))
            "test r15, r14", test((R15, R14))
        );

        // reg32, reg32
        test_encoding!(
            "85 d0 44 85 d9 41 85 dc 45 85 f7"
            "test eax, edx", test((EAX, EDX))
            "test ecx, r11d", test((ECX, R11D))
            "test r12d, ebx", test((R12D, EBX))
            "test r15d, r14d", test((R15D, R14D))
        )

        // TODO: write panic tests
    }

    #[test]
    fn test_rmm_r() {
        test_encoding!(
            "48 85 40 80"
            "test qword ptr [rax - 0x80], rax",
            test((mem64(RAX, i8::MIN.into()), RAX))
        );

        test_encoding!(
            "49 85 44 24 80"
            "test qword ptr [r12 - 0x80], rax",
            test((mem64(R12, i8::MIN.into()), RAX))
        );

        test_encoding!(
            "4d 85 6d 80"
            "test qword ptr [r13 - 0x80], r13",
            test((mem64(R13, i8::MIN.into()), R13))
        );

        // FIXME: Buggy encoding. These registers require a REX prefix.
        test_encoding!(
            "84 f0 84 f8 84 e8 84 e0"
            "test al, sil", test((AL, SIL))
            "test al, dil", test((AL, DIL))
            "test al, bpl", test((AL, BPL))
            "test al, spl", test((AL, SPL))
        );
    }

    #[test]
    fn test_with_memory() {
        test_encoding!(
            "48 f7 40 80 ff ff ff 7f"
            "test qword ptr [rax - 0x80], 0x7fffffff",
            test((mem64(RAX, i8::MIN.into()), i32::MAX))
        );

        test_encoding!(
            "49 f7 45 7f ff ff ff 7f"
            "test qword ptr [r13 + 0x7f], 0x7fffffff",
            test((mem64(R13, i8::MAX.into()), i32::MAX))
        );

        test_encoding!(
            "48 f7 44 24 80 00 00 00 80"
            "test qword ptr [rsp - 0x80], -0x80000000",
            test((mem64(RSP, i8::MIN.into()), i32::MIN))
        );

        // RSP, RBP, R12 and R13 are special because the lower part of their regiser id
        // are escape codes in the ModR/M byte.
        test_encoding!(
            "48 f7 04 24 ff ff ff 7f 49 f7 04 24 00 00 00 80 \
             48 f7 45 00 ff ff ff 7f 49 f7 45 00 00 00 00 80"

            "test qword ptr [rsp], 0x7fffffff", test((mem64(RSP, 0), i32::MAX))
            "test qword ptr [r12], -0x80000000", test((mem64(R12, 0), i32::MIN))

            "test qword ptr [rbp], 0x7fffffff", test((mem64(RBP, 0), i32::MAX))
            "test qword ptr [r13], -0x80000000", test((mem64(R13, 0), i32::MIN))
        );

        test_encoding!(
            "49 f7 84 24 80 00 00 00 01 00 00 00"
            "test qword ptr [r12 + 0x80], 1",
            test((mem64(R12, 1 + i32::from(i8::MAX)), 1))
        );

        test_encoding!(
            "48 f7 84 24 7f ff ff ff fe ca ab 0f"
            "test qword ptr [rsp - 0x81], 0xfabcafe",
            test((mem64(RSP, i32::from(i8::MIN) - 1), 0xfabcafe))
        );
    }

    #[test]
    fn push() {
        test_encoding!(
            "50 41 54 41 55"
            "push rax", push(RAX)
            "push r12", push(R12)
            "push r13", push(R13)
        );
    }

    #[test]
    fn pop() {
        test_encoding!(
            "58 41 5c 41 5d"
            "pop rax", pop(RAX)
            "pop r12", pop(R12)
            "pop r13", pop(R13)
        );
    }

    #[test]
    fn randoms() {
        test_encoding!(
            "ff e0 41 ff e0 ff 21 41 ff 63 f6"
            "jmp rax", jmp(RAX)
            "jmp r8", jmp(R8)
            "jmp qword ptr [rcx]", jmp(mem64(RCX, 0))
            "jmp qword ptr [r11 - 0xa]", jmp(mem64(R11, -0xa))
        );

        test_encoding!(
            "f6 d0 f7 d0 48 f7 d0 49 f7 d0"
            "not al", not(AL)
            "not eax", not(EAX)
            "not rax", not(RAX)
            "not r8", not(R8)
        );

        test_encoding!(
            "ff d0 41 ff d0 ff 11 41 ff 53 f6"
            "call rax", call(RAX)
            "call r8", call(R8)
            "call qword ptr [rcx]", call(mem64(RCX, 0))
            "call qword ptr [r11 - 0xa]", call(mem64(R11, -0xa))
        );
    }

    #[test]
    fn mov() {
        test_encoding!(
            "b0 00 b8 fe ca ab 0f 41 b8 fe ca ab 0f 48 b8 ff ff ff ff ff ff ff ff 49 bf 00 00 00 00 01 00 00 00"
            "mov al, 0", mov((AL, 0))
            "mov eax, 0xfabcafe", mov((EAX, 0xfabcafe))
            "mov r8d, 0xfabcafe", mov((R8D, 0xfabcafe))
            "movabs rax, 0xffffffffffffffff", mov((RAX, u64::MAX))
            "movabs r15, 0x100000000", mov((R15, u64::from(u32::MAX)+1))
        );
    }

    #[test]
    fn mov_load_8b() {
        test_encoding!(
            "45 88 bf 00 ff ff ff"

            "mov byte ptr [r15 - 0x100], r15b", mov((mem8(R15, -0x100), R15B))
        );
    }

    #[test]
    #[cfg(feature = "disassembly")]
    fn basic_capstone_usage() -> Result<(), capstone::Error> {
        // Test drive Capstone with simple input
        extern crate capstone;
        use capstone::prelude::*;
        let cs = Capstone::new()
            .x86()
            .mode(arch::x86::ArchMode::Mode64)
            .syntax(arch::x86::ArchSyntax::Intel)
            .build()?;

        let insns = cs.disasm_all(&[0xCC], 0x1000)?;

        match insns.as_ref() {
            [insn] => {
                assert_eq!(Some("int3"), insn.mnemonic());
                Ok(())
            }
            _ => Err(capstone::Error::CustomError(
                "expected to disassemble to int3",
            )),
        }
    }
}
*/
