// Copyright (c) 2012, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.
//
// Ported from the Dart SDK's disassembler_x86.cc to Rust.
// X64 (long mode, 64-bit) only.

use core::fmt::Write;

// ---------------------------------------------------------------------------
// Operand ordering / size flags
// ---------------------------------------------------------------------------

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[repr(u8)]
enum OperandType {
    UnsetOpOrder = 0,
    RegOperOpOrder = 1,  // Register destination, operand source.
    OperRegOpOrder = 2,  // Operand destination, register source.
    ByteRegOperOpOrder = 5, // REG_OPER | BYTE_SIZE_OPERAND_FLAG
    ByteOperRegOpOrder = 6, // OPER_REG | BYTE_SIZE_OPERAND_FLAG
}

const BYTE_SIZE_OPERAND_FLAG: u8 = 4;

impl OperandType {
    fn has_byte_size(self) -> bool {
        (self as u8) & BYTE_SIZE_OPERAND_FLAG != 0
    }
    fn without_byte_flag(self) -> OperandType {
        match (self as u8) & !BYTE_SIZE_OPERAND_FLAG {
            1 => OperandType::RegOperOpOrder,
            2 => OperandType::OperRegOpOrder,
            _ => OperandType::UnsetOpOrder,
        }
    }
}

// ---------------------------------------------------------------------------
// ByteMnemonic tables
// ---------------------------------------------------------------------------

struct ByteMnemonic {
    b: i16, // -1 terminates
    op_order: OperandType,
    mnem: &'static str,
}

// ALU codes: add=0, or=1, adc=2, sbb=3, and=4, sub=5, xor=6, cmp=7
// For each ALU op with code `c`:
//   c*8+0 => BYTE_OPER_REG
//   c*8+1 => OPER_REG
//   c*8+2 => BYTE_REG_OPER
//   c*8+3 => REG_OPER
static TWO_OPERANDS_INSTR: &[ByteMnemonic] = &[
    // add (code=0)
    ByteMnemonic { b: 0x00, op_order: OperandType::ByteOperRegOpOrder, mnem: "add" },
    ByteMnemonic { b: 0x01, op_order: OperandType::OperRegOpOrder,     mnem: "add" },
    ByteMnemonic { b: 0x02, op_order: OperandType::ByteRegOperOpOrder, mnem: "add" },
    ByteMnemonic { b: 0x03, op_order: OperandType::RegOperOpOrder,     mnem: "add" },
    // or (code=1)
    ByteMnemonic { b: 0x08, op_order: OperandType::ByteOperRegOpOrder, mnem: "or" },
    ByteMnemonic { b: 0x09, op_order: OperandType::OperRegOpOrder,     mnem: "or" },
    ByteMnemonic { b: 0x0A, op_order: OperandType::ByteRegOperOpOrder, mnem: "or" },
    ByteMnemonic { b: 0x0B, op_order: OperandType::RegOperOpOrder,     mnem: "or" },
    // adc (code=2)
    ByteMnemonic { b: 0x10, op_order: OperandType::ByteOperRegOpOrder, mnem: "adc" },
    ByteMnemonic { b: 0x11, op_order: OperandType::OperRegOpOrder,     mnem: "adc" },
    ByteMnemonic { b: 0x12, op_order: OperandType::ByteRegOperOpOrder, mnem: "adc" },
    ByteMnemonic { b: 0x13, op_order: OperandType::RegOperOpOrder,     mnem: "adc" },
    // sbb (code=3)
    ByteMnemonic { b: 0x18, op_order: OperandType::ByteOperRegOpOrder, mnem: "sbb" },
    ByteMnemonic { b: 0x19, op_order: OperandType::OperRegOpOrder,     mnem: "sbb" },
    ByteMnemonic { b: 0x1A, op_order: OperandType::ByteRegOperOpOrder, mnem: "sbb" },
    ByteMnemonic { b: 0x1B, op_order: OperandType::RegOperOpOrder,     mnem: "sbb" },
    // and (code=4)
    ByteMnemonic { b: 0x20, op_order: OperandType::ByteOperRegOpOrder, mnem: "and" },
    ByteMnemonic { b: 0x21, op_order: OperandType::OperRegOpOrder,     mnem: "and" },
    ByteMnemonic { b: 0x22, op_order: OperandType::ByteRegOperOpOrder, mnem: "and" },
    ByteMnemonic { b: 0x23, op_order: OperandType::RegOperOpOrder,     mnem: "and" },
    // sub (code=5)
    ByteMnemonic { b: 0x28, op_order: OperandType::ByteOperRegOpOrder, mnem: "sub" },
    ByteMnemonic { b: 0x29, op_order: OperandType::OperRegOpOrder,     mnem: "sub" },
    ByteMnemonic { b: 0x2A, op_order: OperandType::ByteRegOperOpOrder, mnem: "sub" },
    ByteMnemonic { b: 0x2B, op_order: OperandType::RegOperOpOrder,     mnem: "sub" },
    // xor (code=6)
    ByteMnemonic { b: 0x30, op_order: OperandType::ByteOperRegOpOrder, mnem: "xor" },
    ByteMnemonic { b: 0x31, op_order: OperandType::OperRegOpOrder,     mnem: "xor" },
    ByteMnemonic { b: 0x32, op_order: OperandType::ByteRegOperOpOrder, mnem: "xor" },
    ByteMnemonic { b: 0x33, op_order: OperandType::RegOperOpOrder,     mnem: "xor" },
    // cmp (code=7)
    ByteMnemonic { b: 0x38, op_order: OperandType::ByteOperRegOpOrder, mnem: "cmp" },
    ByteMnemonic { b: 0x39, op_order: OperandType::OperRegOpOrder,     mnem: "cmp" },
    ByteMnemonic { b: 0x3A, op_order: OperandType::ByteRegOperOpOrder, mnem: "cmp" },
    ByteMnemonic { b: 0x3B, op_order: OperandType::RegOperOpOrder,     mnem: "cmp" },
    // movsxd and others
    ByteMnemonic { b: 0x63, op_order: OperandType::RegOperOpOrder,     mnem: "movsxd" },
    ByteMnemonic { b: 0x84, op_order: OperandType::ByteRegOperOpOrder, mnem: "test" },
    ByteMnemonic { b: 0x85, op_order: OperandType::RegOperOpOrder,     mnem: "test" },
    ByteMnemonic { b: 0x86, op_order: OperandType::ByteRegOperOpOrder, mnem: "xchg" },
    ByteMnemonic { b: 0x87, op_order: OperandType::RegOperOpOrder,     mnem: "xchg" },
    ByteMnemonic { b: 0x88, op_order: OperandType::ByteOperRegOpOrder, mnem: "mov" },
    ByteMnemonic { b: 0x89, op_order: OperandType::OperRegOpOrder,     mnem: "mov" },
    ByteMnemonic { b: 0x8A, op_order: OperandType::ByteRegOperOpOrder, mnem: "mov" },
    ByteMnemonic { b: 0x8B, op_order: OperandType::RegOperOpOrder,     mnem: "mov" },
    ByteMnemonic { b: 0x8D, op_order: OperandType::RegOperOpOrder,     mnem: "lea" },
    ByteMnemonic { b: -1,   op_order: OperandType::UnsetOpOrder,       mnem: "" },
];

// X86_ZERO_OPERAND_1_BYTE_INSTRUCTIONS expanded
static ZERO_OPERANDS_INSTR: &[ByteMnemonic] = &[
    ByteMnemonic { b: 0x90, op_order: OperandType::UnsetOpOrder, mnem: "nop" },
    ByteMnemonic { b: 0x9C, op_order: OperandType::UnsetOpOrder, mnem: "pushfq" },
    ByteMnemonic { b: 0x9D, op_order: OperandType::UnsetOpOrder, mnem: "popfq" },
    ByteMnemonic { b: 0xC3, op_order: OperandType::UnsetOpOrder, mnem: "ret" },
    ByteMnemonic { b: 0xC9, op_order: OperandType::UnsetOpOrder, mnem: "leave" },
    ByteMnemonic { b: 0xF4, op_order: OperandType::UnsetOpOrder, mnem: "hlt" },
    ByteMnemonic { b: 0xFC, op_order: OperandType::UnsetOpOrder, mnem: "cld" },
    ByteMnemonic { b: 0xFD, op_order: OperandType::UnsetOpOrder, mnem: "std" },
    ByteMnemonic { b: 0xCC, op_order: OperandType::UnsetOpOrder, mnem: "int3" },
    ByteMnemonic { b: 0x99, op_order: OperandType::UnsetOpOrder, mnem: "cdq" },
    ByteMnemonic { b: 0x9B, op_order: OperandType::UnsetOpOrder, mnem: "fwait" },
    ByteMnemonic { b: 0xA4, op_order: OperandType::UnsetOpOrder, mnem: "movsb" },
    ByteMnemonic { b: 0xA5, op_order: OperandType::UnsetOpOrder, mnem: "movs" },
    ByteMnemonic { b: 0xA6, op_order: OperandType::UnsetOpOrder, mnem: "cmpsb" },
    ByteMnemonic { b: 0xA7, op_order: OperandType::UnsetOpOrder, mnem: "cmps" },
    ByteMnemonic { b: -1,   op_order: OperandType::UnsetOpOrder, mnem: "" },
];

static CALL_JUMP_INSTR: &[ByteMnemonic] = &[
    ByteMnemonic { b: 0xE8, op_order: OperandType::UnsetOpOrder, mnem: "call" },
    ByteMnemonic { b: 0xE9, op_order: OperandType::UnsetOpOrder, mnem: "jmp" },
    ByteMnemonic { b: -1,   op_order: OperandType::UnsetOpOrder, mnem: "" },
];

// SHORT_IMMEDIATE: code*8+5 for each ALU code
static SHORT_IMMEDIATE_INSTR: &[ByteMnemonic] = &[
    ByteMnemonic { b: 0 * 8 + 5, op_order: OperandType::UnsetOpOrder, mnem: "add" },
    ByteMnemonic { b: 1 * 8 + 5, op_order: OperandType::UnsetOpOrder, mnem: "or"  },
    ByteMnemonic { b: 2 * 8 + 5, op_order: OperandType::UnsetOpOrder, mnem: "adc" },
    ByteMnemonic { b: 3 * 8 + 5, op_order: OperandType::UnsetOpOrder, mnem: "sbb" },
    ByteMnemonic { b: 4 * 8 + 5, op_order: OperandType::UnsetOpOrder, mnem: "and" },
    ByteMnemonic { b: 5 * 8 + 5, op_order: OperandType::UnsetOpOrder, mnem: "sub" },
    ByteMnemonic { b: 6 * 8 + 5, op_order: OperandType::UnsetOpOrder, mnem: "xor" },
    ByteMnemonic { b: 7 * 8 + 5, op_order: OperandType::UnsetOpOrder, mnem: "cmp" },
    ByteMnemonic { b: -1,        op_order: OperandType::UnsetOpOrder, mnem: "" },
];

// Conditional suffixes: o(0), no(1), c(2), nc(3), z(4), nz(5),
//   na(6), a(7), s(8), ns(9), pe(10), po(11), l(12), ge(13), le(14), g(15)
static CONDITIONAL_CODE_SUFFIX: &[&str] = &[
    "o", "no", "c", "nc", "z", "nz", "na", "a",
    "s", "ns", "pe", "po", "l", "ge", "le", "g",
];

// XMM conditional codes for cmpps etc.
static XMM_CONDITIONAL_CODE_SUFFIX: &[&str] = &[
    "eq", "lt", "le", "unord", "neq", "nlt", "nle", "ord",
];

// ---------------------------------------------------------------------------
// InstructionType
// ---------------------------------------------------------------------------

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum InstructionType {
    NoInstr,
    ZeroOperandsInstr,
    TwoOperandsInstr,
    JumpConditionalShortInstr,
    RegisterInstr,
    PushPopInstr,
    MoveRegInstr,
    CallJumpInstr,
    ShortImmediateInstr,
}

// ---------------------------------------------------------------------------
// InstructionDesc
// ---------------------------------------------------------------------------

#[derive(Clone, Copy)]
struct InstructionDesc {
    mnem: &'static str,
    itype: InstructionType,
    op_order: OperandType,
    byte_size_operation: bool,
}

impl Default for InstructionDesc {
    fn default() -> Self {
        InstructionDesc {
            mnem: "(bad)",
            itype: InstructionType::NoInstr,
            op_order: OperandType::UnsetOpOrder,
            byte_size_operation: false,
        }
    }
}

// ---------------------------------------------------------------------------
// InstructionTable
// ---------------------------------------------------------------------------

struct InstructionTable {
    instructions: [InstructionDesc; 256],
}

impl InstructionTable {
    fn new() -> Self {
        let mut t = InstructionTable {
            instructions: [InstructionDesc::default(); 256],
        };
        t.init();
        t
    }

    fn get(&self, x: u8) -> &InstructionDesc {
        &self.instructions[x as usize]
    }

    fn init(&mut self) {
        self.copy_table(TWO_OPERANDS_INSTR, InstructionType::TwoOperandsInstr);
        self.copy_table(ZERO_OPERANDS_INSTR, InstructionType::ZeroOperandsInstr);
        self.copy_table(CALL_JUMP_INSTR, InstructionType::CallJumpInstr);
        self.copy_table(SHORT_IMMEDIATE_INSTR, InstructionType::ShortImmediateInstr);
        self.add_jump_conditional_short();
        self.set_table_range(InstructionType::PushPopInstr, 0x50, 0x57, false, "push");
        self.set_table_range(InstructionType::PushPopInstr, 0x58, 0x5F, false, "pop");
        self.set_table_range(InstructionType::MoveRegInstr, 0xB8, 0xBF, false, "mov");
    }

    fn copy_table(&mut self, bm: &[ByteMnemonic], itype: InstructionType) {
        for entry in bm {
            if entry.b < 0 {
                break;
            }
            let idx = entry.b as usize;
            let id = &mut self.instructions[idx];
            id.mnem = entry.mnem;
            id.op_order = entry.op_order.without_byte_flag();
            id.itype = itype;
            id.byte_size_operation = entry.op_order.has_byte_size();
        }
    }

    fn set_table_range(
        &mut self,
        itype: InstructionType,
        start: u8,
        end: u8,
        byte_size: bool,
        mnem: &'static str,
    ) {
        for b in start..=end {
            let id = &mut self.instructions[b as usize];
            id.mnem = mnem;
            id.itype = itype;
            id.byte_size_operation = byte_size;
        }
    }

    fn add_jump_conditional_short(&mut self) {
        for b in 0x70u8..=0x7F {
            let id = &mut self.instructions[b as usize];
            id.mnem = "";
            id.itype = InstructionType::JumpConditionalShortInstr;
        }
    }
}

// ---------------------------------------------------------------------------
// cmov instruction descriptors
// ---------------------------------------------------------------------------

static CMOV_INSTRUCTIONS: &[InstructionDesc] = &[
    InstructionDesc { mnem: "cmovo",  itype: InstructionType::TwoOperandsInstr, op_order: OperandType::RegOperOpOrder, byte_size_operation: false },
    InstructionDesc { mnem: "cmovno", itype: InstructionType::TwoOperandsInstr, op_order: OperandType::RegOperOpOrder, byte_size_operation: false },
    InstructionDesc { mnem: "cmovc",  itype: InstructionType::TwoOperandsInstr, op_order: OperandType::RegOperOpOrder, byte_size_operation: false },
    InstructionDesc { mnem: "cmovnc", itype: InstructionType::TwoOperandsInstr, op_order: OperandType::RegOperOpOrder, byte_size_operation: false },
    InstructionDesc { mnem: "cmovz",  itype: InstructionType::TwoOperandsInstr, op_order: OperandType::RegOperOpOrder, byte_size_operation: false },
    InstructionDesc { mnem: "cmovnz", itype: InstructionType::TwoOperandsInstr, op_order: OperandType::RegOperOpOrder, byte_size_operation: false },
    InstructionDesc { mnem: "cmovna", itype: InstructionType::TwoOperandsInstr, op_order: OperandType::RegOperOpOrder, byte_size_operation: false },
    InstructionDesc { mnem: "cmova",  itype: InstructionType::TwoOperandsInstr, op_order: OperandType::RegOperOpOrder, byte_size_operation: false },
    InstructionDesc { mnem: "cmovs",  itype: InstructionType::TwoOperandsInstr, op_order: OperandType::RegOperOpOrder, byte_size_operation: false },
    InstructionDesc { mnem: "cmovns", itype: InstructionType::TwoOperandsInstr, op_order: OperandType::RegOperOpOrder, byte_size_operation: false },
    InstructionDesc { mnem: "cmovpe", itype: InstructionType::TwoOperandsInstr, op_order: OperandType::RegOperOpOrder, byte_size_operation: false },
    InstructionDesc { mnem: "cmovpo", itype: InstructionType::TwoOperandsInstr, op_order: OperandType::RegOperOpOrder, byte_size_operation: false },
    InstructionDesc { mnem: "cmovl",  itype: InstructionType::TwoOperandsInstr, op_order: OperandType::RegOperOpOrder, byte_size_operation: false },
    InstructionDesc { mnem: "cmovge", itype: InstructionType::TwoOperandsInstr, op_order: OperandType::RegOperOpOrder, byte_size_operation: false },
    InstructionDesc { mnem: "cmovle", itype: InstructionType::TwoOperandsInstr, op_order: OperandType::RegOperOpOrder, byte_size_operation: false },
    InstructionDesc { mnem: "cmovg",  itype: InstructionType::TwoOperandsInstr, op_order: OperandType::RegOperOpOrder, byte_size_operation: false },
];

// ---------------------------------------------------------------------------
// XMM instruction mnemonics
// ---------------------------------------------------------------------------

struct XmmMnemonic {
    ps_name: &'static str,
    pd_name: &'static str,
    ss_name: &'static str,
    sd_name: &'static str,
}

// XMM_ALU_CODES: sqrt(1), rsqrt(2), rcp(3), and(4), andn(5), or(6), xor(7),
//   add(8), mul(9), cvtss2sd/cvtsd2ss/cvtps2pd/cvtpd2ps(0xA), sub(0xC),
//   min(0xD), div(0xE), max(0xF)
// indices 0..15 for opcodes 0x50..0x5F, but only certain ones are valid.
// The C++ uses xmm_instructions[opcode & 0xF].
static XMM_INSTRUCTIONS: &[XmmMnemonic] = &[
    XmmMnemonic { ps_name: "?",       pd_name: "?",       ss_name: "?",       sd_name: "?" },       // 0x50
    XmmMnemonic { ps_name: "sqrtps",  pd_name: "sqrtpd",  ss_name: "sqrtss",  sd_name: "sqrtsd" },  // 0x51
    XmmMnemonic { ps_name: "rsqrtps", pd_name: "rsqrtpd", ss_name: "rsqrtss", sd_name: "rsqrtsd" }, // 0x52
    XmmMnemonic { ps_name: "rcpps",   pd_name: "rcppd",   ss_name: "rcpss",   sd_name: "rcpsd" },   // 0x53
    XmmMnemonic { ps_name: "andps",   pd_name: "andpd",   ss_name: "andss",   sd_name: "andsd" },   // 0x54
    XmmMnemonic { ps_name: "andnps",  pd_name: "andnpd",  ss_name: "andnss",  sd_name: "andnsd" },  // 0x55
    XmmMnemonic { ps_name: "orps",    pd_name: "orpd",    ss_name: "orss",    sd_name: "orsd" },    // 0x56
    XmmMnemonic { ps_name: "xorps",   pd_name: "xorpd",   ss_name: "xorss",   sd_name: "xorsd" },   // 0x57
    XmmMnemonic { ps_name: "addps",   pd_name: "addpd",   ss_name: "addss",   sd_name: "addsd" },   // 0x58
    XmmMnemonic { ps_name: "mulps",   pd_name: "mulpd",   ss_name: "mulss",   sd_name: "mulsd" },   // 0x59
    XmmMnemonic { ps_name: "cvtps2pd",pd_name: "cvtpd2ps",ss_name: "cvtss2sd",sd_name: "cvtsd2ss"}, // 0x5A
    XmmMnemonic { ps_name: "cvtdq2ps",pd_name: "cvtps2dq",ss_name: "cvttps2dq",sd_name: "?" },      // 0x5B
    XmmMnemonic { ps_name: "subps",   pd_name: "subpd",   ss_name: "subss",   sd_name: "subsd" },   // 0x5C
    XmmMnemonic { ps_name: "minps",   pd_name: "minpd",   ss_name: "minss",   sd_name: "minsd" },   // 0x5D
    XmmMnemonic { ps_name: "divps",   pd_name: "divpd",   ss_name: "divss",   sd_name: "divsd" },   // 0x5E
    XmmMnemonic { ps_name: "maxps",   pd_name: "maxpd",   ss_name: "maxss",   sd_name: "maxsd" },   // 0x5F
];

// ---------------------------------------------------------------------------
// Register names
// ---------------------------------------------------------------------------

static REG_NAMES: &[&str] = &[
    "rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi",
    "r8",  "r9",  "r10", "r11", "r12", "r13", "r14", "r15",
];

static BYTE_REG_NAMES: &[&str] = &[
    "al",  "cl",  "dl",  "bl",  "spl", "bpl", "sil", "dil",
    "r8b", "r9b", "r10b","r11b","r12b","r13b","r14b","r15b",
];

static XMM_REG_NAMES: &[&str] = &[
    "xmm0", "xmm1", "xmm2",  "xmm3",  "xmm4",  "xmm5",  "xmm6",  "xmm7",
    "xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15",
];

fn name_of_cpu_register(reg: usize) -> &'static str {
    REG_NAMES[reg]
}

fn name_of_byte_cpu_register(reg: usize) -> &'static str {
    BYTE_REG_NAMES[reg]
}

fn name_of_xmm_register(reg: usize) -> &'static str {
    XMM_REG_NAMES[reg]
}

// ---------------------------------------------------------------------------
// OperandSize
// ---------------------------------------------------------------------------

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum OperandSize {
    ByteSize = 0,
    WordSize = 1,
    DoublewordSize = 2,
    QuadwordSize = 3,
}

// ---------------------------------------------------------------------------
// DisassemblerX64
// ---------------------------------------------------------------------------

struct DisassemblerX64<'a> {
    code: &'a [u8],
    // Current position within `code`.
    pos: usize,
    // Start of the current instruction (for relative jump computation).
    instr_start: usize,
    // Output buffer.
    out: String,

    // Decoded prefix state (reset per instruction).
    rex: u8,
    operand_size_prefix: u8, // 0x66 or 0
    group_1_prefix: u8,      // 0xF2, 0xF3, or 0
    byte_size_operand: bool,

    // Base address for absolute address display.
    base_addr: usize,

    // Instruction table.
    table: InstructionTable,
}

impl<'a> DisassemblerX64<'a> {
    fn new(code: &'a [u8], base_addr: usize) -> Self {
        DisassemblerX64 {
            code,
            pos: 0,
            instr_start: 0,
            out: String::new(),
            rex: 0,
            operand_size_prefix: 0,
            group_1_prefix: 0,
            byte_size_operand: false,
            base_addr,
            table: InstructionTable::new(),
        }
    }

    fn reset_prefixes(&mut self) {
        self.rex = 0;
        self.operand_size_prefix = 0;
        self.group_1_prefix = 0;
        self.byte_size_operand = false;
    }

    // -- REX helpers --
    fn rex_b(&self) -> bool { (self.rex & 0x01) != 0 }
    fn rex_x(&self) -> bool { (self.rex & 0x02) != 0 }
    fn rex_r(&self) -> bool { (self.rex & 0x04) != 0 }
    fn rex_w(&self) -> bool { (self.rex & 0x08) != 0 }

    fn base_reg(&self, low_bits: usize) -> usize {
        low_bits | (if self.rex_b() { 8 } else { 0 })
    }

    fn operand_size(&self) -> OperandSize {
        if self.byte_size_operand {
            OperandSize::ByteSize
        } else if self.rex_w() {
            OperandSize::QuadwordSize
        } else if self.operand_size_prefix != 0 {
            OperandSize::WordSize
        } else {
            OperandSize::DoublewordSize
        }
    }

    fn operand_size_code(&self) -> &'static str {
        match self.operand_size() {
            OperandSize::ByteSize => "b",
            OperandSize::WordSize => "w",
            OperandSize::DoublewordSize => "l",
            OperandSize::QuadwordSize => "q",
        }
    }

    // -- Read helpers --
    fn peek(&self) -> u8 {
        self.code[self.pos]
    }

    fn read_u8(&mut self) -> u8 {
        let v = self.code[self.pos];
        self.pos += 1;
        v
    }

    fn read_i8(&mut self) -> i8 {
        self.read_u8() as i8
    }

    fn read_u16_le(&mut self) -> u16 {
        let lo = self.code[self.pos] as u16;
        let hi = self.code[self.pos + 1] as u16;
        self.pos += 2;
        lo | (hi << 8)
    }

    fn read_i16_le(&mut self) -> i16 {
        self.read_u16_le() as i16
    }

    fn read_u32_le(&mut self) -> u32 {
        let b = &self.code[self.pos..self.pos + 4];
        self.pos += 4;
        u32::from_le_bytes([b[0], b[1], b[2], b[3]])
    }

    fn read_i32_le(&mut self) -> i32 {
        self.read_u32_le() as i32
    }

    fn read_i64_le(&mut self) -> i64 {
        let b = &self.code[self.pos..self.pos + 8];
        self.pos += 8;
        i64::from_le_bytes([b[0], b[1], b[2], b[3], b[4], b[5], b[6], b[7]])
    }

    fn peek_i8_at(&self, offset: usize) -> i8 {
        self.code[offset] as i8
    }

    fn peek_i32_at(&self, offset: usize) -> i32 {
        let b = &self.code[offset..offset + 4];
        i32::from_le_bytes([b[0], b[1], b[2], b[3]])
    }

    // -- ModR/M and SIB --
    fn get_modrm(&self, data: u8) -> (usize, usize, usize) {
        let modd = ((data >> 6) & 3) as usize;
        let regop = (((data >> 3) & 7) | if self.rex_r() { 8 } else { 0 }) as usize;
        let rm = ((data & 7) | if self.rex_b() { 8 } else { 0 }) as usize;
        (modd, regop, rm)
    }

    fn get_sib(&self, data: u8) -> (usize, usize, usize) {
        let scale = ((data >> 6) & 3) as usize;
        let index = (((data >> 3) & 7) | if self.rex_x() { 8 } else { 0 }) as usize;
        let base = ((data & 7) | if self.rex_b() { 8 } else { 0 }) as usize;
        (scale, index, base)
    }

    // -- Output helpers --
    fn print(&mut self, s: &str) {
        self.out.push_str(s);
    }

    fn print_fmt(&mut self, args: core::fmt::Arguments<'_>) {
        let _ = self.out.write_fmt(args);
    }

    fn print_disp(&mut self, disp: i32, after: &str) {
        if disp < 0 {
            let abs = (-(disp as i64)) as u32;
            if abs == 0 {
                self.out.push_str("-0");
            } else {
                let _ = write!(self.out, "-{:#x}", abs);
            }
        } else if disp == 0 {
            self.out.push_str("+0");
        } else {
            let _ = write!(self.out, "+{:#x}", disp as u32);
        }
        self.out.push_str(after);
    }

    fn print_immediate_value(&mut self, value: i64, signed_value: bool, byte_count: i32) {
        if value >= 0 && value <= 9 {
            let _ = write!(self.out, "{}", value);
        } else if signed_value && value < 0 && value >= -9 {
            let _ = write!(self.out, "-{}", -value);
        } else if byte_count == 1 {
            let v8 = value as i8;
            if v8 < 0 && signed_value {
                let _ = write!(self.out, "-{:#x}", (-(v8 as i16)) as u8);
            } else {
                let _ = write!(self.out, "{:#x}", v8 as u8);
            }
        } else if byte_count == 2 {
            let v16 = value as i16;
            if v16 < 0 && signed_value {
                let _ = write!(self.out, "-{:#x}", (-(v16 as i32)) as u16);
            } else {
                let _ = write!(self.out, "{:#x}", v16 as u16);
            }
        } else if byte_count == 4 {
            let v32 = value as i32;
            if v32 < 0 && signed_value {
                let _ = write!(self.out, "-{:#010x}", (-(v32 as i64)) as u32);
            } else {
                let uv = v32 as u32;
                if uv > 0xffff {
                    let _ = write!(self.out, "{:#010x}", uv);
                } else {
                    let _ = write!(self.out, "{:#x}", uv);
                }
            }
        } else if byte_count == 8 {
            let v64 = value;
            if v64 < 0 && signed_value {
                let _ = write!(self.out, "-{:#018x}", (-v64) as u64);
            } else {
                let uv = v64 as u64;
                if uv > 0xffffffff {
                    let _ = write!(self.out, "{:#018x}", uv);
                } else {
                    let _ = write!(self.out, "{:#x}", uv);
                }
            }
        } else {
            // Natural sized
            if value < 0 && signed_value {
                let _ = write!(self.out, "-{:#x}", (-value) as u64);
            } else {
                let _ = write!(self.out, "{:#x}", value as u64);
            }
        }
    }

    fn print_immediate(&mut self, size: OperandSize, sign_extend: bool) -> usize {
        let (value, count): (i64, usize) = match size {
            OperandSize::ByteSize => {
                if sign_extend {
                    (self.read_i8() as i64, 1)
                } else {
                    (self.read_u8() as i64, 1)
                }
            }
            OperandSize::WordSize => {
                if sign_extend {
                    (self.read_i16_le() as i64, 2)
                } else {
                    (self.read_u16_le() as i64, 2)
                }
            }
            OperandSize::DoublewordSize | OperandSize::QuadwordSize => {
                if sign_extend {
                    (self.read_i32_le() as i64, 4)
                } else {
                    (self.read_u32_le() as i64, 4)
                }
            }
        };
        self.print_immediate_value(value, sign_extend, count as i32);
        count
    }

    fn print_address(&mut self, addr: u64) {
        let _ = write!(self.out, "{:#018x}", addr);
    }

    fn print_jump(&mut self, disp: i32) {
        // Print as signed relative displacement
        let _ = write!(self.out, "{:+}", disp);
    }

    // -- Right operand helpers --

    // `register_name_fn`: 0 = cpu reg, 1 = byte reg, 2 = xmm reg
    fn print_right_operand_helper(&mut self, register_name_fn: u8) -> usize {
        let modrm = self.read_u8();
        let (modd, _regop, rm) = self.get_modrm(modrm);

        // For mod==3, use the direct_register_name; otherwise use cpu reg for addresses
        let reg_name_for = |me: &Self, r: usize, mod3: bool| -> &'static str {
            if mod3 {
                match register_name_fn {
                    1 => name_of_byte_cpu_register(r),
                    2 => name_of_xmm_register(r),
                    _ => name_of_cpu_register(r),
                }
            } else {
                name_of_cpu_register(r)
            }
        };

        match modd {
            0 => {
                if (rm & 7) == 5 {
                    // RIP-relative
                    let disp = self.read_i32_le();
                    self.print("[rip");
                    self.print_disp(disp, "]");
                    5 // modrm + 4 bytes disp
                } else if (rm & 7) == 4 {
                    // SIB byte
                    let sib = self.read_u8();
                    let (scale, index, base) = self.get_sib(sib);
                    if index == 4 && (base & 7) == 4 && scale == 0 {
                        // index==rsp means no index
                        let _ = write!(self.out, "[{}]", name_of_cpu_register(base));
                        2
                    } else if (base & 7) == 5 {
                        // base==rbp means no base register when mod==0
                        let disp = self.read_i32_le();
                        let _ = write!(self.out, "[{}*{}", name_of_cpu_register(index), 1 << scale);
                        self.print_disp(disp, "]");
                        6
                    } else if index != 4 && (base & 7) != 5 {
                        let _ = write!(
                            self.out, "[{}+{}*{}]",
                            name_of_cpu_register(base),
                            name_of_cpu_register(index),
                            1 << scale
                        );
                        2
                    } else {
                        self.print("unknown");
                        1
                    }
                } else {
                    let _ = write!(self.out, "[{}]", name_of_cpu_register(rm));
                    1
                }
            }
            1 | 2 => {
                if (rm & 7) == 4 {
                    // SIB byte
                    let sib = self.read_u8();
                    let (scale, index, base) = self.get_sib(sib);
                    let disp = if modd == 2 {
                        self.read_i32_le()
                    } else {
                        self.read_i8() as i32
                    };
                    if index == 4 && (base & 7) == 4 && scale == 0 {
                        let _ = write!(self.out, "[{}", name_of_cpu_register(base));
                        self.print_disp(disp, "]");
                    } else {
                        let _ = write!(
                            self.out, "[{}+{}*{}",
                            name_of_cpu_register(base),
                            name_of_cpu_register(index),
                            1 << scale
                        );
                        self.print_disp(disp, "]");
                    }
                    if modd == 2 { 6 } else { 3 }
                } else {
                    let disp = if modd == 2 {
                        self.read_i32_le()
                    } else {
                        self.read_i8() as i32
                    };
                    let _ = write!(self.out, "[{}", name_of_cpu_register(rm));
                    self.print_disp(disp, "]");
                    if modd == 2 { 5 } else { 2 }
                }
            }
            3 => {
                let name = reg_name_for(self, rm, true);
                self.print(name);
                1
            }
            _ => {
                self.print("unknown");
                1
            }
        }
    }

    fn print_right_operand(&mut self) -> usize {
        self.print_right_operand_helper(0)
    }

    fn print_right_byte_operand(&mut self) -> usize {
        self.print_right_operand_helper(1)
    }

    fn print_right_xmm_operand(&mut self) -> usize {
        self.print_right_operand_helper(2)
    }

    fn print_operands(&mut self, mnem: &str, op_order: OperandType) -> usize {
        let modrm = self.peek();
        let (_modd, regop, _rm) = self.get_modrm(modrm);
        let register_name = if self.byte_size_operand {
            name_of_byte_cpu_register(regop)
        } else {
            name_of_cpu_register(regop)
        };
        match op_order {
            OperandType::RegOperOpOrder => {
                let _ = write!(self.out, "{}{} {},", mnem, self.operand_size_code(), register_name);
                if self.byte_size_operand {
                    self.print_right_byte_operand()
                } else {
                    self.print_right_operand()
                }
            }
            OperandType::OperRegOpOrder => {
                let sc = self.operand_size_code();
                let _ = write!(self.out, "{}{} ", mnem, sc);
                let advance = if self.byte_size_operand {
                    self.print_right_byte_operand()
                } else {
                    self.print_right_operand()
                };
                let _ = write!(self.out, ",{}", register_name);
                advance
            }
            _ => 0,
        }
    }

    // -- Immediate op (0x80/0x81/0x83) --
    fn print_immediate_op(&mut self) -> usize {
        let opcode = self.read_u8();
        let byte_size_immediate = (opcode & 0x03) != 1;
        let modrm = self.peek();
        let (_modd, regop, _rm) = self.get_modrm(modrm);
        let mnem = match regop & 7 {
            0 => "add",
            1 => "or",
            2 => "adc",
            3 => "sbb",
            4 => "and",
            5 => "sub",
            6 => "xor",
            7 => "cmp",
            _ => "???",
        };
        let sc = self.operand_size_code();
        let _ = write!(self.out, "{}{} ", mnem, sc);
        let count = self.print_right_operand();
        self.print(",");
        let immediate_size = if byte_size_immediate {
            OperandSize::ByteSize
        } else {
            self.operand_size()
        };
        let imm_count = self.print_immediate(immediate_size, byte_size_immediate);
        1 + count + imm_count
    }

    // -- F6/F7 instruction --
    fn f6f7_instruction(&mut self) -> usize {
        let opcode = self.read_u8();
        let modrm = self.peek();
        let (modd, regop, rm) = self.get_modrm(modrm);
        static MNEMONICS: &[Option<&str>] = &[
            Some("test"), None, Some("not"), Some("neg"),
            Some("mul"), Some("imul"), Some("div"), Some("idiv"),
        ];
        let mnem = MNEMONICS[regop & 7];
        if modd == 3 && (regop & 7) != 0 {
            let mnem = mnem.unwrap_or("???");
            if (regop & 7) > 3 {
                let _ = write!(
                    self.out, "{}{} ({},{}),{}",
                    mnem, self.operand_size_code(),
                    name_of_cpu_register(0), name_of_cpu_register(2),
                    name_of_cpu_register(rm)
                );
            } else {
                let _ = write!(
                    self.out, "{}{} {}",
                    mnem, self.operand_size_code(),
                    name_of_cpu_register(rm)
                );
            }
            // consume modrm
            self.pos += 1;
            2
        } else if (regop & 7) == 0 {
            let _ = write!(self.out, "test{} ", self.operand_size_code());
            let count = self.print_right_operand();
            self.print(",");
            let os = self.operand_size();
            let imm_count = self.print_immediate(os, false);
            1 + count + imm_count
        } else if (regop & 7) >= 4 {
            let mnem = mnem.unwrap_or("???");
            let _ = write!(
                self.out, "{}{} ({},{}),",
                mnem, self.operand_size_code(),
                name_of_cpu_register(0), name_of_cpu_register(2)
            );
            let count = self.print_right_operand();
            1 + count
        } else {
            self.print("unknown");
            1
        }
    }

    // -- Shift instruction --
    fn shift_instruction(&mut self) -> usize {
        let start = self.pos;
        let data = self.read_u8();
        let op = data & !1;
        if op != 0xD0 && op != 0xD2 && op != 0xC0 {
            self.print("unknown");
            return self.pos - start;
        }
        let modrm = self.peek();
        let (_modd, regop, _rm) = self.get_modrm(modrm);
        let regop7 = regop & 0x7;
        let mnem = match regop7 {
            0 => "rol",
            1 => "ror",
            2 => "rcl",
            3 => "rcr",
            4 => "shl",
            5 => "shr",
            7 => "sar",
            _ => { self.print("unknown"); return self.pos - start; }
        };
        let sc = self.operand_size_code();
        let _ = write!(self.out, "{}{} ", mnem, sc);
        if self.byte_size_operand {
            self.print_right_byte_operand();
        } else {
            self.print_right_operand();
        }
        if op == 0xD0 {
            self.print(",1");
        } else if op == 0xC0 {
            let imm8 = self.read_u8();
            let _ = write!(self.out, ",{}", imm8);
        } else {
            // op == 0xD2
            self.print(",cl");
        }
        self.pos - start
    }

    // -- Jump helpers --
    fn jump_short(&mut self) -> usize {
        let _opcode = self.read_u8(); // 0xEB
        let b = self.read_u8();
        let disp = (b as i8) as i32 + 2;
        self.print("jmp ");
        self.print_jump(disp);
        2
    }

    fn jump_conditional(&mut self) -> usize {
        // data[0] = 0x0F, data[1] = 0x8x
        let _op0f = self.read_u8();
        let cond_byte = self.read_u8();
        let cond = (cond_byte & 0x0F) as usize;
        let disp = self.read_i32_le() + 6;
        let mnem = CONDITIONAL_CODE_SUFFIX[cond];
        let _ = write!(self.out, "j{} ", mnem);
        self.print_jump(disp);
        6
    }

    fn jump_conditional_short(&mut self) -> usize {
        let data = self.read_u8();
        let cond = (data & 0x0F) as usize;
        let b = self.read_u8();
        let disp = (b as i8) as i32 + 2;
        let mnem = CONDITIONAL_CODE_SUFFIX[cond];
        let _ = write!(self.out, "j{} ", mnem);
        self.print_jump(disp);
        2
    }

    fn set_cc(&mut self) -> usize {
        let _op0f = self.read_u8();
        let cond_byte = self.read_u8();
        let cond = (cond_byte & 0x0F) as usize;
        let mnem = CONDITIONAL_CODE_SUFFIX[cond];
        let _ = write!(self.out, "set{} ", mnem);
        self.print_right_byte_operand();
        3
    }

    // -- FPU instructions --
    fn fpu_instruction(&mut self) -> usize {
        let escape_opcode = self.read_u8();
        let modrm_byte = self.peek();
        if modrm_byte >= 0xC0 {
            self.register_fpu_instruction(escape_opcode, modrm_byte)
        } else {
            self.memory_fpu_instruction(escape_opcode, modrm_byte)
        }
    }

    fn memory_fpu_instruction(&mut self, escape_opcode: u8, modrm_byte: u8) -> usize {
        let regop = ((modrm_byte >> 3) & 0x7) as usize;
        let mnem = match escape_opcode {
            0xD9 => match regop {
                0 => Some("fld_s"),
                3 => Some("fstp_s"),
                5 => Some("fldcw"),
                7 => Some("fnstcw"),
                _ => None,
            },
            0xDB => match regop {
                0 => Some("fild_s"),
                1 => Some("fisttp_s"),
                2 => Some("fist_s"),
                3 => Some("fistp_s"),
                _ => None,
            },
            0xDD => match regop {
                0 => Some("fld_d"),
                3 => Some("fstp_d"),
                _ => None,
            },
            0xDF => match regop {
                5 => Some("fild_d"),
                7 => Some("fistp_d"),
                _ => None,
            },
            _ => None,
        };
        if let Some(m) = mnem {
            let _ = write!(self.out, "{} ", m);
            let count = self.print_right_operand();
            count + 1
        } else {
            self.print("unknown");
            // consume the modrm byte
            self.pos += 1;
            2
        }
    }

    fn register_fpu_instruction(&mut self, escape_opcode: u8, modrm_byte: u8) -> usize {
        self.pos += 1; // consume modrm_byte
        let mut has_register = false;
        let mnem;
        match escape_opcode {
            0xD8 => { self.print("unknown"); return 2; }
            0xD9 => {
                match modrm_byte & 0xF8 {
                    0xC0 => { mnem = "fld"; has_register = true; }
                    0xC8 => { mnem = "fxch"; has_register = true; }
                    _ => match modrm_byte {
                        0xE0 => mnem = "fchs",
                        0xE1 => mnem = "fabs",
                        0xE3 => mnem = "fninit",
                        0xE4 => mnem = "ftst",
                        0xE8 => mnem = "fld1",
                        0xEB => mnem = "fldpi",
                        0xED => mnem = "fldln2",
                        0xEE => mnem = "fldz",
                        0xF0 => mnem = "f2xm1",
                        0xF1 => mnem = "fyl2x",
                        0xF2 => mnem = "fptan",
                        0xF5 => mnem = "fprem1",
                        0xF7 => mnem = "fincstp",
                        0xF8 => mnem = "fprem",
                        0xFB => mnem = "fsincos",
                        0xFD => mnem = "fscale",
                        0xFE => mnem = "fsin",
                        0xFF => mnem = "fcos",
                        _ => { self.print("unknown"); return 2; }
                    }
                }
            }
            0xDA => {
                if modrm_byte == 0xE9 { mnem = "fucompp"; }
                else { self.print("unknown"); return 2; }
            }
            0xDB => {
                if (modrm_byte & 0xF8) == 0xE8 { mnem = "fucomi"; has_register = true; }
                else if modrm_byte == 0xE2 { mnem = "fclex"; }
                else { self.print("unknown"); return 2; }
            }
            0xDC => {
                has_register = true;
                match modrm_byte & 0xF8 {
                    0xC0 => mnem = "fadd",
                    0xE8 => mnem = "fsub",
                    0xC8 => mnem = "fmul",
                    0xF8 => mnem = "fdiv",
                    _ => { self.print("unknown"); return 2; }
                }
            }
            0xDD => {
                has_register = true;
                match modrm_byte & 0xF8 {
                    0xC0 => mnem = "ffree",
                    0xD8 => mnem = "fstp",
                    _ => { self.print("unknown"); return 2; }
                }
            }
            0xDE => {
                if modrm_byte == 0xD9 {
                    mnem = "fcompp";
                } else {
                    has_register = true;
                    match modrm_byte & 0xF8 {
                        0xC0 => mnem = "faddp",
                        0xE8 => mnem = "fsubp",
                        0xC8 => mnem = "fmulp",
                        0xF8 => mnem = "fdivp",
                        _ => { self.print("unknown"); return 2; }
                    }
                }
            }
            0xDF => {
                if modrm_byte == 0xE0 { mnem = "fnstsw_ax"; }
                else if (modrm_byte & 0xF8) == 0xE8 { mnem = "fucomip"; has_register = true; }
                else { self.print("unknown"); return 2; }
            }
            _ => { self.print("unknown"); return 2; }
        }
        if has_register {
            let _ = write!(self.out, "{} st{}", mnem, modrm_byte & 0x7);
        } else {
            self.print(mnem);
        }
        2
    }

    // -- Two-byte mnemonic helper --
    fn two_byte_mnemonic(opcode: u8) -> Option<&'static str> {
        if opcode == 0x5A {
            return Some("cvtps2pd");
        }
        if (0x51..=0x5F).contains(&opcode) {
            return Some(XMM_INSTRUCTIONS[(opcode & 0xF) as usize].ps_name);
        }
        if (0xA2..=0xBF).contains(&opcode) {
            static MNEMONICS: &[Option<&str>] = &[
                Some("cpuid"),  Some("bt"),     Some("shld"),   Some("shld"),
                None,           None,           None,           None,
                None,           Some("bts"),    Some("shrd"),   Some("shrd"),
                None,           Some("imul"),   Some("cmpxchg"),Some("cmpxchg"),
                None,           None,           None,           None,
                Some("movzxb"), Some("movzxw"), Some("popcnt"), None,
                None,           None,           Some("bsf"),    Some("bsr"),
                Some("movsxb"), Some("movsxw"),
            ];
            return MNEMONICS[(opcode - 0xA2) as usize];
        }
        match opcode {
            0x12 => Some("movhlps"),
            0x16 => Some("movlhps"),
            0x1F => Some("nop"),
            0x2A => Some("cvtsi2s"),
            0x31 => Some("rdtsc"),
            _ => None,
        }
    }

    // -- 660F38 instruction --
    fn print_660f38_instruction(&mut self) -> usize {
        let b = self.read_u8();
        if b == 0x25 {
            let modrm = self.peek();
            let (_modd, regop, _rm) = self.get_modrm(modrm);
            let _ = write!(self.out, "pmovsxdq {},", name_of_xmm_register(regop));
            1 + self.print_right_xmm_operand()
        } else if b == 0x29 {
            let modrm = self.peek();
            let (_modd, regop, _rm) = self.get_modrm(modrm);
            let _ = write!(self.out, "pcmpeqq {},", name_of_xmm_register(regop));
            1 + self.print_right_xmm_operand()
        } else {
            self.print("unknown");
            1
        }
    }

    // -- Two-byte opcode instruction (0x0F prefix) --
    fn two_byte_opcode_instruction(&mut self) -> usize {
        let start = self.pos;
        let _op0f = self.read_u8(); // 0x0F
        let opcode = self.read_u8();
        let mnemonic = Self::two_byte_mnemonic(opcode);

        if self.operand_size_prefix == 0x66 {
            // 0x66 0x0F prefix
            if opcode == 0xC6 {
                let modrm = self.peek();
                let (_modd, regop, _rm) = self.get_modrm(modrm);
                let _ = write!(self.out, "shufpd {}, ", name_of_xmm_register(regop));
                self.print_right_xmm_operand();
                let imm = self.read_u8();
                let _ = write!(self.out, " [{:x}]", imm);
            } else if opcode == 0x3A {
                let third_byte = self.read_u8();
                if third_byte == 0x16 {
                    let modrm = self.peek();
                    let (_modd, regop, _rm) = self.get_modrm(modrm);
                    self.print("pextrd ");
                    self.print_right_operand();
                    let imm = self.read_u8();
                    let _ = write!(self.out, ",{},{}", name_of_xmm_register(regop), imm & 7);
                } else if third_byte == 0x17 {
                    let modrm = self.peek();
                    let (_modd, regop, _rm) = self.get_modrm(modrm);
                    self.print("extractps ");
                    self.print_right_operand();
                    let imm = self.read_u8();
                    let _ = write!(self.out, ", {}, {}", name_of_cpu_register(regop), imm & 3);
                } else if third_byte == 0x0b {
                    let modrm = self.peek();
                    let (_modd, regop, _rm) = self.get_modrm(modrm);
                    let _ = write!(self.out, "roundsd {}, ", name_of_cpu_register(regop));
                    self.print_right_operand();
                    let imm = self.read_u8();
                    let _ = write!(self.out, ", {}", imm & 3);
                } else {
                    self.print("unknown");
                }
            } else {
                let modrm = self.peek();
                let (modd, regop, rm) = self.get_modrm(modrm);
                if opcode == 0x1f {
                    self.pos += 1; // consume modrm
                    if (rm & 7) == 4 { self.pos += 1; } // SIB
                    if modd == 1 { self.pos += 1; }
                    else if modd == 2 { self.pos += 4; }
                    self.print("nop");
                } else if opcode == 0x28 {
                    let _ = write!(self.out, "movapd {}, ", name_of_xmm_register(regop));
                    self.print_right_xmm_operand();
                } else if opcode == 0x29 {
                    self.print("movapd ");
                    self.print_right_xmm_operand();
                    let _ = write!(self.out, ", {}", name_of_xmm_register(regop));
                } else if opcode == 0x38 {
                    self.print_660f38_instruction();
                } else if opcode == 0x6E {
                    let c = if self.rex_w() { 'q' } else { 'd' };
                    let _ = write!(self.out, "mov{} {},", c, name_of_xmm_register(regop));
                    self.print_right_operand();
                } else if opcode == 0x6F {
                    let _ = write!(self.out, "movdqa {},", name_of_xmm_register(regop));
                    self.print_right_xmm_operand();
                } else if opcode == 0x7E {
                    let c = if self.rex_w() { 'q' } else { 'd' };
                    let _ = write!(self.out, "mov{} ", c);
                    self.print_right_operand();
                    let _ = write!(self.out, ",{}", name_of_xmm_register(regop));
                } else if opcode == 0x7F {
                    self.print("movdqa ");
                    self.print_right_xmm_operand();
                    let _ = write!(self.out, ",{}", name_of_xmm_register(regop));
                } else if opcode == 0xD6 {
                    self.print("movq ");
                    self.print_right_xmm_operand();
                    let _ = write!(self.out, ",{}", name_of_xmm_register(regop));
                } else if opcode == 0x50 {
                    let _ = write!(self.out, "movmskpd {},", name_of_cpu_register(regop));
                    self.print_right_xmm_operand();
                } else if opcode == 0xD7 {
                    let _ = write!(self.out, "pmovmskb {},", name_of_cpu_register(regop));
                    self.print_right_xmm_operand();
                } else {
                    let m = if opcode == 0x5A {
                        "cvtpd2ps"
                    } else if (0x51..=0x5F).contains(&opcode) {
                        XMM_INSTRUCTIONS[(opcode & 0xF) as usize].pd_name
                    } else if opcode == 0x14 {
                        "unpcklpd"
                    } else if opcode == 0x15 {
                        "unpckhpd"
                    } else if opcode == 0x2E {
                        "ucomisd"
                    } else if opcode == 0x2F {
                        "comisd"
                    } else if opcode == 0xFE {
                        "paddd"
                    } else if opcode == 0xFA {
                        "psubd"
                    } else if opcode == 0xEF {
                        "pxor"
                    } else {
                        self.print("unknown");
                        return self.pos - start;
                    };
                    let _ = write!(self.out, "{} {},", m, name_of_xmm_register(regop));
                    self.print_right_xmm_operand();
                }
            }
        } else if self.group_1_prefix == 0xF2 {
            if opcode == 0x11 || opcode == 0x10 {
                self.print("movsd ");
                let modrm = self.peek();
                let (_modd, regop, _rm) = self.get_modrm(modrm);
                if opcode == 0x11 {
                    self.print_right_xmm_operand();
                    let _ = write!(self.out, ",{}", name_of_xmm_register(regop));
                } else {
                    let _ = write!(self.out, "{},", name_of_xmm_register(regop));
                    self.print_right_xmm_operand();
                }
            } else if opcode == 0x2A {
                let modrm = self.peek();
                let (_modd, regop, _rm) = self.get_modrm(modrm);
                let m = mnemonic.unwrap_or("cvtsi2s");
                let _ = write!(self.out, "{}d {},", m, name_of_xmm_register(regop));
                self.print_right_operand();
            } else if opcode == 0x2C {
                let modrm = self.peek();
                let (_modd, regop, _rm) = self.get_modrm(modrm);
                let _ = write!(self.out, "cvttsd2si{} {},", self.operand_size_code(), name_of_cpu_register(regop));
                self.print_right_xmm_operand();
            } else if opcode == 0x2D {
                let modrm = self.peek();
                let (_modd, regop, _rm) = self.get_modrm(modrm);
                let _ = write!(self.out, "cvtsd2si{} {},", self.operand_size_code(), name_of_cpu_register(regop));
                self.print_right_xmm_operand();
            } else if (0x51..=0x5F).contains(&opcode) {
                let modrm = self.peek();
                let (_modd, regop, _rm) = self.get_modrm(modrm);
                let m = if opcode == 0x5A {
                    "cvtsd2ss"
                } else {
                    XMM_INSTRUCTIONS[(opcode & 0xF) as usize].sd_name
                };
                let _ = write!(self.out, "{} {},", m, name_of_xmm_register(regop));
                self.print_right_xmm_operand();
            } else {
                self.print("unknown");
            }
        } else if self.group_1_prefix == 0xF3 {
            if opcode == 0x11 || opcode == 0x10 {
                self.print("movss ");
                let modrm = self.peek();
                let (_modd, regop, _rm) = self.get_modrm(modrm);
                if opcode == 0x11 {
                    self.print_right_operand();
                    let _ = write!(self.out, ",{}", name_of_xmm_register(regop));
                } else {
                    let _ = write!(self.out, "{},", name_of_xmm_register(regop));
                    self.print_right_operand();
                }
            } else if opcode == 0x2A {
                let modrm = self.peek();
                let (_modd, regop, _rm) = self.get_modrm(modrm);
                let m = mnemonic.unwrap_or("cvtsi2s");
                let _ = write!(self.out, "{}s {},", m, name_of_xmm_register(regop));
                self.print_right_operand();
            } else if opcode == 0x2C || opcode == 0x2D {
                let truncating = (opcode & 1) == 0;
                let modrm = self.peek();
                let (_modd, regop, _rm) = self.get_modrm(modrm);
                let _ = write!(
                    self.out, "cvt{}ss2si{} {},",
                    if truncating { "t" } else { "" },
                    self.operand_size_code(),
                    name_of_cpu_register(regop)
                );
                self.print_right_xmm_operand();
            } else if (0x51..=0x5F).contains(&opcode) {
                let modrm = self.peek();
                let (_modd, regop, _rm) = self.get_modrm(modrm);
                let m = if opcode == 0x5A {
                    "cvtss2sd"
                } else {
                    XMM_INSTRUCTIONS[(opcode & 0xF) as usize].ss_name
                };
                let _ = write!(self.out, "{} {},", m, name_of_xmm_register(regop));
                self.print_right_xmm_operand();
            } else if opcode == 0x7E {
                let modrm = self.peek();
                let (_modd, regop, _rm) = self.get_modrm(modrm);
                let _ = write!(self.out, "movq {}, ", name_of_xmm_register(regop));
                self.print_right_xmm_operand();
            } else if opcode == 0xE6 {
                let modrm = self.peek();
                let (_modd, regop, _rm) = self.get_modrm(modrm);
                let _ = write!(self.out, "cvtdq2pd {},", name_of_xmm_register(regop));
                self.print_right_xmm_operand();
            } else if opcode == 0xB8 {
                let m = mnemonic.unwrap_or("popcnt");
                self.print_operands(m, OperandType::RegOperOpOrder);
            } else if opcode == 0xBD {
                self.print_operands("lzcnt", OperandType::RegOperOpOrder);
            } else {
                self.print("unknown");
            }
        } else if opcode == 0x1F {
            // NOP
            let modrm = self.peek();
            let (modd, _regop, rm) = self.get_modrm(modrm);
            self.pos += 1; // consume modrm
            if (rm & 7) == 4 { self.pos += 1; } // SIB
            if modd == 1 { self.pos += 1; }
            else if modd == 2 { self.pos += 4; }
            self.print("nop");
        } else if opcode == 0x28 || opcode == 0x2f {
            let modrm = self.peek();
            let (_modd, regop, _rm) = self.get_modrm(modrm);
            let m = if opcode == 0x28 { "movaps" } else { "comiss" };
            let _ = write!(self.out, "{} {},", m, name_of_xmm_register(regop));
            self.print_right_xmm_operand();
        } else if opcode == 0x29 {
            let modrm = self.peek();
            let (_modd, regop, _rm) = self.get_modrm(modrm);
            self.print("movaps ");
            self.print_right_xmm_operand();
            let _ = write!(self.out, ",{}", name_of_xmm_register(regop));
        } else if opcode == 0x11 {
            let modrm = self.peek();
            let (_modd, regop, _rm) = self.get_modrm(modrm);
            self.print("movups ");
            self.print_right_xmm_operand();
            let _ = write!(self.out, ",{}", name_of_xmm_register(regop));
        } else if opcode == 0x50 {
            let modrm = self.peek();
            let (_modd, regop, _rm) = self.get_modrm(modrm);
            let _ = write!(self.out, "movmskps {},", name_of_cpu_register(regop));
            self.print_right_xmm_operand();
        } else if opcode == 0xA2 || opcode == 0x31 {
            let m = mnemonic.unwrap_or("?");
            self.print(m);
        } else if (opcode & 0xF0) == 0x40 {
            // CMOVcc
            let condition = (opcode & 0x0F) as usize;
            let idesc = &CMOV_INSTRUCTIONS[condition];
            self.byte_size_operand = idesc.byte_size_operation;
            self.print_operands(idesc.mnem, idesc.op_order);
        } else if (0x10..=0x16).contains(&opcode) {
            static PS_MNEMONICS: &[Option<&str>] = &[
                Some("movups"), None, Some("movhlps"), None,
                Some("unpcklps"), Some("unpckhps"), Some("movlhps"),
            ];
            let m = PS_MNEMONICS[(opcode - 0x10) as usize];
            if let Some(m) = m {
                let modrm = self.peek();
                let (_modd, regop, _rm) = self.get_modrm(modrm);
                let _ = write!(self.out, "{} {},", m, name_of_xmm_register(regop));
                self.print_right_xmm_operand();
            } else {
                self.print("unknown");
            }
        } else if (0x51..=0x5F).contains(&opcode) {
            let modrm = self.peek();
            let (_modd, regop, _rm) = self.get_modrm(modrm);
            let m = if opcode == 0x5A {
                "cvtps2pd"
            } else {
                XMM_INSTRUCTIONS[(opcode & 0xF) as usize].ps_name
            };
            let _ = write!(self.out, "{} {},", m, name_of_xmm_register(regop));
            self.print_right_xmm_operand();
        } else if opcode == 0xC2 || opcode == 0xC6 {
            let modrm = self.peek();
            let (_modd, regop, _rm) = self.get_modrm(modrm);
            if opcode == 0xC2 {
                let _ = write!(self.out, "cmpps {},", name_of_xmm_register(regop));
                self.print_right_xmm_operand();
                let imm = self.read_u8();
                let _ = write!(self.out, " [{}]", XMM_CONDITIONAL_CODE_SUFFIX[imm as usize]);
            } else {
                let _ = write!(self.out, "shufps {},", name_of_xmm_register(regop));
                self.print_right_xmm_operand();
                let imm = self.read_u8();
                let _ = write!(self.out, " [{:x}]", imm);
            }
        } else if (opcode & 0xF0) == 0x80 {
            // Jcc: conditional jump (long)
            // Back up so jump_conditional can re-read
            self.pos = start;
            self.jump_conditional();
        } else if opcode == 0xBE || opcode == 0xBF || opcode == 0xB6 ||
                  opcode == 0xB7 || opcode == 0xAF || opcode == 0xB0 ||
                  opcode == 0xB1 || opcode == 0xBC || opcode == 0xBD {
            let m = mnemonic.unwrap_or("?");
            self.print_operands(m, OperandType::RegOperOpOrder);
        } else if (opcode & 0xF0) == 0x90 {
            // SETcc
            self.pos = start;
            self.set_cc();
        } else if ((opcode & 0xFE) == 0xA4) || ((opcode & 0xFE) == 0xAC) ||
                  (opcode == 0xAB) || (opcode == 0xA3) {
            let m = mnemonic.unwrap_or("?");
            let sc = self.operand_size_code();
            let _ = write!(self.out, "{}{} ", m, sc);
            let modrm = self.peek();
            let (_modd, regop, _rm) = self.get_modrm(modrm);
            self.print_right_operand();
            let _ = write!(self.out, ",{}", name_of_cpu_register(regop));
            if opcode == 0xAB || opcode == 0xA3 || opcode == 0xBD {
                // done
            } else if opcode == 0xA5 || opcode == 0xAD {
                self.print(",cl");
            } else {
                self.print(",");
                self.print_immediate(OperandSize::ByteSize, false);
            }
        } else if opcode == 0xBA {
            let modrm = self.peek();
            if (modrm & 0x60) == 0x60 {
                let r = ((modrm >> 3) & 7) as usize;
                static BT_NAMES: &[&str] = &["bt", "bts", "btr", "btc"];
                let sc = self.operand_size_code();
                let _ = write!(self.out, "{}{} ", BT_NAMES[r - 4], sc);
                self.print_right_operand();
                let bit = self.read_u8();
                let _ = write!(self.out, ",{}", bit);
            } else {
                self.print("unknown");
            }
        } else {
            self.print("unknown");
        }
        self.pos - start
    }

    // -- Main decode --
    fn decode_instruction_type(&mut self) -> bool {
        // Scan for prefixes
        loop {
            let current = self.peek();
            if current == 0x66 {
                self.operand_size_prefix = current;
            } else if (current & 0xF0) == 0x40 {
                // REX prefix
                self.rex = current;
            } else if (current & 0xFE) == 0xF2 {
                self.group_1_prefix = current;
            } else if current == 0xF0 {
                self.print("lock ");
            } else {
                break;
            }
            self.pos += 1;
        }

        let current = self.peek();
        let idesc = *self.table.get(current);
        self.byte_size_operand = idesc.byte_size_operation;

        match idesc.itype {
            InstructionType::ZeroOperandsInstr => {
                if current >= 0xA4 && current <= 0xA7 {
                    if self.group_1_prefix == 0xF3 {
                        self.print("rep ");
                    }
                    if (current & 0x01) == 0x01 {
                        match self.operand_size() {
                            OperandSize::WordSize => { let _ = write!(self.out, "{}w", idesc.mnem); }
                            OperandSize::DoublewordSize => { let _ = write!(self.out, "{}l", idesc.mnem); }
                            OperandSize::QuadwordSize => { let _ = write!(self.out, "{}q", idesc.mnem); }
                            _ => {}
                        }
                    } else {
                        self.print(idesc.mnem);
                    }
                } else if current == 0x99 && self.rex_w() {
                    self.print("cqo");
                } else {
                    self.print(idesc.mnem);
                }
                self.pos += 1;
                true
            }
            InstructionType::TwoOperandsInstr => {
                self.pos += 1;
                self.print_operands(idesc.mnem, idesc.op_order);
                true
            }
            InstructionType::JumpConditionalShortInstr => {
                self.jump_conditional_short();
                true
            }
            InstructionType::RegisterInstr => {
                let reg = self.base_reg((current & 0x07) as usize);
                let _ = write!(
                    self.out, "{}{} {}",
                    idesc.mnem, self.operand_size_code(),
                    name_of_cpu_register(reg)
                );
                self.pos += 1;
                true
            }
            InstructionType::PushPopInstr => {
                let reg = self.base_reg((current & 0x07) as usize);
                let _ = write!(self.out, "{} {}", idesc.mnem, name_of_cpu_register(reg));
                self.pos += 1;
                true
            }
            InstructionType::MoveRegInstr => {
                let reg = self.base_reg((current & 0x07) as usize);
                self.pos += 1;
                let (addr, imm_bytes): (i64, usize) = match self.operand_size() {
                    OperandSize::WordSize => {
                        (self.read_i16_le() as i64, 2)
                    }
                    OperandSize::DoublewordSize => {
                        (self.read_i32_le() as i64, 4)
                    }
                    OperandSize::QuadwordSize => {
                        (self.read_i64_le(), 8)
                    }
                    _ => (0, 0),
                };
                let _ = write!(
                    self.out, "mov{} {},",
                    self.operand_size_code(),
                    name_of_cpu_register(reg)
                );
                self.print_immediate_value(addr, false, imm_bytes as i32);
                true
            }
            InstructionType::CallJumpInstr => {
                self.pos += 1;
                let disp = self.read_i32_le() + 5;
                let _ = write!(self.out, "{} ", idesc.mnem);
                self.print_jump(disp);
                true
            }
            InstructionType::ShortImmediateInstr => {
                let sc = self.operand_size_code();
                let rax = name_of_cpu_register(0);
                let _ = write!(self.out, "{}{} {},", idesc.mnem, sc, rax);
                self.pos += 1;
                self.print_immediate(OperandSize::DoublewordSize, false);
                true
            }
            InstructionType::NoInstr => false,
        }
    }

    fn instruction_decode(&mut self) -> usize {
        self.reset_prefixes();
        self.instr_start = self.pos;

        let processed = self.decode_instruction_type();

        if !processed {
            let current = self.peek();
            match current {
                0xC2 => {
                    self.pos += 1;
                    self.print("ret ");
                    let imm = self.read_u16_le();
                    self.print_immediate_value(imm as i64, false, -1);
                }
                0xC8 => {
                    self.pos += 1;
                    let size = self.read_u16_le();
                    let nesting = self.read_u8();
                    let _ = write!(self.out, "enter {}, {}", size, nesting);
                }
                0x69 | 0x6B => {
                    let opcode = self.read_u8();
                    let modrm = self.peek();
                    let (_modd, regop, rm) = self.get_modrm(modrm);
                    self.pos += 1; // consume modrm
                    let imm: i32 = if opcode == 0x6B {
                        self.read_i8() as i32
                    } else {
                        self.read_i32_le()
                    };
                    let _ = write!(
                        self.out, "imul{} {},{},",
                        self.operand_size_code(),
                        name_of_cpu_register(regop),
                        name_of_cpu_register(rm)
                    );
                    self.print_immediate_value(imm as i64, false, -1);
                }
                0x81 | 0x83 => {
                    self.print_immediate_op();
                }
                0x80 => {
                    self.byte_size_operand = true;
                    self.print_immediate_op();
                }
                0x0F => {
                    self.two_byte_opcode_instruction();
                }
                0x8F => {
                    self.pos += 1;
                    let modrm = self.peek();
                    let (_modd, regop, _rm) = self.get_modrm(modrm);
                    if (regop & 7) == 0 {
                        self.print("pop ");
                        self.print_right_operand();
                    }
                }
                0xFF => {
                    self.pos += 1;
                    let modrm = self.peek();
                    let (_modd, regop, _rm) = self.get_modrm(modrm);
                    let mnem = match regop & 7 {
                        0 => "inc",
                        1 => "dec",
                        2 => "call",
                        4 => "jmp",
                        6 => "push",
                        _ => "???",
                    };
                    if (regop & 7) <= 1 {
                        let sc = self.operand_size_code();
                        let _ = write!(self.out, "{}{} ", mnem, sc);
                    } else {
                        let _ = write!(self.out, "{} ", mnem);
                    }
                    self.print_right_operand();
                }
                0xC7 | 0xC6 => {
                    let is_byte = current == 0xC6;
                    self.pos += 1;
                    if is_byte {
                        self.print("movb ");
                        self.print_right_byte_operand();
                        self.print(",");
                        self.print_immediate(OperandSize::ByteSize, false);
                    } else {
                        let sc = self.operand_size_code();
                        let _ = write!(self.out, "mov{} ", sc);
                        self.print_right_operand();
                        self.print(",");
                        let os = self.operand_size();
                        self.print_immediate(os, true);
                    }
                }
                0xEB => {
                    self.jump_short();
                }
                0xF6 => {
                    self.byte_size_operand = true;
                    self.f6f7_instruction();
                }
                0xF7 => {
                    self.f6f7_instruction();
                }
                0xD1 | 0xD3 | 0xC1 => {
                    self.shift_instruction();
                }
                0xD0 | 0xD2 | 0xC0 => {
                    self.byte_size_operand = true;
                    self.shift_instruction();
                }
                0xD8 | 0xD9 | 0xDA | 0xDB | 0xDC | 0xDD | 0xDE | 0xDF => {
                    self.fpu_instruction();
                }
                0x88 | 0x89 => {
                    let is_byte = current == 0x88;
                    self.pos += 1;
                    let modrm = self.peek();
                    let (_modd, regop, _rm) = self.get_modrm(modrm);
                    if is_byte {
                        self.print("movb ");
                        self.print_right_byte_operand();
                        let _ = write!(self.out, ",{}", name_of_byte_cpu_register(regop));
                    } else {
                        let sc = self.operand_size_code();
                        let _ = write!(self.out, "mov{} ", sc);
                        self.print_right_operand();
                        let _ = write!(self.out, ",{}", name_of_cpu_register(regop));
                    }
                }
                0x90..=0x97 => {
                    let reg = (current & 0x7) as usize | if self.rex_b() { 8 } else { 0 };
                    if reg == 0 {
                        self.print("nop");
                    } else {
                        let _ = write!(
                            self.out, "xchg{} {}, {}",
                            self.operand_size_code(),
                            name_of_cpu_register(0),
                            name_of_cpu_register(reg)
                        );
                    }
                    self.pos += 1;
                }
                0xB0..=0xBF => {
                    let opcode = self.read_u8();
                    let is_not_8bit = opcode >= 0xB8;
                    let reg = ((opcode & 0x7) as usize) | if self.rex_b() { 8 } else { 0 };
                    if is_not_8bit {
                        let sc = self.operand_size_code();
                        let _ = write!(self.out, "mov{} {},", sc, name_of_cpu_register(reg));
                        let os = self.operand_size();
                        self.print_immediate(os, false);
                    } else {
                        let _ = write!(self.out, "movb {},", name_of_byte_cpu_register(reg));
                        self.print_immediate(OperandSize::ByteSize, false);
                    }
                }
                0xFE => {
                    self.pos += 1;
                    let modrm = self.peek();
                    let (_modd, regop, _rm) = self.get_modrm(modrm);
                    if (regop & 7) == 1 {
                        self.print("decb ");
                        self.print_right_byte_operand();
                    } else {
                        self.print("unknown");
                    }
                }
                0x68 => {
                    self.pos += 1;
                    self.print("push ");
                    let imm = self.read_i32_le();
                    self.print_immediate_value(imm as i64, false, -1);
                }
                0x6A => {
                    self.pos += 1;
                    self.print("push ");
                    let imm = self.read_i8();
                    self.print_immediate_value(imm as i64, false, -1);
                }
                0xA8 => {
                    self.pos += 1;
                    self.print("test al,");
                    let imm = self.read_u8();
                    self.print_immediate_value(imm as i64, false, -1);
                }
                0xA9 => {
                    self.pos += 1;
                    let sc = self.operand_size_code();
                    let rax = name_of_cpu_register(0);
                    let _ = write!(self.out, "test{} {},", sc, rax);
                    let os = self.operand_size();
                    self.print_immediate(os, false);
                }
                _ => {
                    self.print("unknown");
                    self.pos += 1;
                }
            }
        }

        let instr_len = self.pos - self.instr_start;
        assert!(instr_len > 0, "No progress in instruction decode");
        instr_len
    }
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Disassemble a single instruction from `code` at offset 0.
/// Returns (disassembly_text, byte_length).
pub fn disassemble_one(code: &[u8], pc: usize) -> (String, usize) {
    let mut d = DisassemblerX64::new(code, pc);
    let len = d.instruction_decode();
    (d.out, len)
}

/// Disassemble all instructions in `code`, starting at virtual address `base_addr`.
/// Returns a multi-line string with one line per instruction, each terminated by '\n'.
pub fn disassemble(code: &[u8], base_addr: usize) -> String {
    let mut result = String::new();
    let mut d = DisassemblerX64::new(code, base_addr);
    while d.pos < d.code.len() {
        d.out.clear();
        d.instruction_decode();
        result.push_str(&d.out);
        result.push('\n');
    }
    result
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    /// Helper: disassemble bytes, return the single-instruction text.
    fn dis(bytes: &[u8]) -> String {
        let (text, _len) = disassemble_one(bytes, 0);
        text
    }

    /// Helper: disassemble bytes, return (text, length).
    fn dis_len(bytes: &[u8]) -> (String, usize) {
        disassemble_one(bytes, 0)
    }

    /// Helper: disassemble all, return multi-line string.
    fn dis_all(bytes: &[u8]) -> String {
        disassemble(bytes, 0)
    }

    // -----------------------------------------------------------------------
    // 1. ret
    // -----------------------------------------------------------------------
    #[test]
    fn test_ret() {
        assert_eq!(dis(&[0xC3]), "ret");
    }

    // -----------------------------------------------------------------------
    // 2. nop
    // -----------------------------------------------------------------------
    #[test]
    fn test_nop() {
        assert_eq!(dis(&[0x90]), "nop");
    }

    // -----------------------------------------------------------------------
    // 3. int3
    // -----------------------------------------------------------------------
    #[test]
    fn test_int3() {
        assert_eq!(dis(&[0xCC]), "int3");
    }

    // -----------------------------------------------------------------------
    // 4. push/pop register
    // -----------------------------------------------------------------------
    #[test]
    fn test_push_rax() {
        assert_eq!(dis(&[0x50]), "push rax");
    }

    #[test]
    fn test_push_rbp() {
        assert_eq!(dis(&[0x55]), "push rbp");
    }

    #[test]
    fn test_pop_rax() {
        assert_eq!(dis(&[0x58]), "pop rax");
    }

    #[test]
    fn test_pop_rdx() {
        assert_eq!(dis(&[0x5A]), "pop rdx");
    }

    #[test]
    fn test_push_r15() {
        // REX.B=1 (0x41), push (0x57 = push rdi, but with REX.B -> r15)
        assert_eq!(dis(&[0x41, 0x57]), "push r15");
    }

    #[test]
    fn test_pop_r15() {
        assert_eq!(dis(&[0x41, 0x5F]), "pop r15");
    }

    // -----------------------------------------------------------------------
    // 5. mov reg,reg (REX.W movq)
    //    48 89 c8 = movq rax,rcx
    // -----------------------------------------------------------------------
    #[test]
    fn test_movq_rax_rcx() {
        // 48 89 C8: REX.W mov r/m64, r64  -- mod=11, reg=rcx(1), rm=rax(0)
        assert_eq!(dis(&[0x48, 0x89, 0xC8]), "movq rax,rcx");
    }

    #[test]
    fn test_movq_rcx_rax() {
        // 48 8B C8: REX.W mov r64, r/m64 -- mod=11, reg=rcx(1), rm=rax(0)
        assert_eq!(dis(&[0x48, 0x8B, 0xC8]), "movq rcx,rax");
    }

    // -----------------------------------------------------------------------
    // 6. movl (32-bit, no REX.W)
    //    89 C8 = movl rax,rcx
    // -----------------------------------------------------------------------
    #[test]
    fn test_movl_rax_rcx() {
        assert_eq!(dis(&[0x89, 0xC8]), "movl rax,rcx");
    }

    // -----------------------------------------------------------------------
    // 7. mov reg, immediate
    //    B8+rd = movl reg, imm32
    // -----------------------------------------------------------------------
    #[test]
    fn test_movl_rax_imm() {
        // B8 2A 00 00 00 = movl rax, 0x2a
        assert_eq!(dis(&[0xB8, 0x2A, 0x00, 0x00, 0x00]), "movl rax,0x2a");
    }

    #[test]
    fn test_movl_rax_0() {
        assert_eq!(dis(&[0xB8, 0x00, 0x00, 0x00, 0x00]), "movl rax,0");
    }

    #[test]
    fn test_movl_rcx_0() {
        assert_eq!(dis(&[0xB9, 0x00, 0x00, 0x00, 0x00]), "movl rcx,0");
    }

    // -----------------------------------------------------------------------
    // 8. movq reg, imm64 (REX.W + B8+rd)
    // -----------------------------------------------------------------------
    #[test]
    fn test_movq_rax_imm64() {
        // 48 B8 21 43 65 87 78 56 34 12 = movq rax, 0x1234567887654321
        assert_eq!(
            dis(&[0x48, 0xB8, 0x21, 0x43, 0x65, 0x87, 0x78, 0x56, 0x34, 0x12]),
            "movq rax,0x1234567887654321"
        );
    }

    #[test]
    fn test_movq_rcx_neg1() {
        // 48 C7 C1 FF FF FF FF = movq rcx,-1 (mov r/m64, imm32 sign-ext)
        assert_eq!(
            dis(&[0x48, 0xC7, 0xC1, 0xFF, 0xFF, 0xFF, 0xFF]),
            "movq rcx,-1"
        );
    }

    #[test]
    fn test_movq_rcx_neg1_movabs() {
        // 48 B9 FF FF FF FF FF FF FF FF = movq rcx, 0xffffffffffffffff (movabs)
        // Large value that doesn't fit in i32 sign-ext form
        assert_eq!(
            dis(&[0x48, 0xB9, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]),
            "movq rcx,0xffffffffffffffff"
        );
    }

    // -----------------------------------------------------------------------
    // 9. mov reg, [base] (memory operand)
    //    48 8B 04 24 = movq rax,[rsp]
    // -----------------------------------------------------------------------
    #[test]
    fn test_movq_rax_mem_rsp() {
        assert_eq!(dis(&[0x48, 0x8B, 0x04, 0x24]), "movq rax,[rsp]");
    }

    // -----------------------------------------------------------------------
    // 10. mov reg, [base+disp8]
    //     48 8B 45 08 = movq rax,[rbp+0x8]
    // -----------------------------------------------------------------------
    #[test]
    fn test_movq_rax_mem_rbp_disp8() {
        assert_eq!(dis(&[0x48, 0x8B, 0x45, 0x08]), "movq rax,[rbp+0x8]");
    }

    #[test]
    fn test_movq_rax_mem_rbp_neg_disp8() {
        // 48 8B 45 F8 = movq rax,[rbp-0x8]
        assert_eq!(dis(&[0x48, 0x8B, 0x45, 0xF8]), "movq rax,[rbp-0x8]");
    }

    // -----------------------------------------------------------------------
    // 11. mov reg, [base+disp32]
    //     48 8B 85 00 08 00 00 = movq rax,[rbp+0x800]
    // -----------------------------------------------------------------------
    #[test]
    fn test_movq_rax_mem_rbp_disp32() {
        assert_eq!(
            dis(&[0x48, 0x8B, 0x85, 0x00, 0x08, 0x00, 0x00]),
            "movq rax,[rbp+0x800]"
        );
    }

    #[test]
    fn test_movq_rax_mem_rbp_neg_disp32() {
        assert_eq!(
            dis(&[0x48, 0x8B, 0x85, 0x00, 0xF8, 0xFF, 0xFF]),
            "movq rax,[rbp-0x800]"
        );
    }

    // -----------------------------------------------------------------------
    // 12. mov reg, [rsp+disp8]  (SIB encoding)
    //     48 8B 44 24 08 = movq rax,[rsp+0x8]
    // -----------------------------------------------------------------------
    #[test]
    fn test_movq_rax_mem_rsp_disp8() {
        assert_eq!(dis(&[0x48, 0x8B, 0x44, 0x24, 0x08]), "movq rax,[rsp+0x8]");
    }

    #[test]
    fn test_movq_rax_mem_rsp_neg_disp8() {
        assert_eq!(dis(&[0x48, 0x8B, 0x44, 0x24, 0xF8]), "movq rax,[rsp-0x8]");
    }

    // -----------------------------------------------------------------------
    // 13. mov [base], reg
    //     48 89 04 24 = movq [rsp],rax
    // -----------------------------------------------------------------------
    #[test]
    fn test_movq_mem_rsp_rax() {
        assert_eq!(dis(&[0x48, 0x89, 0x04, 0x24]), "movq [rsp],rax");
    }

    // -----------------------------------------------------------------------
    // 14. mov reg, [rax]
    //     48 8B 00 = movq rax,[rax]
    // -----------------------------------------------------------------------
    #[test]
    fn test_movq_rax_mem_rax() {
        assert_eq!(dis(&[0x48, 0x8B, 0x00]), "movq rax,[rax]");
    }

    // -----------------------------------------------------------------------
    // 15. REX.B registers: r10, r12, r13
    // -----------------------------------------------------------------------
    #[test]
    fn test_movq_rax_mem_r10() {
        // 49 8B 02 = movq rax,[r10]
        assert_eq!(dis(&[0x49, 0x8B, 0x02]), "movq rax,[r10]");
    }

    #[test]
    fn test_movq_rax_mem_r12() {
        // 49 8B 04 24 = movq rax,[r12]  (SIB for r12)
        assert_eq!(dis(&[0x49, 0x8B, 0x04, 0x24]), "movq rax,[r12]");
    }

    #[test]
    fn test_movq_rax_mem_r13_disp0() {
        // 49 8B 45 00 = movq rax,[r13+0]  (r13 requires disp8=0 for mod=01)
        assert_eq!(dis(&[0x49, 0x8B, 0x45, 0x00]), "movq rax,[r13+0]");
    }

    // -----------------------------------------------------------------------
    // 16. add reg,reg
    //     48 01 C8 = addq rax,rcx  (add r/m64, r64)
    // -----------------------------------------------------------------------
    #[test]
    fn test_addq_rax_rcx() {
        assert_eq!(dis(&[0x48, 0x01, 0xC8]), "addq rax,rcx");
    }

    #[test]
    fn test_addl_rax_rcx() {
        assert_eq!(dis(&[0x01, 0xC8]), "addl rax,rcx");
    }

    // -----------------------------------------------------------------------
    // 17. sub reg,reg
    //     48 29 C8 = subq rax,rcx
    // -----------------------------------------------------------------------
    #[test]
    fn test_subq_rax_rcx() {
        assert_eq!(dis(&[0x48, 0x29, 0xC8]), "subq rax,rcx");
    }

    // -----------------------------------------------------------------------
    // 18. and, or, xor reg,reg
    // -----------------------------------------------------------------------
    #[test]
    fn test_andq_rax_rcx() {
        // 48 21 C8
        assert_eq!(dis(&[0x48, 0x21, 0xC8]), "andq rax,rcx");
    }

    #[test]
    fn test_orq_rax_rcx() {
        // 48 09 C8
        assert_eq!(dis(&[0x48, 0x09, 0xC8]), "orq rax,rcx");
    }

    #[test]
    fn test_xorq_rax_rax() {
        // 48 31 C0
        assert_eq!(dis(&[0x48, 0x31, 0xC0]), "xorq rax,rax");
    }

    // -----------------------------------------------------------------------
    // 19. cmp reg,reg
    //     48 39 C8 = cmpq rax,rcx
    // -----------------------------------------------------------------------
    #[test]
    fn test_cmpq_rax_rcx() {
        assert_eq!(dis(&[0x48, 0x39, 0xC8]), "cmpq rax,rcx");
    }

    // -----------------------------------------------------------------------
    // 20. add reg, imm8 (sign-extended)
    //     48 83 C0 02 = addq rax,2
    // -----------------------------------------------------------------------
    #[test]
    fn test_addq_rax_imm8() {
        assert_eq!(dis(&[0x48, 0x83, 0xC0, 0x02]), "addq rax,2");
    }

    // -----------------------------------------------------------------------
    // 21. sub reg, imm8
    //     48 83 E8 02 = subq rax,2
    // -----------------------------------------------------------------------
    #[test]
    fn test_subq_rax_imm8() {
        assert_eq!(dis(&[0x48, 0x83, 0xE8, 0x02]), "subq rax,2");
    }

    // -----------------------------------------------------------------------
    // 22. cmp reg, imm8
    //     48 83 F9 57 = cmpq rcx,0x57
    // -----------------------------------------------------------------------
    #[test]
    fn test_cmpq_rcx_imm8() {
        assert_eq!(dis(&[0x48, 0x83, 0xF9, 0x57]), "cmpq rcx,0x57");
    }

    // -----------------------------------------------------------------------
    // 23. cmp reg, imm32
    //     48 81 F8 FF FF FF 7F = cmpq rax,0x7fffffff
    // -----------------------------------------------------------------------
    #[test]
    fn test_cmpq_rax_imm32() {
        assert_eq!(
            dis(&[0x48, 0x81, 0xF8, 0xFF, 0xFF, 0xFF, 0x7F]),
            "cmpq rax,0x7fffffff"
        );
    }

    // -----------------------------------------------------------------------
    // 24. test reg,reg
    //     48 85 C0 = testq rax,rax
    // -----------------------------------------------------------------------
    #[test]
    fn test_testq_rax_rax() {
        assert_eq!(dis(&[0x48, 0x85, 0xC0]), "testq rax,rax");
    }

    // -----------------------------------------------------------------------
    // 25. inc / dec
    //     48 FF C1 = incq rcx (FF /0)
    //     48 FF C9 = decq rcx (FF /1)
    // -----------------------------------------------------------------------
    #[test]
    fn test_incq_rcx() {
        assert_eq!(dis(&[0x48, 0xFF, 0xC1]), "incq rcx");
    }

    #[test]
    fn test_decq_rcx() {
        assert_eq!(dis(&[0x48, 0xFF, 0xC9]), "decq rcx");
    }

    #[test]
    fn test_incl_mem_rsp() {
        // FF 04 24 = incl [rsp]
        assert_eq!(dis(&[0xFF, 0x04, 0x24]), "incl [rsp]");
    }

    #[test]
    fn test_incq_mem_rsp() {
        // 48 FF 04 24 = incq [rsp]
        assert_eq!(dis(&[0x48, 0xFF, 0x04, 0x24]), "incq [rsp]");
    }

    #[test]
    fn test_decl_mem_rsp() {
        // FF 0C 24 = decl [rsp]
        assert_eq!(dis(&[0xFF, 0x0C, 0x24]), "decl [rsp]");
    }

    #[test]
    fn test_decq_mem_rsp() {
        // 48 FF 0C 24 = decq [rsp]
        assert_eq!(dis(&[0x48, 0xFF, 0x0C, 0x24]), "decq [rsp]");
    }

    // -----------------------------------------------------------------------
    // 26. lea
    //     48 8D 05 xx xx xx xx = leaq rax,[rip+disp32]
    // -----------------------------------------------------------------------
    #[test]
    fn test_leaq_rax_rip() {
        assert_eq!(
            dis(&[0x48, 0x8D, 0x05, 0x10, 0x00, 0x00, 0x00]),
            "leaq rax,[rip+0x10]"
        );
    }

    // -----------------------------------------------------------------------
    // 27. shl / shr / sar
    //     48 C1 E0 04 = shlq rax,4
    //     48 C1 E8 04 = shrq rax,4
    //     48 C1 F8 01 = sarq rax,1
    // -----------------------------------------------------------------------
    #[test]
    fn test_shlq_rax_imm() {
        assert_eq!(dis(&[0x48, 0xC1, 0xE0, 0x04]), "shlq rax,4");
    }

    #[test]
    fn test_shrq_rax_imm() {
        assert_eq!(dis(&[0x48, 0xC1, 0xE8, 0x04]), "shrq rax,4");
    }

    #[test]
    fn test_sarq_rax_1() {
        // 48 D1 F8 = sarq rax,1
        assert_eq!(dis(&[0x48, 0xD1, 0xF8]), "sarq rax,1");
    }

    #[test]
    fn test_shlq_rax_cl() {
        // 48 D3 E0 = shlq rax,cl
        assert_eq!(dis(&[0x48, 0xD3, 0xE0]), "shlq rax,cl");
    }

    // -----------------------------------------------------------------------
    // 28. jmp short (EB)
    // -----------------------------------------------------------------------
    #[test]
    fn test_jmp_short() {
        // EB 0B = jmp +13 (disp=0x0B, total=0x0B+2=13)
        assert_eq!(dis(&[0xEB, 0x0B]), "jmp +13");
    }

    // -----------------------------------------------------------------------
    // 29. jcc short (7x)
    // -----------------------------------------------------------------------
    #[test]
    fn test_jz_short() {
        // 74 05 = jz +7 (disp=5, +2=7)
        assert_eq!(dis(&[0x74, 0x05]), "jz +7");
    }

    #[test]
    fn test_jl_short_neg() {
        // 7C F5 = jl -9 (disp=0xF5=-11, +2=-9)
        assert_eq!(dis(&[0x7C, 0xF5]), "jl -9");
    }

    // -----------------------------------------------------------------------
    // 30. call rel32
    //     E8 xx xx xx xx
    // -----------------------------------------------------------------------
    #[test]
    fn test_call_rel32() {
        // E8 0A 00 00 00 = call +15 (disp=10, +5=15)
        assert_eq!(dis(&[0xE8, 0x0A, 0x00, 0x00, 0x00]), "call +15");
    }

    // -----------------------------------------------------------------------
    // 31. jmp rel32
    //     E9 xx xx xx xx
    // -----------------------------------------------------------------------
    #[test]
    fn test_jmp_rel32() {
        assert_eq!(dis(&[0xE9, 0x0A, 0x00, 0x00, 0x00]), "jmp +15");
    }

    // -----------------------------------------------------------------------
    // 32. call indirect
    //     FF 15 xx = call [rip+disp32] (FF /2)
    //     FF D0 = call rax
    // -----------------------------------------------------------------------
    #[test]
    fn test_call_rax() {
        assert_eq!(dis(&[0xFF, 0xD0]), "call rax");
    }

    // -----------------------------------------------------------------------
    // 33. jmp indirect
    //     FF E0 = jmp rax
    // -----------------------------------------------------------------------
    #[test]
    fn test_jmp_rax() {
        assert_eq!(dis(&[0xFF, 0xE0]), "jmp rax");
    }

    // -----------------------------------------------------------------------
    // 34. movzxb / movzxw / movsxb / movsxw
    // -----------------------------------------------------------------------
    #[test]
    fn test_movzxbq() {
        // 48 0F B6 04 24 = movzxbq rax,[rsp]
        assert_eq!(dis(&[0x48, 0x0F, 0xB6, 0x04, 0x24]), "movzxbq rax,[rsp]");
    }

    #[test]
    fn test_movzxwq() {
        // 48 0F B7 00 = movzxwq rax,[rax]
        assert_eq!(dis(&[0x48, 0x0F, 0xB7, 0x00]), "movzxwq rax,[rax]");
    }

    #[test]
    fn test_movsxbq() {
        // 48 0F BE C1 = movsxbq rax,rcx
        assert_eq!(dis(&[0x48, 0x0F, 0xBE, 0xC1]), "movsxbq rax,rcx");
    }

    #[test]
    fn test_movsxwq() {
        // 4C 0F BF 04 24 = movsxwq r8,[rsp]
        assert_eq!(dis(&[0x4C, 0x0F, 0xBF, 0x04, 0x24]), "movsxwq r8,[rsp]");
    }

    // -----------------------------------------------------------------------
    // 35. movsxd
    //     48 63 C1 = movsxdq rax,rcx (MOVSXD with REX.W)
    // -----------------------------------------------------------------------
    #[test]
    fn test_movsxdq() {
        assert_eq!(dis(&[0x48, 0x63, 0xC1]), "movsxdq rax,rcx");
    }

    // -----------------------------------------------------------------------
    // 36. imul reg,reg
    //     48 0F AF C1 = imulq rax,rcx
    // -----------------------------------------------------------------------
    #[test]
    fn test_imulq_rax_rcx() {
        assert_eq!(dis(&[0x48, 0x0F, 0xAF, 0xC1]), "imulq rax,rcx");
    }

    // -----------------------------------------------------------------------
    // 37. imul reg,reg,imm
    //     48 6B C1 04 = imulq rax,rcx,4
    // -----------------------------------------------------------------------
    #[test]
    fn test_imulq_rax_rcx_imm8() {
        assert_eq!(dis(&[0x48, 0x6B, 0xC1, 0x04]), "imulq rax,rcx,4");
    }

    #[test]
    fn test_imull_rax_rax_imm32() {
        // 69 C0 E8 03 00 00 = imull rax,rax,0x3e8
        assert_eq!(dis(&[0x69, 0xC0, 0xE8, 0x03, 0x00, 0x00]), "imull rax,rax,0x3e8");
    }

    // -----------------------------------------------------------------------
    // 38. push imm8, push imm32
    // -----------------------------------------------------------------------
    #[test]
    fn test_push_imm8() {
        assert_eq!(dis(&[0x6A, 0x00]), "push 0");
    }

    #[test]
    fn test_push_imm32() {
        // 68 FF FF FF 7F = push 0x7fffffff
        assert_eq!(dis(&[0x68, 0xFF, 0xFF, 0xFF, 0x7F]), "push 0x7fffffff");
    }

    // -----------------------------------------------------------------------
    // 39. setcc
    //     0F 94 C0 = setz al
    // -----------------------------------------------------------------------
    #[test]
    fn test_setz() {
        assert_eq!(dis(&[0x0F, 0x94, 0xC0]), "setz al");
    }

    #[test]
    fn test_setl() {
        assert_eq!(dis(&[0x0F, 0x9C, 0xC0]), "setl al");
    }

    // -----------------------------------------------------------------------
    // 40. cmovcc
    //     48 0F 44 C1 = cmovzq rax,rcx
    // -----------------------------------------------------------------------
    #[test]
    fn test_cmovzq() {
        assert_eq!(dis(&[0x48, 0x0F, 0x44, 0xC1]), "cmovzq rax,rcx");
    }

    // -----------------------------------------------------------------------
    // 41. hlt
    // -----------------------------------------------------------------------
    #[test]
    fn test_hlt() {
        assert_eq!(dis(&[0xF4]), "hlt");
    }

    // -----------------------------------------------------------------------
    // 42. leave
    // -----------------------------------------------------------------------
    #[test]
    fn test_leave() {
        assert_eq!(dis(&[0xC9]), "leave");
    }

    // -----------------------------------------------------------------------
    // 43. cld / std
    // -----------------------------------------------------------------------
    #[test]
    fn test_cld() {
        assert_eq!(dis(&[0xFC]), "cld");
    }

    #[test]
    fn test_std() {
        assert_eq!(dis(&[0xFD]), "std");
    }

    // -----------------------------------------------------------------------
    // 44. SIB addressing: base+index*scale
    //     48 8B 04 48 = movq rax,[rax+rcx*2]
    // -----------------------------------------------------------------------
    #[test]
    fn test_movq_rax_sib_base_index_scale() {
        assert_eq!(dis(&[0x48, 0x8B, 0x04, 0x48]), "movq rax,[rax+rcx*2]");
    }

    // -----------------------------------------------------------------------
    // 45. SIB with displacement
    //     48 8B 44 48 08 = movq rax,[rax+rcx*2+0x8]
    // -----------------------------------------------------------------------
    #[test]
    fn test_movq_sib_disp8() {
        assert_eq!(dis(&[0x48, 0x8B, 0x44, 0x48, 0x08]), "movq rax,[rax+rcx*2+0x8]");
    }

    // -----------------------------------------------------------------------
    // 46. SIB with R12 base (needs SIB escape)
    //     49 8B 44 24 08 = movq rax,[r12+0x8]
    // -----------------------------------------------------------------------
    #[test]
    fn test_movq_rax_r12_disp8() {
        assert_eq!(dis(&[0x49, 0x8B, 0x44, 0x24, 0x08]), "movq rax,[r12+0x8]");
    }

    // -----------------------------------------------------------------------
    // 47. SIB with R13 base (needs disp8=0)
    //     49 8B 45 08 = movq rax,[r13+0x8]
    // -----------------------------------------------------------------------
    #[test]
    fn test_movq_rax_r13_disp8() {
        assert_eq!(dis(&[0x49, 0x8B, 0x45, 0x08]), "movq rax,[r13+0x8]");
    }

    // -----------------------------------------------------------------------
    // 48. lock prefix
    //     F0 48 FF 04 24 = lock incq [rsp]
    // -----------------------------------------------------------------------
    #[test]
    fn test_lock_incq() {
        assert_eq!(dis(&[0xF0, 0x48, 0xFF, 0x04, 0x24]), "lock incq [rsp]");
    }

    // -----------------------------------------------------------------------
    // 49. Multi-instruction disassembly
    // -----------------------------------------------------------------------
    #[test]
    fn test_multi_instruction() {
        // push rax; pop rdx; ret
        let result = dis_all(&[0x50, 0x5A, 0xC3]);
        assert_eq!(result, "push rax\npop rdx\nret\n");
    }

    // -----------------------------------------------------------------------
    // 50. Addressing mode: rbp+0 (mod=01, disp8=0)
    //     48 8B 45 00 = movq rax,[rbp+0]
    // -----------------------------------------------------------------------
    #[test]
    fn test_movq_rax_rbp_disp0() {
        assert_eq!(dis(&[0x48, 0x8B, 0x45, 0x00]), "movq rax,[rbp+0]");
    }

    // -----------------------------------------------------------------------
    // 51. Addressing mode: base+index*scale with no disp (mod=0, SIB)
    //     48 8B 04 68 = movq rax,[rax+rbp*2]
    // -----------------------------------------------------------------------
    #[test]
    fn test_movq_sib_base_index_no_disp() {
        assert_eq!(dis(&[0x48, 0x8B, 0x04, 0x68]), "movq rax,[rax+rbp*2]");
    }

    // -----------------------------------------------------------------------
    // 52. neg
    //     48 F7 D8 = negq rax
    // -----------------------------------------------------------------------
    #[test]
    fn test_negq_rax() {
        assert_eq!(dis(&[0x48, 0xF7, 0xD8]), "negq rax");
    }

    // -----------------------------------------------------------------------
    // 53. not
    //     48 F7 D0 = notq rax
    // -----------------------------------------------------------------------
    #[test]
    fn test_notq_rax() {
        assert_eq!(dis(&[0x48, 0xF7, 0xD0]), "notq rax");
    }

    // -----------------------------------------------------------------------
    // 54. idiv
    //     48 F7 F9 = idivq (rax,rdx),rcx
    // -----------------------------------------------------------------------
    #[test]
    fn test_idivq_rcx() {
        assert_eq!(dis(&[0x48, 0xF7, 0xF9]), "idivq (rax,rdx),rcx");
    }

    // -----------------------------------------------------------------------
    // 55. test imm
    //     48 F7 C1 xx xx xx xx = testq rcx,imm32
    // -----------------------------------------------------------------------
    #[test]
    fn test_testq_rcx_imm() {
        assert_eq!(
            dis(&[0x48, 0xF7, 0xC1, 0x01, 0x00, 0x00, 0x00]),
            "testq rcx,1"
        );
    }

    // -----------------------------------------------------------------------
    // 56. cqo (REX.W + CDQ)
    // -----------------------------------------------------------------------
    #[test]
    fn test_cqo() {
        // 48 99 = cqo
        assert_eq!(dis(&[0x48, 0x99]), "cqo");
    }

    // -----------------------------------------------------------------------
    // 57. cdq (no REX.W)
    // -----------------------------------------------------------------------
    #[test]
    fn test_cdq() {
        assert_eq!(dis(&[0x99]), "cdq");
    }

    // -----------------------------------------------------------------------
    // 58. bsf / bsr
    //     48 0F BC C8 = bsfq rcx,rax
    //     48 0F BD C8 = bsrq rcx,rax
    // -----------------------------------------------------------------------
    #[test]
    fn test_bsfq() {
        assert_eq!(dis(&[0x48, 0x0F, 0xBC, 0xC8]), "bsfq rcx,rax");
    }

    #[test]
    fn test_bsrq() {
        assert_eq!(dis(&[0x48, 0x0F, 0xBD, 0xC8]), "bsrq rcx,rax");
    }

    // -----------------------------------------------------------------------
    // 59. Instruction length correctness
    // -----------------------------------------------------------------------
    #[test]
    fn test_instruction_lengths() {
        assert_eq!(dis_len(&[0xC3]).1, 1); // ret
        assert_eq!(dis_len(&[0x50]).1, 1); // push rax
        assert_eq!(dis_len(&[0x48, 0x89, 0xC8]).1, 3); // movq rax,rcx
        assert_eq!(dis_len(&[0x48, 0x8B, 0x45, 0x08]).1, 4); // movq rax,[rbp+0x8]
        assert_eq!(dis_len(&[0xB8, 0x2A, 0x00, 0x00, 0x00]).1, 5); // movl rax,0x2a
        assert_eq!(dis_len(&[0x48, 0xB8, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08]).1, 10); // movq rax,imm64
    }

    // -----------------------------------------------------------------------
    // 60. mov [mem], imm  (C7 /0)
    //     48 C7 04 24 00 00 00 00 = movq [rsp],0
    // -----------------------------------------------------------------------
    #[test]
    fn test_movq_mem_imm() {
        assert_eq!(
            dis(&[0x48, 0xC7, 0x04, 0x24, 0x00, 0x00, 0x00, 0x00]),
            "movq [rsp],0"
        );
    }

    // -----------------------------------------------------------------------
    // 61. SIB: index only (no base, mod=0, base=5)
    //     48 8B 04 4D 00 00 00 00 = movq rax,[rcx*2+0]
    //   Here SIB: scale=1(TIMES_2), index=rcx(1), base=5(no base mod=0)
    // -----------------------------------------------------------------------
    #[test]
    fn test_movq_sib_index_only() {
        assert_eq!(
            dis(&[0x48, 0x8B, 0x04, 0x4D, 0x00, 0x00, 0x00, 0x00]),
            "movq rax,[rcx*2+0]"
        );
    }

    // -----------------------------------------------------------------------
    // 62. addq rsp, imm8
    //     48 83 C4 08 = addq rsp,8
    // -----------------------------------------------------------------------
    #[test]
    fn test_addq_rsp_imm8() {
        assert_eq!(dis(&[0x48, 0x83, 0xC4, 0x08]), "addq rsp,8");
    }

    // -----------------------------------------------------------------------
    // 63. movl with negative imm (sign-extended printing)
    //     B8 FF FF FF FF = movl rax,0xffffffff
    // -----------------------------------------------------------------------
    #[test]
    fn test_movl_rax_neg1() {
        assert_eq!(dis(&[0xB8, 0xFF, 0xFF, 0xFF, 0xFF]), "movl rax,0xffffffff");
    }

    // -----------------------------------------------------------------------
    // 64. jcc long (0F 8x)
    //     0F 84 05 00 00 00 = jz +11 (disp=5, +6=11)
    // -----------------------------------------------------------------------
    #[test]
    fn test_jz_long() {
        assert_eq!(dis(&[0x0F, 0x84, 0x05, 0x00, 0x00, 0x00]), "jz +11");
    }

    // -----------------------------------------------------------------------
    // 65. pushfq / popfq
    // -----------------------------------------------------------------------
    #[test]
    fn test_pushfq() {
        assert_eq!(dis(&[0x9C]), "pushfq");
    }

    #[test]
    fn test_popfq() {
        assert_eq!(dis(&[0x9D]), "popfq");
    }

    // -----------------------------------------------------------------------
    // 66. jmp [reg+disp8]
    //     FF 67 28 = jmp [rdi+0x28]
    // -----------------------------------------------------------------------
    #[test]
    fn test_jmp_mem_disp() {
        assert_eq!(dis(&[0xFF, 0x67, 0x28]), "jmp [rdi+0x28]");
    }

    // -----------------------------------------------------------------------
    // 67. movq r10,[rax]
    //     4C 8B 10 = movq r10,[rax]
    // -----------------------------------------------------------------------
    #[test]
    fn test_movq_r10_mem_rax() {
        assert_eq!(dis(&[0x4C, 0x8B, 0x10]), "movq r10,[rax]");
    }

    // -----------------------------------------------------------------------
    // 68. SIB: rsp+rbp*2
    //     48 8B 04 6C = movq rax,[rsp+rbp*2]
    // -----------------------------------------------------------------------
    #[test]
    fn test_sib_rsp_rbp_times2() {
        assert_eq!(dis(&[0x48, 0x8B, 0x04, 0x6C]), "movq rax,[rsp+rbp*2]");
    }

    // -----------------------------------------------------------------------
    // 69. SIB: rbp+rax*2+0 (mod=1, disp8=0 because rbp base needs mod!=0)
    //     48 8B 44 45 00 = movq rax,[rbp+rax*2+0]
    // -----------------------------------------------------------------------
    #[test]
    fn test_sib_rbp_rax_times2_disp0() {
        assert_eq!(dis(&[0x48, 0x8B, 0x44, 0x45, 0x00]), "movq rax,[rbp+rax*2+0]");
    }

    // -----------------------------------------------------------------------
    // SIB: r13+rax*2+0 (r13 base requires disp8=0, like rbp)
    //   REX=0x49(W=1,B=1), modrm=0x44(mod=01,rm=SIB), SIB=0x45, disp8=0
    // -----------------------------------------------------------------------
    #[test]
    fn test_sib_r13_rax_times2_disp0() {
        assert_eq!(dis(&[0x49, 0x8B, 0x44, 0x45, 0x00]), "movq rax,[r13+rax*2+0]");
    }

    // -----------------------------------------------------------------------
    // Additional: verify disassemble_one returns correct length for multi-byte
    // -----------------------------------------------------------------------
    #[test]
    fn test_disassemble_one_length() {
        let code = [0x48, 0x83, 0xC0, 0x02, 0xC3]; // addq rax,2; ret
        let (text, len) = disassemble_one(&code, 0);
        assert_eq!(text, "addq rax,2");
        assert_eq!(len, 4);
    }

    // -----------------------------------------------------------------------
    // SimpleLoop test (multi-instruction matching Dart test output)
    // -----------------------------------------------------------------------
    #[test]
    fn test_simple_loop() {
        // Upstream Dart SimpleLoop test (from assembler_x64_test.cc).
        let code: Vec<u8> = vec![
            0xB8, 0x00, 0x00, 0x00, 0x00,       // movl rax,0
            0xB9, 0x00, 0x00, 0x00, 0x00,       // movl rcx,0
            0x48, 0x83, 0xC0, 0x02,              // addq rax,2
            0x48, 0xFF, 0xC1,                    // incq rcx
            0x48, 0x83, 0xF9, 0x57,              // cmpq rcx,0x57
            0x7C, 0xF3,                          // jl (byte=-13, disp=-11)
            0xC3,                                // ret
        ];
        let result = dis_all(&code);
        assert_eq!(
            result,
            "movl rax,0\n\
             movl rcx,0\n\
             addq rax,2\n\
             incq rcx\n\
             cmpq rcx,0x57\n\
             jl -11\n\
             ret\n"
        );
    }

    // -----------------------------------------------------------------------
    // Addressing modes from Dart test
    // -----------------------------------------------------------------------
    #[test]
    fn test_movq_rax_mem_rsp_0() {
        // 48 8B 04 24 = movq rax,[rsp]
        assert_eq!(dis(&[0x48, 0x8B, 0x04, 0x24]), "movq rax,[rsp]");
    }

    // Additional SIB modes with R12 base and index
    #[test]
    fn test_movq_rax_r12_rbp_times2() {
        // 4A 8B 04 6C -> needs REX.B for r12
        // REX: 0x49 (W=1, B=1), modrm=0x04 (mod=0, reg=rax, rm=100->SIB)
        // SIB: 0x6C (scale=1, index=rbp(5), base=4(rsp) -> but REX.B makes base=r12)
        assert_eq!(dis(&[0x49, 0x8B, 0x04, 0x6C]), "movq rax,[r12+rbp*2]");
    }

    // -----------------------------------------------------------------------
    // Negative immediate in movl
    // -----------------------------------------------------------------------
    #[test]
    fn test_movl_rax_neg87() {
        // movl rax,-0x00000057
        // B8 A9 FF FF FF = movl rax, 0xffffffa9 which is -87 as i32
        // But Dart prints "movl rax,-0x00000057"
        // That's via the MOVE_REG_INSTR path with PrintImmediateValue(addr, false, 4)
        // since signed_value=false, it just prints the hex of the u32.
        // For -87: 0xFFFFFF_A9, which as u32 > 0xFFFF so prints 0xffffffa9
        assert_eq!(
            dis(&[0xB8, 0xA9, 0xFF, 0xFF, 0xFF]),
            "movl rax,0xffffffa9"
        );
    }

    // -----------------------------------------------------------------------
    // andl with imm32 (short immediate form: opcode 0x25)
    //   25 FF FF FF FF = andl rax,0xffffffff
    // -----------------------------------------------------------------------
    #[test]
    fn test_andl_rax_imm32_short() {
        assert_eq!(
            dis(&[0x25, 0xFF, 0xFF, 0xFF, 0xFF]),
            "andl rax,0xffffffff"
        );
    }

    // -----------------------------------------------------------------------
    // SIB: rsp+disp32
    //   48 8B 84 24 00 08 00 00 = movq rax,[rsp+0x800]
    // -----------------------------------------------------------------------
    #[test]
    fn test_movq_rax_rsp_disp32() {
        assert_eq!(
            dis(&[0x48, 0x8B, 0x84, 0x24, 0x00, 0x08, 0x00, 0x00]),
            "movq rax,[rsp+0x800]"
        );
    }

    #[test]
    fn test_movq_rax_rsp_neg_disp32() {
        assert_eq!(
            dis(&[0x48, 0x8B, 0x84, 0x24, 0x00, 0xF8, 0xFF, 0xFF]),
            "movq rax,[rsp-0x800]"
        );
    }

    // =======================================================================
    // Upstream Dart disassembler tests ported below
    // =======================================================================

    // -----------------------------------------------------------------------
    // Addressing modes: base+disp with various registers (from AddressingModes)
    // -----------------------------------------------------------------------
    #[test]
    fn test_addr_movq_rax_mem_rbp_0() {
        // movq rax,[rbp+0]  -- rbp base always needs explicit disp
        // 48 8B 45 00
        assert_eq!(dis(&[0x48, 0x8B, 0x45, 0x00]), "movq rax,[rbp+0]");
    }

    #[test]
    fn test_addr_movq_rax_mem_rax() {
        // movq rax,[rax]  -- 48 8B 00
        assert_eq!(dis(&[0x48, 0x8B, 0x00]), "movq rax,[rax]");
    }

    #[test]
    fn test_addr_movq_r10_mem_rax() {
        // movq r10,[rax] -- 4C 8B 10
        assert_eq!(dis(&[0x4C, 0x8B, 0x10]), "movq r10,[rax]");
    }

    #[test]
    fn test_addr_movq_rax_mem_r10_disp8() {
        // movq rax,[r10+0x8] -- 49 8B 42 08
        assert_eq!(dis(&[0x49, 0x8B, 0x42, 0x08]), "movq rax,[r10+0x8]");
    }

    #[test]
    fn test_addr_movq_rax_mem_r12_disp8() {
        // movq rax,[r12+0x8] -- 49 8B 44 24 08  (r12 base needs SIB)
        assert_eq!(dis(&[0x49, 0x8B, 0x44, 0x24, 0x08]), "movq rax,[r12+0x8]");
    }

    #[test]
    fn test_addr_movq_rax_mem_r13_disp8() {
        // movq rax,[r13+0x8] -- 49 8B 45 08
        assert_eq!(dis(&[0x49, 0x8B, 0x45, 0x08]), "movq rax,[r13+0x8]");
    }

    #[test]
    fn test_addr_movq_rax_mem_r10_neg_disp8() {
        // movq rax,[r10-0x8] -- 49 8B 42 F8
        assert_eq!(dis(&[0x49, 0x8B, 0x42, 0xF8]), "movq rax,[r10-0x8]");
    }

    #[test]
    fn test_addr_movq_rax_mem_r10_disp32() {
        // movq rax,[r10+0x800] -- 49 8B 82 00 08 00 00
        assert_eq!(
            dis(&[0x49, 0x8B, 0x82, 0x00, 0x08, 0x00, 0x00]),
            "movq rax,[r10+0x800]"
        );
    }

    #[test]
    fn test_addr_movq_rax_mem_r12_disp32() {
        // movq rax,[r12+0x800] -- 49 8B 84 24 00 08 00 00
        assert_eq!(
            dis(&[0x49, 0x8B, 0x84, 0x24, 0x00, 0x08, 0x00, 0x00]),
            "movq rax,[r12+0x800]"
        );
    }

    #[test]
    fn test_addr_movq_rax_mem_r13_disp32() {
        // movq rax,[r13+0x800] -- 49 8B 85 00 08 00 00
        assert_eq!(
            dis(&[0x49, 0x8B, 0x85, 0x00, 0x08, 0x00, 0x00]),
            "movq rax,[r13+0x800]"
        );
    }

    // -----------------------------------------------------------------------
    // SIB addressing: base+index*scale  (from AddressingModes)
    // -----------------------------------------------------------------------
    #[test]
    fn test_addr_sib_rax_rbp_times2() {
        // movq rax,[rax+rbp*2] -- 48 8B 04 68
        assert_eq!(dis(&[0x48, 0x8B, 0x04, 0x68]), "movq rax,[rax+rbp*2]");
    }

    #[test]
    fn test_addr_sib_rax_rax_times2() {
        // movq rax,[rax+rax*2] -- 48 8B 04 40
        assert_eq!(dis(&[0x48, 0x8B, 0x04, 0x40]), "movq rax,[rax+rax*2]");
    }

    #[test]
    fn test_addr_sib_rax_r10_times2() {
        // movq rax,[rax+r10*2] -- 4A 8B 04 50
        assert_eq!(dis(&[0x4A, 0x8B, 0x04, 0x50]), "movq rax,[rax+r10*2]");
    }

    #[test]
    fn test_addr_sib_rax_r12_times2() {
        // movq rax,[rax+r12*2] -- 4A 8B 04 60
        assert_eq!(dis(&[0x4A, 0x8B, 0x04, 0x60]), "movq rax,[rax+r12*2]");
    }

    #[test]
    fn test_addr_sib_rax_r13_times2() {
        // movq rax,[rax+r13*2] -- 4A 8B 04 68
        assert_eq!(dis(&[0x4A, 0x8B, 0x04, 0x68]), "movq rax,[rax+r13*2]");
    }

    #[test]
    fn test_addr_sib_rsp_rbp_times2() {
        // movq rax,[rsp+rbp*2] -- 48 8B 04 6C
        assert_eq!(dis(&[0x48, 0x8B, 0x04, 0x6C]), "movq rax,[rsp+rbp*2]");
    }

    #[test]
    fn test_addr_sib_rsp_rax_times2() {
        // movq rax,[rsp+rax*2] -- 48 8B 04 44
        assert_eq!(dis(&[0x48, 0x8B, 0x04, 0x44]), "movq rax,[rsp+rax*2]");
    }

    #[test]
    fn test_addr_sib_r10_rbp_times2() {
        // movq rax,[r10+rbp*2] -- 49 8B 04 6A
        assert_eq!(dis(&[0x49, 0x8B, 0x04, 0x6A]), "movq rax,[r10+rbp*2]");
    }

    #[test]
    fn test_addr_sib_r10_r10_times2() {
        // movq rax,[r10+r10*2] -- 4B 8B 04 52
        assert_eq!(dis(&[0x4B, 0x8B, 0x04, 0x52]), "movq rax,[r10+r10*2]");
    }

    // SIB with base+index*scale+disp8
    #[test]
    fn test_addr_sib_rax_rbp_times2_disp8() {
        // movq rax,[rax+rbp*2+0x8] -- 48 8B 44 68 08
        assert_eq!(dis(&[0x48, 0x8B, 0x44, 0x68, 0x08]), "movq rax,[rax+rbp*2+0x8]");
    }

    #[test]
    fn test_addr_sib_rbp_rbp_times2_disp8() {
        // movq rax,[rbp+rbp*2+0x8] -- 48 8B 44 6D 08
        assert_eq!(dis(&[0x48, 0x8B, 0x44, 0x6D, 0x08]), "movq rax,[rbp+rbp*2+0x8]");
    }

    #[test]
    fn test_addr_sib_rsp_r10_times2_disp8() {
        // movq rax,[rsp+r10*2+0x8] -- 4A 8B 44 54 08
        assert_eq!(dis(&[0x4A, 0x8B, 0x44, 0x54, 0x08]), "movq rax,[rsp+r10*2+0x8]");
    }

    // SIB with base+index*scale+disp32
    #[test]
    fn test_addr_sib_rax_rbp_times2_disp32() {
        // movq rax,[rax+rbp*2+0x800] -- 48 8B 84 68 00 08 00 00
        assert_eq!(
            dis(&[0x48, 0x8B, 0x84, 0x68, 0x00, 0x08, 0x00, 0x00]),
            "movq rax,[rax+rbp*2+0x800]"
        );
    }

    // -----------------------------------------------------------------------
    // SIB: index-only addressing (no base, from AddressingModes)
    // -----------------------------------------------------------------------
    #[test]
    fn test_addr_sib_index_times1() {
        // movq rax,[rax*1+0] -- 48 8B 04 05 00 00 00 00
        assert_eq!(
            dis(&[0x48, 0x8B, 0x04, 0x05, 0x00, 0x00, 0x00, 0x00]),
            "movq rax,[rax*1+0]"
        );
    }

    #[test]
    fn test_addr_sib_index_times2() {
        // movq rax,[rax*2+0] -- 48 8B 04 45 00 00 00 00
        assert_eq!(
            dis(&[0x48, 0x8B, 0x04, 0x45, 0x00, 0x00, 0x00, 0x00]),
            "movq rax,[rax*2+0]"
        );
    }

    #[test]
    fn test_addr_sib_index_times4() {
        // movq rax,[rax*4+0] -- 48 8B 04 85 00 00 00 00
        assert_eq!(
            dis(&[0x48, 0x8B, 0x04, 0x85, 0x00, 0x00, 0x00, 0x00]),
            "movq rax,[rax*4+0]"
        );
    }

    #[test]
    fn test_addr_sib_index_times8() {
        // movq rax,[rax*8+0] -- 48 8B 04 C5 00 00 00 00
        assert_eq!(
            dis(&[0x48, 0x8B, 0x04, 0xC5, 0x00, 0x00, 0x00, 0x00]),
            "movq rax,[rax*8+0]"
        );
    }

    #[test]
    fn test_addr_sib_rbp_times2_disp8() {
        // movq rax,[rbp*2+0x8] -- 48 8B 04 6D 08 00 00 00
        assert_eq!(
            dis(&[0x48, 0x8B, 0x04, 0x6D, 0x08, 0x00, 0x00, 0x00]),
            "movq rax,[rbp*2+0x8]"
        );
    }

    #[test]
    fn test_addr_sib_r10_times2_disp0() {
        // movq rax,[r10*2+0] -- 4A 8B 04 55 00 00 00 00
        assert_eq!(
            dis(&[0x4A, 0x8B, 0x04, 0x55, 0x00, 0x00, 0x00, 0x00]),
            "movq rax,[r10*2+0]"
        );
    }

    #[test]
    fn test_addr_sib_r13_times2_disp0() {
        // movq rax,[r13*2+0] -- 4A 8B 04 6D 00 00 00 00
        assert_eq!(
            dis(&[0x4A, 0x8B, 0x04, 0x6D, 0x00, 0x00, 0x00, 0x00]),
            "movq rax,[r13*2+0]"
        );
    }

    // -----------------------------------------------------------------------
    // RIP-relative addressing
    // -----------------------------------------------------------------------
    #[test]
    fn test_addr_rip_relative_positive() {
        // movq rax,[rip+0x100] -- 48 8B 05 00 01 00 00
        assert_eq!(
            dis(&[0x48, 0x8B, 0x05, 0x00, 0x01, 0x00, 0x00]),
            "movq rax,[rip+0x100]"
        );
    }

    #[test]
    fn test_addr_rip_relative_negative() {
        // movq rax,[rip-0x10] -- 48 8B 05 F0 FF FF FF
        assert_eq!(
            dis(&[0x48, 0x8B, 0x05, 0xF0, 0xFF, 0xFF, 0xFF]),
            "movq rax,[rip-0x10]"
        );
    }

    // -----------------------------------------------------------------------
    // SSE/SSE2: movsd, movss, addsd, mulsd, subsd, divsd, sqrtsd
    // -----------------------------------------------------------------------
    #[test]
    fn test_movsd_xmm0_xmm1() {
        // F2 0F 10 C1 = movsd xmm0,xmm1
        assert_eq!(dis(&[0xF2, 0x0F, 0x10, 0xC1]), "movsd xmm0,xmm1");
    }

    #[test]
    fn test_movsd_xmm1_mem_rsp() {
        // F2 0F 10 0C 24 = movsd xmm1,[rsp]
        assert_eq!(dis(&[0xF2, 0x0F, 0x10, 0x0C, 0x24]), "movsd xmm1,[rsp]");
    }

    #[test]
    fn test_movsd_mem_rsp_xmm0() {
        // F2 0F 11 04 24 = movsd [rsp],xmm0
        assert_eq!(dis(&[0xF2, 0x0F, 0x11, 0x04, 0x24]), "movsd [rsp],xmm0");
    }

    #[test]
    fn test_addsd_xmm0_xmm1() {
        // F2 0F 58 C1 = addsd xmm0,xmm1
        assert_eq!(dis(&[0xF2, 0x0F, 0x58, 0xC1]), "addsd xmm0,xmm1");
    }

    #[test]
    fn test_mulsd_xmm0_xmm1() {
        // F2 0F 59 C1 = mulsd xmm0,xmm1
        assert_eq!(dis(&[0xF2, 0x0F, 0x59, 0xC1]), "mulsd xmm0,xmm1");
    }

    #[test]
    fn test_subsd_xmm0_xmm1() {
        // F2 0F 5C C1 = subsd xmm0,xmm1
        assert_eq!(dis(&[0xF2, 0x0F, 0x5C, 0xC1]), "subsd xmm0,xmm1");
    }

    #[test]
    fn test_divsd_xmm0_xmm1() {
        // F2 0F 5E C1 = divsd xmm0,xmm1
        assert_eq!(dis(&[0xF2, 0x0F, 0x5E, 0xC1]), "divsd xmm0,xmm1");
    }

    #[test]
    fn test_sqrtsd_xmm0_xmm1() {
        // F2 0F 51 C1 = sqrtsd xmm0,xmm1
        assert_eq!(dis(&[0xF2, 0x0F, 0x51, 0xC1]), "sqrtsd xmm0,xmm1");
    }

    #[test]
    fn test_minsd_xmm0_xmm1() {
        // F2 0F 5D C1 = minsd xmm0,xmm1
        assert_eq!(dis(&[0xF2, 0x0F, 0x5D, 0xC1]), "minsd xmm0,xmm1");
    }

    #[test]
    fn test_maxsd_xmm0_xmm1() {
        // F2 0F 5F C1 = maxsd xmm0,xmm1
        assert_eq!(dis(&[0xF2, 0x0F, 0x5F, 0xC1]), "maxsd xmm0,xmm1");
    }

    // SSE: movss
    #[test]
    fn test_movss_xmm0_xmm1() {
        // F3 0F 10 C1 = movss xmm0,rcx (Dart-style: uses CPU reg name for reg-to-reg movss)
        assert_eq!(dis(&[0xF3, 0x0F, 0x10, 0xC1]), "movss xmm0,rcx");
    }

    #[test]
    fn test_movss_xmm1_mem_rsp() {
        // F3 0F 10 0C 24 = movss xmm1,[rsp]
        assert_eq!(dis(&[0xF3, 0x0F, 0x10, 0x0C, 0x24]), "movss xmm1,[rsp]");
    }

    #[test]
    fn test_movss_mem_rsp_xmm0() {
        // F3 0F 11 04 24 = movss [rsp],xmm0
        assert_eq!(dis(&[0xF3, 0x0F, 0x11, 0x04, 0x24]), "movss [rsp],xmm0");
    }

    // SSE: addss, mulss, subss, divss
    #[test]
    fn test_addss_xmm0_xmm1() {
        // F3 0F 58 C1
        assert_eq!(dis(&[0xF3, 0x0F, 0x58, 0xC1]), "addss xmm0,xmm1");
    }

    #[test]
    fn test_mulss_xmm0_xmm1() {
        // F3 0F 59 C1
        assert_eq!(dis(&[0xF3, 0x0F, 0x59, 0xC1]), "mulss xmm0,xmm1");
    }

    #[test]
    fn test_subss_xmm0_xmm1() {
        // F3 0F 5C C1
        assert_eq!(dis(&[0xF3, 0x0F, 0x5C, 0xC1]), "subss xmm0,xmm1");
    }

    #[test]
    fn test_divss_xmm0_xmm1() {
        // F3 0F 5E C1
        assert_eq!(dis(&[0xF3, 0x0F, 0x5E, 0xC1]), "divss xmm0,xmm1");
    }

    // SSE with high XMM registers
    #[test]
    fn test_addss_xmm8_xmm9() {
        // F3 45 0F 58 C1 = addss xmm8,xmm9
        assert_eq!(dis(&[0xF3, 0x45, 0x0F, 0x58, 0xC1]), "addss xmm8,xmm9");
    }

    #[test]
    fn test_addsd_xmm10_xmm11() {
        // F2 45 0F 58 D3 = addsd xmm10,xmm11
        assert_eq!(dis(&[0xF2, 0x45, 0x0F, 0x58, 0xD3]), "addsd xmm10,xmm11");
    }

    // -----------------------------------------------------------------------
    // SSE conversion instructions
    // -----------------------------------------------------------------------
    #[test]
    fn test_cvtss2sd_xmm0_xmm0() {
        // F3 0F 5A C0
        assert_eq!(dis(&[0xF3, 0x0F, 0x5A, 0xC0]), "cvtss2sd xmm0,xmm0");
    }

    #[test]
    fn test_cvtsd2ss_xmm0_xmm0() {
        // F2 0F 5A C0
        assert_eq!(dis(&[0xF2, 0x0F, 0x5A, 0xC0]), "cvtsd2ss xmm0,xmm0");
    }

    #[test]
    fn test_cvtps2pd_xmm0_xmm0() {
        // 0F 5A C0
        assert_eq!(dis(&[0x0F, 0x5A, 0xC0]), "cvtps2pd xmm0,xmm0");
    }

    #[test]
    fn test_cvtpd2ps_xmm0_xmm0() {
        // 66 0F 5A C0
        assert_eq!(dis(&[0x66, 0x0F, 0x5A, 0xC0]), "cvtpd2ps xmm0,xmm0");
    }

    #[test]
    fn test_cvtsi2sd_xmm0_rax() {
        // F2 48 0F 2A C0 = cvtsi2sd xmm0,rax (REX.W for 64-bit source)
        assert_eq!(dis(&[0xF2, 0x48, 0x0F, 0x2A, 0xC0]), "cvtsi2sd xmm0,rax");
    }

    #[test]
    fn test_cvtsi2ss_xmm0_rax() {
        // F3 48 0F 2A C0 = cvtsi2ss xmm0,rax
        assert_eq!(dis(&[0xF3, 0x48, 0x0F, 0x2A, 0xC0]), "cvtsi2ss xmm0,rax");
    }

    #[test]
    fn test_cvttsd2siq_rax_xmm0() {
        // F2 48 0F 2C C0 = cvttsd2siq rax,xmm0
        assert_eq!(dis(&[0xF2, 0x48, 0x0F, 0x2C, 0xC0]), "cvttsd2siq rax,xmm0");
    }

    #[test]
    fn test_cvttss2siq_rax_xmm0() {
        // F3 48 0F 2C C0 = cvttss2siq rax,xmm0
        assert_eq!(dis(&[0xF3, 0x48, 0x0F, 0x2C, 0xC0]), "cvttss2siq rax,xmm0");
    }

    // -----------------------------------------------------------------------
    // SSE packed operations: addps, mulps, subps, divps, etc.
    // -----------------------------------------------------------------------
    #[test]
    fn test_addps_xmm0_xmm0() {
        // 0F 58 C0
        assert_eq!(dis(&[0x0F, 0x58, 0xC0]), "addps xmm0,xmm0");
    }

    #[test]
    fn test_addpd_xmm0_xmm0() {
        // 66 0F 58 C0
        assert_eq!(dis(&[0x66, 0x0F, 0x58, 0xC0]), "addpd xmm0,xmm0");
    }

    #[test]
    fn test_subpd_xmm10_xmm11() {
        // 66 45 0F 5C D3
        assert_eq!(dis(&[0x66, 0x45, 0x0F, 0x5C, 0xD3]), "subpd xmm10,xmm11");
    }

    #[test]
    fn test_mulpd_xmm10_xmm11() {
        // 66 45 0F 59 D3
        assert_eq!(dis(&[0x66, 0x45, 0x0F, 0x59, 0xD3]), "mulpd xmm10,xmm11");
    }

    #[test]
    fn test_divpd_xmm10_xmm11() {
        // 66 45 0F 5E D3
        assert_eq!(dis(&[0x66, 0x45, 0x0F, 0x5E, 0xD3]), "divpd xmm10,xmm11");
    }

    #[test]
    fn test_sqrtpd_xmm10_xmm10() {
        // 66 45 0F 51 D2
        assert_eq!(dis(&[0x66, 0x45, 0x0F, 0x51, 0xD2]), "sqrtpd xmm10,xmm10");
    }

    #[test]
    fn test_minpd_xmm10_xmm11() {
        // 66 45 0F 5D D3
        assert_eq!(dis(&[0x66, 0x45, 0x0F, 0x5D, 0xD3]), "minpd xmm10,xmm11");
    }

    #[test]
    fn test_maxpd_xmm10_xmm11() {
        // 66 45 0F 5F D3
        assert_eq!(dis(&[0x66, 0x45, 0x0F, 0x5F, 0xD3]), "maxpd xmm10,xmm11");
    }

    // -----------------------------------------------------------------------
    // SSE: movaps, movups, movhlps, movlhps, unpcklps, unpckhps
    // -----------------------------------------------------------------------
    #[test]
    fn test_movaps_xmm0_xmm10() {
        // 41 0F 28 C2 = movaps xmm0,xmm10
        assert_eq!(dis(&[0x41, 0x0F, 0x28, 0xC2]), "movaps xmm0,xmm10");
    }

    #[test]
    fn test_movaps_xmm11_xmm0() {
        // movaps store form (0F 29): xmm0,xmm11
        assert_eq!(dis(&[0x44, 0x0F, 0x29, 0xD8]), "movaps xmm0,xmm11");
    }

    #[test]
    fn test_movups_xmm10_mem_rax() {
        // 44 0F 10 10 = movups xmm10,[rax]
        assert_eq!(dis(&[0x44, 0x0F, 0x10, 0x10]), "movups xmm10,[rax]");
    }

    #[test]
    fn test_movups_mem_rsp_xmm10() {
        // 44 0F 11 14 24 = movups [rsp],xmm10
        assert_eq!(dis(&[0x44, 0x0F, 0x11, 0x14, 0x24]), "movups [rsp],xmm10");
    }

    #[test]
    fn test_movhlps_xmm9_xmm1() {
        // 44 0F 12 C9 = movhlps xmm9,xmm1
        assert_eq!(dis(&[0x44, 0x0F, 0x12, 0xC9]), "movhlps xmm9,xmm1");
    }

    #[test]
    fn test_movlhps_xmm9_xmm1() {
        // 44 0F 16 C9 = movlhps xmm9,xmm1
        assert_eq!(dis(&[0x44, 0x0F, 0x16, 0xC9]), "movlhps xmm9,xmm1");
    }

    #[test]
    fn test_unpcklps_xmm9_xmm1() {
        // 44 0F 14 C9 = unpcklps xmm9,xmm1
        assert_eq!(dis(&[0x44, 0x0F, 0x14, 0xC9]), "unpcklps xmm9,xmm1");
    }

    #[test]
    fn test_unpckhps_xmm9_xmm1() {
        // 44 0F 15 C9 = unpckhps xmm9,xmm1
        assert_eq!(dis(&[0x44, 0x0F, 0x15, 0xC9]), "unpckhps xmm9,xmm1");
    }

    // -----------------------------------------------------------------------
    // SSE: movd (xmm <-> gp)
    // -----------------------------------------------------------------------
    #[test]
    fn test_movd_xmm0_rax() {
        // 66 0F 6E C0 = movd xmm0,rax (without REX.W)
        assert_eq!(dis(&[0x66, 0x0F, 0x6E, 0xC0]), "movd xmm0,rax");
    }

    #[test]
    fn test_movq_xmm0_rax() {
        // 66 48 0F 6E C0 = movq xmm0,rax (with REX.W)
        assert_eq!(dis(&[0x66, 0x48, 0x0F, 0x6E, 0xC0]), "movq xmm0,rax");
    }

    #[test]
    fn test_movd_rax_xmm0() {
        // 66 0F 7E C0 = movd rax,xmm0 (without REX.W)
        assert_eq!(dis(&[0x66, 0x0F, 0x7E, 0xC0]), "movd rax,xmm0");
    }

    #[test]
    fn test_movq_rax_xmm0() {
        // 66 48 0F 7E C0 = movq rax,xmm0 (with REX.W)
        assert_eq!(dis(&[0x66, 0x48, 0x0F, 0x7E, 0xC0]), "movq rax,xmm0");
    }

    // -----------------------------------------------------------------------
    // SSE: shufps, cmpps
    // -----------------------------------------------------------------------
    #[test]
    fn test_shufps_xmm0_xmm0_imm() {
        // 0F C6 C0 00 = shufps xmm0,xmm0 [0]
        assert_eq!(dis(&[0x0F, 0xC6, 0xC0, 0x00]), "shufps xmm0,xmm0 [0]");
    }

    #[test]
    fn test_shufps_xmm0_xmm0_imm_55() {
        // 0F C6 C0 55 = shufps xmm0,xmm0 [55]
        assert_eq!(dis(&[0x0F, 0xC6, 0xC0, 0x55]), "shufps xmm0,xmm0 [55]");
    }

    #[test]
    fn test_cmpps_eq() {
        // 0F C2 C1 00 = cmpps xmm0,xmm1 [eq]
        assert_eq!(dis(&[0x0F, 0xC2, 0xC1, 0x00]), "cmpps xmm0,xmm1 [eq]");
    }

    #[test]
    fn test_cmpps_neq() {
        // 0F C2 C1 04 = cmpps xmm0,xmm1 [neq]
        assert_eq!(dis(&[0x0F, 0xC2, 0xC1, 0x04]), "cmpps xmm0,xmm1 [neq]");
    }

    #[test]
    fn test_cmpps_lt() {
        // 0F C2 C1 01 = cmpps xmm0,xmm1 [lt]
        assert_eq!(dis(&[0x0F, 0xC2, 0xC1, 0x01]), "cmpps xmm0,xmm1 [lt]");
    }

    // -----------------------------------------------------------------------
    // SSE: xorps, orps, andps, andnps, xorpd, orpd, andpd
    // -----------------------------------------------------------------------
    #[test]
    fn test_xorps_xmm0_xmm0() {
        // 0F 57 C0 = xorps xmm0,xmm0
        assert_eq!(dis(&[0x0F, 0x57, 0xC0]), "xorps xmm0,xmm0");
    }

    #[test]
    fn test_orps_xmm0_xmm1() {
        // 0F 56 C1 = orps xmm0,xmm1
        assert_eq!(dis(&[0x0F, 0x56, 0xC1]), "orps xmm0,xmm1");
    }

    #[test]
    fn test_andps_xmm0_xmm1() {
        // 0F 54 C1 = andps xmm0,xmm1
        assert_eq!(dis(&[0x0F, 0x54, 0xC1]), "andps xmm0,xmm1");
    }

    #[test]
    fn test_xorpd_xmm0_xmm1() {
        // 66 0F 57 C1 = xorpd xmm0,xmm1
        assert_eq!(dis(&[0x66, 0x0F, 0x57, 0xC1]), "xorpd xmm0,xmm1");
    }

    #[test]
    fn test_andpd_xmm0_xmm1() {
        // 66 0F 54 C1 = andpd xmm0,xmm1
        assert_eq!(dis(&[0x66, 0x0F, 0x54, 0xC1]), "andpd xmm0,xmm1");
    }

    // -----------------------------------------------------------------------
    // SSE: packed integer ops (paddd, psubd, pxor)
    // -----------------------------------------------------------------------
    #[test]
    fn test_paddd_xmm0_xmm1() {
        // 66 0F FE C1 = paddd xmm0,xmm1
        assert_eq!(dis(&[0x66, 0x0F, 0xFE, 0xC1]), "paddd xmm0,xmm1");
    }

    #[test]
    fn test_psubd_xmm0_xmm1() {
        // 66 0F FA C1 = psubd xmm0,xmm1
        assert_eq!(dis(&[0x66, 0x0F, 0xFA, 0xC1]), "psubd xmm0,xmm1");
    }

    #[test]
    fn test_pxor_xmm0_xmm1() {
        // 66 0F EF C1 = pxor xmm0,xmm1
        assert_eq!(dis(&[0x66, 0x0F, 0xEF, 0xC1]), "pxor xmm0,xmm1");
    }

    // -----------------------------------------------------------------------
    // SSE: ucomisd, comiss
    // -----------------------------------------------------------------------
    #[test]
    fn test_ucomisd_xmm0_xmm1() {
        // 66 0F 2E C1 = ucomisd xmm0,xmm1
        assert_eq!(dis(&[0x66, 0x0F, 0x2E, 0xC1]), "ucomisd xmm0,xmm1");
    }

    #[test]
    fn test_comiss_xmm0_xmm1() {
        // 0F 2F C1 = comiss xmm0,xmm1
        assert_eq!(dis(&[0x0F, 0x2F, 0xC1]), "comiss xmm0,xmm1");
    }

    // -----------------------------------------------------------------------
    // SSE: rcpps, rsqrtps, sqrtps
    // -----------------------------------------------------------------------
    #[test]
    fn test_rcpps_xmm11_xmm11() {
        // 45 0F 53 DB = rcpps xmm11,xmm11
        assert_eq!(dis(&[0x45, 0x0F, 0x53, 0xDB]), "rcpps xmm11,xmm11");
    }

    #[test]
    fn test_sqrtps_xmm11_xmm11() {
        // 45 0F 51 DB = sqrtps xmm11,xmm11
        assert_eq!(dis(&[0x45, 0x0F, 0x51, 0xDB]), "sqrtps xmm11,xmm11");
    }

    #[test]
    fn test_rsqrtps_xmm0_xmm0() {
        // 0F 52 C0 = rsqrtps xmm0,xmm0
        assert_eq!(dis(&[0x0F, 0x52, 0xC0]), "rsqrtps xmm0,xmm0");
    }

    // -----------------------------------------------------------------------
    // SSE: minps, maxps
    // -----------------------------------------------------------------------
    #[test]
    fn test_minps_xmm0_xmm1() {
        // 0F 5D C1 = minps xmm0,xmm1
        assert_eq!(dis(&[0x0F, 0x5D, 0xC1]), "minps xmm0,xmm1");
    }

    #[test]
    fn test_maxps_xmm0_xmm1() {
        // 0F 5F C1 = maxps xmm0,xmm1
        assert_eq!(dis(&[0x0F, 0x5F, 0xC1]), "maxps xmm0,xmm1");
    }

    // -----------------------------------------------------------------------
    // Conditional moves (cmovz, cmovnz, etc.) from upstream
    // -----------------------------------------------------------------------
    #[test]
    fn test_cmovzq_rax_rcx() {
        // 48 0F 44 C1 = cmovzq rax,rcx
        assert_eq!(dis(&[0x48, 0x0F, 0x44, 0xC1]), "cmovzq rax,rcx");
    }

    #[test]
    fn test_cmovnzq_rax_rcx() {
        // 48 0F 45 C1 = cmovnzq rax,rcx
        assert_eq!(dis(&[0x48, 0x0F, 0x45, 0xC1]), "cmovnzq rax,rcx");
    }

    #[test]
    fn test_cmovlq_rax_rcx() {
        // 48 0F 4C C1 = cmovlq rax,rcx
        assert_eq!(dis(&[0x48, 0x0F, 0x4C, 0xC1]), "cmovlq rax,rcx");
    }

    #[test]
    fn test_cmovgeq_rax_rcx() {
        // 48 0F 4D C1 = cmovgeq rax,rcx
        assert_eq!(dis(&[0x48, 0x0F, 0x4D, 0xC1]), "cmovgeq rax,rcx");
    }

    #[test]
    fn test_cmovleq_rax_rcx() {
        // 48 0F 4E C1 = cmovleq rax,rcx
        assert_eq!(dis(&[0x48, 0x0F, 0x4E, 0xC1]), "cmovleq rax,rcx");
    }

    #[test]
    fn test_cmovgq_rax_rcx() {
        // 48 0F 4F C1 = cmovgq rax,rcx
        assert_eq!(dis(&[0x48, 0x0F, 0x4F, 0xC1]), "cmovgq rax,rcx");
    }

    #[test]
    fn test_cmovaq_rax_rcx() {
        // 48 0F 47 C1 = cmovaq rax,rcx
        assert_eq!(dis(&[0x48, 0x0F, 0x47, 0xC1]), "cmovaq rax,rcx");
    }

    #[test]
    fn test_cmovnaq_rax_rcx() {
        // 48 0F 46 C1 = cmovnaq rax,rcx
        assert_eq!(dis(&[0x48, 0x0F, 0x46, 0xC1]), "cmovnaq rax,rcx");
    }

    // cmov without REX.W (32-bit)
    #[test]
    fn test_cmovzl_rax_rcx() {
        // 0F 44 C1 = cmovzl rax,rcx
        assert_eq!(dis(&[0x0F, 0x44, 0xC1]), "cmovzl rax,rcx");
    }

    // -----------------------------------------------------------------------
    // Bit operations: bt, bts, btr (from upstream)
    // -----------------------------------------------------------------------
    #[test]
    fn test_btq_rax_imm() {
        // 48 0F BA E0 05 = btq rax,5
        assert_eq!(dis(&[0x48, 0x0F, 0xBA, 0xE0, 0x05]), "btq rax,5");
    }

    #[test]
    fn test_btsq_rax_imm() {
        // 48 0F BA E8 05 = btsq rax,5
        assert_eq!(dis(&[0x48, 0x0F, 0xBA, 0xE8, 0x05]), "btsq rax,5");
    }

    #[test]
    fn test_btrq_rax_imm() {
        // 48 0F BA F0 05 = btrq rax,5
        assert_eq!(dis(&[0x48, 0x0F, 0xBA, 0xF0, 0x05]), "btrq rax,5");
    }

    #[test]
    fn test_btq_reg_reg() {
        // 48 0F A3 C8 = btq rax,rcx (0F A3 /r)
        assert_eq!(dis(&[0x48, 0x0F, 0xA3, 0xC8]), "btq rax,rcx");
    }

    #[test]
    fn test_btsq_reg_reg() {
        // 48 0F AB C8 = btsq rax,rcx (0F AB /r)
        assert_eq!(dis(&[0x48, 0x0F, 0xAB, 0xC8]), "btsq rax,rcx");
    }

    // -----------------------------------------------------------------------
    // String operations: rep movsb, rep movsl, rep movsq
    // -----------------------------------------------------------------------
    #[test]
    fn test_rep_movsb() {
        // F3 A4 = rep movsb
        assert_eq!(dis(&[0xF3, 0xA4]), "rep movsb");
    }

    #[test]
    fn test_rep_movsl() {
        // F3 A5 = rep movsl  (32-bit: no REX.W, no 0x66)
        assert_eq!(dis(&[0xF3, 0xA5]), "rep movsl");
    }

    #[test]
    fn test_rep_movsq() {
        // F3 48 A5 = rep movsq (REX.W)
        assert_eq!(dis(&[0xF3, 0x48, 0xA5]), "rep movsq");
    }

    // -----------------------------------------------------------------------
    // xchg, lock cmpxchg (from CompareSwap tests)
    // -----------------------------------------------------------------------
    #[test]
    fn test_xchgq_rax_rdx() {
        // 48 87 C2 = xchgq rax,rdx  (87 /r with REX.W)
        assert_eq!(dis(&[0x48, 0x87, 0xC2]), "xchgq rax,rdx");
    }

    #[test]
    fn test_xchgq_rax_rdx_short() {
        // 48 92 = xchgq rax,rdx (short form: 0x90+rd with REX.W)
        assert_eq!(dis(&[0x48, 0x92]), "xchgq rax, rdx");
    }

    #[test]
    fn test_lock_cmpxchgq_mem_rsp_rcx() {
        // F0 48 0F B1 0C 24 = lock cmpxchgq rcx,[rsp]
        assert_eq!(
            dis(&[0xF0, 0x48, 0x0F, 0xB1, 0x0C, 0x24]),
            "lock cmpxchgq rcx,[rsp]"
        );
    }

    #[test]
    fn test_lock_cmpxchgl_mem_rsp_rcx() {
        // F0 0F B1 0C 24 = lock cmpxchgl rcx,[rsp]
        assert_eq!(
            dis(&[0xF0, 0x0F, 0xB1, 0x0C, 0x24]),
            "lock cmpxchgl rcx,[rsp]"
        );
    }

    // -----------------------------------------------------------------------
    // 32-bit operand forms (movl, addl, subl, etc.) from upstream
    // -----------------------------------------------------------------------
    #[test]
    fn test_addl_rax_rcx_oper() {
        // 01 C8 = addl rax,rcx
        assert_eq!(dis(&[0x01, 0xC8]), "addl rax,rcx");
    }

    #[test]
    fn test_subl_rax_rcx() {
        // 29 C8 = subl rax,rcx
        assert_eq!(dis(&[0x29, 0xC8]), "subl rax,rcx");
    }

    #[test]
    fn test_adcl_rdx_r8() {
        // 44 11 C2 = adcl rdx,r8
        assert_eq!(dis(&[0x44, 0x11, 0xC2]), "adcl rdx,r8");
    }

    #[test]
    fn test_sbbl_rdx_r8() {
        // 44 19 C2 = sbbl rdx,r8
        assert_eq!(dis(&[0x44, 0x19, 0xC2]), "sbbl rdx,r8");
    }

    #[test]
    fn test_xorl_rcx_rcx() {
        // 31 C9 = xorl rcx,rcx
        assert_eq!(dis(&[0x31, 0xC9]), "xorl rcx,rcx");
    }

    #[test]
    fn test_orl_rcx_imm32() {
        // 81 C9 00 01 00 00 = orl rcx,0x100
        assert_eq!(
            dis(&[0x81, 0xC9, 0x00, 0x01, 0x00, 0x00]),
            "orl rcx,0x100"
        );
    }

    #[test]
    fn test_andl_rcx_rax() {
        // 21 C1 = andl rcx,rax
        assert_eq!(dis(&[0x21, 0xC1]), "andl rcx,rax");
    }

    #[test]
    fn test_orl_rcx_rax() {
        // 09 C1 = orl rcx,rax
        assert_eq!(dis(&[0x09, 0xC1]), "orl rcx,rax");
    }

    // -----------------------------------------------------------------------
    // Immediate forms: addq rax,imm32 (short immediate)
    // -----------------------------------------------------------------------
    #[test]
    fn test_addq_rax_imm32_short() {
        // 48 05 E8 03 00 00 = addq rax,0x3e8
        assert_eq!(
            dis(&[0x48, 0x05, 0xE8, 0x03, 0x00, 0x00]),
            "addq rax,0x3e8"
        );
    }

    #[test]
    fn test_subq_rax_imm32_short() {
        // 48 2D E8 03 00 00 = subq rax,0x3e8
        assert_eq!(
            dis(&[0x48, 0x2D, 0xE8, 0x03, 0x00, 0x00]),
            "subq rax,0x3e8"
        );
    }

    #[test]
    fn test_cmpq_rax_imm32_short() {
        // 48 3D E8 03 00 00 = cmpq rax,0x3e8
        assert_eq!(
            dis(&[0x48, 0x3D, 0xE8, 0x03, 0x00, 0x00]),
            "cmpq rax,0x3e8"
        );
    }

    #[test]
    fn test_orq_rax_imm32_short() {
        // 48 0D 00 01 00 00 = orq rax,0x100
        assert_eq!(
            dis(&[0x48, 0x0D, 0x00, 0x01, 0x00, 0x00]),
            "orq rax,0x100"
        );
    }

    // -----------------------------------------------------------------------
    // Shift operations: shll, shrl, sarl from LogicalOps
    // -----------------------------------------------------------------------
    #[test]
    fn test_shll_rax_3() {
        // C1 E0 03 = shll rax,3
        assert_eq!(dis(&[0xC1, 0xE0, 0x03]), "shll rax,3");
    }

    #[test]
    fn test_shrl_rax_1() {
        // D1 E8 = shrl rax,1
        assert_eq!(dis(&[0xD1, 0xE8]), "shrl rax,1");
    }

    #[test]
    fn test_shrl_rax_3() {
        // C1 E8 03 = shrl rax,3
        assert_eq!(dis(&[0xC1, 0xE8, 0x03]), "shrl rax,3");
    }

    #[test]
    fn test_shll_rax_cl() {
        // D3 E0 = shll rax,cl
        assert_eq!(dis(&[0xD3, 0xE0]), "shll rax,cl");
    }

    #[test]
    fn test_shrl_rax_cl() {
        // D3 E8 = shrl rax,cl
        assert_eq!(dis(&[0xD3, 0xE8]), "shrl rax,cl");
    }

    #[test]
    fn test_sarl_rax_3() {
        // C1 F8 03 = sarl rax,3
        assert_eq!(dis(&[0xC1, 0xF8, 0x03]), "sarl rax,3");
    }

    #[test]
    fn test_sarl_rax_cl() {
        // D3 F8 = sarl rax,cl
        assert_eq!(dis(&[0xD3, 0xF8]), "sarl rax,cl");
    }

    #[test]
    fn test_sarq_rax_3() {
        // 48 C1 F8 03 = sarq rax,3
        assert_eq!(dis(&[0x48, 0xC1, 0xF8, 0x03]), "sarq rax,3");
    }

    // -----------------------------------------------------------------------
    // shld, shrd (double-precision shift) from LogicalOps
    // -----------------------------------------------------------------------
    #[test]
    fn test_shldl_rdx_r8_imm() {
        // 44 0F A4 C2 02 = shldl rdx,r8,2
        assert_eq!(dis(&[0x44, 0x0F, 0xA4, 0xC2, 0x02]), "shldl rdx,r8,2");
    }

    #[test]
    fn test_shldq_rdx_r8_imm() {
        // 4C 0F A4 C2 02 = shldq rdx,r8,2
        assert_eq!(dis(&[0x4C, 0x0F, 0xA4, 0xC2, 0x02]), "shldq rdx,r8,2");
    }

    #[test]
    fn test_shldq_rdx_r8_cl() {
        // 4C 0F A5 C2 = shldq rdx,r8,cl
        assert_eq!(dis(&[0x4C, 0x0F, 0xA5, 0xC2]), "shldq rdx,r8,cl");
    }

    #[test]
    fn test_shrdq_rdx_r8_cl() {
        // 4C 0F AD C2 = shrdq rdx,r8,cl
        assert_eq!(dis(&[0x4C, 0x0F, 0xAD, 0xC2]), "shrdq rdx,r8,cl");
    }

    // -----------------------------------------------------------------------
    // test (from LogicalTestL, LogicalTestQ)
    // -----------------------------------------------------------------------
    #[test]
    fn test_testl_rax_rcx() {
        // 85 C8 = testl rcx,rax (test is REG_OPER order: reg=rcx, rm=rax)
        assert_eq!(dis(&[0x85, 0xC8]), "testl rcx,rax");
    }

    #[test]
    fn test_testl_rdx_rcx() {
        // 85 CA = testl rcx,rdx
        assert_eq!(dis(&[0x85, 0xCA]), "testl rcx,rdx");
    }

    #[test]
    fn test_test_al_0() {
        // A8 00 = test al,0
        assert_eq!(dis(&[0xA8, 0x00]), "test al,0");
    }

    #[test]
    fn test_test_al_0xff() {
        // A8 FF = test al,0xff
        assert_eq!(dis(&[0xA8, 0xFF]), "test al,0xff");
    }

    #[test]
    fn test_testb_rcx_4() {
        // F6 C1 04 = testb rcx,4 (F6 /0 with modrm=C1: mod=11, reg=0, rm=rcx)
        assert_eq!(dis(&[0xF6, 0xC1, 0x04]), "testb rcx,4");
    }

    // -----------------------------------------------------------------------
    // imull with 3-operand immediate form
    // -----------------------------------------------------------------------
    #[test]
    fn test_imull_rax_rcx() {
        // 0F AF C1 = imull rax,rcx
        assert_eq!(dis(&[0x0F, 0xAF, 0xC1]), "imull rax,rcx");
    }

    #[test]
    fn test_imulq_rax_rcx_twobyte() {
        // 48 0F AF C1 = imulq rax,rcx (two-byte opcode form)
        assert_eq!(dis(&[0x48, 0x0F, 0xAF, 0xC1]), "imulq rax,rcx");
    }

    #[test]
    fn test_imull_rax_rax_1000() {
        // 69 C0 E8 03 00 00 = imull rax,rax,0x3e8
        assert_eq!(
            dis(&[0x69, 0xC0, 0xE8, 0x03, 0x00, 0x00]),
            "imull rax,rax,0x3e8"
        );
    }

    #[test]
    fn test_imull_rdx_rcx() {
        // 0F AF D1 = imull rdx,rcx
        assert_eq!(dis(&[0x0F, 0xAF, 0xD1]), "imull rdx,rcx");
    }

    #[test]
    fn test_imull_rdx_rdx_1000() {
        // 69 D2 E8 03 00 00 = imull rdx,rdx,0x3e8
        assert_eq!(
            dis(&[0x69, 0xD2, 0xE8, 0x03, 0x00, 0x00]),
            "imull rdx,rdx,0x3e8"
        );
    }

    // -----------------------------------------------------------------------
    // mull, divl, idivl, imulq (implicit RDX:RAX) from upstream
    // -----------------------------------------------------------------------
    #[test]
    fn test_mull_rcx() {
        // F7 E1 = mull (rax,rdx),rcx
        assert_eq!(dis(&[0xF7, 0xE1]), "mull (rax,rdx),rcx");
    }

    #[test]
    fn test_mulq_rcx() {
        // 48 F7 E1 = mulq (rax,rdx),rcx
        assert_eq!(dis(&[0x48, 0xF7, 0xE1]), "mulq (rax,rdx),rcx");
    }

    #[test]
    fn test_idivl_rcx() {
        // F7 F9 = idivl (rax,rdx),rcx
        assert_eq!(dis(&[0xF7, 0xF9]), "idivl (rax,rdx),rcx");
    }

    #[test]
    fn test_divl_rcx() {
        // F7 F1 = divl (rax,rdx),rcx
        assert_eq!(dis(&[0xF7, 0xF1]), "divl (rax,rdx),rcx");
    }

    #[test]
    fn test_divq_rcx() {
        // 48 F7 F1 = divq (rax,rdx),rcx
        assert_eq!(dis(&[0x48, 0xF7, 0xF1]), "divq (rax,rdx),rcx");
    }

    #[test]
    fn test_imulq_rdx_implicit() {
        // 48 F7 EA = imulq (rax,rdx),rdx
        assert_eq!(dis(&[0x48, 0xF7, 0xEA]), "imulq (rax,rdx),rdx");
    }

    // -----------------------------------------------------------------------
    // negq, notq (from Negate test)
    // -----------------------------------------------------------------------
    #[test]
    fn test_negq_rcx() {
        // 48 F7 D9 = negq rcx
        assert_eq!(dis(&[0x48, 0xF7, 0xD9]), "negq rcx");
    }

    #[test]
    fn test_notq_rcx() {
        // 48 F7 D1 = notq rcx
        assert_eq!(dis(&[0x48, 0xF7, 0xD1]), "notq rcx");
    }

    // -----------------------------------------------------------------------
    // movzxb, movzxw, movsxb, movsxw (from MoveExtend tests)
    // -----------------------------------------------------------------------
    #[test]
    fn test_movzxbq_rax_rdx() {
        // 48 0F B6 C2 = movzxbq rax,rdx
        assert_eq!(dis(&[0x48, 0x0F, 0xB6, 0xC2]), "movzxbq rax,rdx");
    }

    #[test]
    fn test_movsxwq_r8_rdx() {
        // 4C 0F BF C2 = movsxwq r8,rdx
        assert_eq!(dis(&[0x4C, 0x0F, 0xBF, 0xC2]), "movsxwq r8,rdx");
    }

    #[test]
    fn test_movzxwq_rcx_rdx() {
        // 48 0F B7 CA = movzxwq rcx,rdx
        assert_eq!(dis(&[0x48, 0x0F, 0xB7, 0xCA]), "movzxwq rcx,rdx");
    }

    #[test]
    fn test_movzxbq_rax_mem_rsp() {
        // 48 0F B6 04 24 = movzxbq rax,[rsp]
        assert_eq!(dis(&[0x48, 0x0F, 0xB6, 0x04, 0x24]), "movzxbq rax,[rsp]");
    }

    #[test]
    fn test_movsxwq_r8_mem_rsp() {
        // 4C 0F BF 04 24 = movsxwq r8,[rsp]
        assert_eq!(dis(&[0x4C, 0x0F, 0xBF, 0x04, 0x24]), "movsxwq r8,[rsp]");
    }

    // -----------------------------------------------------------------------
    // movsxdq (from MoveExtend32)
    // -----------------------------------------------------------------------
    #[test]
    fn test_movsxdq_rdx_rdx() {
        // 48 63 D2 = movsxdq rdx,rdx
        assert_eq!(dis(&[0x48, 0x63, 0xD2]), "movsxdq rdx,rdx");
    }

    #[test]
    fn test_movsxdq_rdx_mem_rsp_disp8() {
        // 48 63 54 24 08 = movsxdq rdx,[rsp+0x8]
        assert_eq!(
            dis(&[0x48, 0x63, 0x54, 0x24, 0x08]),
            "movsxdq rdx,[rsp+0x8]"
        );
    }

    // -----------------------------------------------------------------------
    // popcntq, lzcntq (from Popcnt, Lzcnt tests)
    // -----------------------------------------------------------------------
    #[test]
    fn test_popcntq_rax_rcx() {
        // F3 48 0F B8 C1 = popcntq rax,rcx
        assert_eq!(dis(&[0xF3, 0x48, 0x0F, 0xB8, 0xC1]), "popcntq rax,rcx");
    }

    #[test]
    fn test_lzcntq_rax_rcx() {
        // F3 48 0F BD C1 = lzcntq rax,rcx
        assert_eq!(dis(&[0xF3, 0x48, 0x0F, 0xBD, 0xC1]), "lzcntq rax,rcx");
    }

    // -----------------------------------------------------------------------
    // Word/byte operations (from WordOps, ByteOps tests)
    // -----------------------------------------------------------------------
    #[test]
    fn test_movw_mem_rax_rcx() {
        // 66 89 08 = movw [rax],rcx
        assert_eq!(dis(&[0x66, 0x89, 0x08]), "movw [rax],rcx");
    }

    #[test]
    fn test_movzxwq_rax_mem_rax() {
        // 48 0F B7 00 = movzxwq rax,[rax]
        assert_eq!(dis(&[0x48, 0x0F, 0xB7, 0x00]), "movzxwq rax,[rax]");
    }

    #[test]
    fn test_addw_mem_rsp_imm() {
        // 66 81 04 24 FF FD = addw [rsp],0xfdff
        assert_eq!(
            dis(&[0x66, 0x81, 0x04, 0x24, 0xFF, 0xFD]),
            "addw [rsp],0xfdff"
        );
    }

    #[test]
    fn test_subw_mem_rsp_disp_imm() {
        // 66 81 6C 24 02 01 02 = subw [rsp+0x2],0x201
        assert_eq!(
            dis(&[0x66, 0x81, 0x6C, 0x24, 0x02, 0x01, 0x02]),
            "subw [rsp+0x2],0x201"
        );
    }

    #[test]
    fn test_addb_mem_rsp_imm() {
        // 80 04 24 FF = addb [rsp],-1
        assert_eq!(dis(&[0x80, 0x04, 0x24, 0xFF]), "addb [rsp],-1");
    }

    #[test]
    fn test_subb_mem_rsp_disp_imm() {
        // 80 6C 24 02 01 = subb [rsp+0x2],1
        assert_eq!(dis(&[0x80, 0x6C, 0x24, 0x02, 0x01]), "subb [rsp+0x2],1");
    }

    // -----------------------------------------------------------------------
    // setcc (from upstream tests)
    // -----------------------------------------------------------------------
    #[test]
    fn test_setnz_rax() {
        // 0F 95 C0 = setnz al
        assert_eq!(dis(&[0x0F, 0x95, 0xC0]), "setnz al");
    }

    #[test]
    fn test_setg_al() {
        // 0F 9F C0 = setg al
        assert_eq!(dis(&[0x0F, 0x9F, 0xC0]), "setg al");
    }

    // -----------------------------------------------------------------------
    // All conditional jumps (short) from JumpAroundCrash test
    // -----------------------------------------------------------------------
    #[test]
    fn test_all_jcc_short() {
        // jo +109; jno +103; jc +97; jnc +91; jz +85; jnz +79;
        // jna +73; ja +67; js +61; jns +55; jpe +49; jpo +43;
        // jl +37; jge +31; jle +25; jg +19
        // Displacement byte = target_offset - 2 (since jcc short is 2 bytes)
        let code: Vec<u8> = vec![
            0x70, 0x6B,  // jo  (disp=107, +2=109)
            0x71, 0x65,  // jno (disp=101, +2=103)
            0x72, 0x5F,  // jc  (disp=95, +2=97)
            0x73, 0x59,  // jnc (disp=89, +2=91)
            0x74, 0x53,  // jz  (disp=83, +2=85)
            0x75, 0x4D,  // jnz (disp=77, +2=79)
            0x76, 0x47,  // jna (disp=71, +2=73)
            0x77, 0x41,  // ja  (disp=65, +2=67)
            0x78, 0x3B,  // js  (disp=59, +2=61)
            0x79, 0x35,  // jns (disp=53, +2=55)
            0x7A, 0x2F,  // jpe (disp=47, +2=49)
            0x7B, 0x29,  // jpo (disp=41, +2=43)
            0x7C, 0x23,  // jl  (disp=35, +2=37)
            0x7D, 0x1D,  // jge (disp=29, +2=31)
            0x7E, 0x17,  // jle (disp=23, +2=25)
            0x7F, 0x11,  // jg  (disp=17, +2=19)
        ];
        let result = dis_all(&code);
        assert_eq!(
            result,
            "jo +109\n\
             jno +103\n\
             jc +97\n\
             jnc +91\n\
             jz +85\n\
             jnz +79\n\
             jna +73\n\
             ja +67\n\
             js +61\n\
             jns +55\n\
             jpe +49\n\
             jpo +43\n\
             jl +37\n\
             jge +31\n\
             jle +25\n\
             jg +19\n"
        );
    }

    // -----------------------------------------------------------------------
    // jmp [reg+disp] (from JumpAddress test)
    // -----------------------------------------------------------------------
    #[test]
    fn test_jmp_mem_rdi_disp() {
        // FF 67 28 = jmp [rdi+0x28]
        assert_eq!(dis(&[0xFF, 0x67, 0x28]), "jmp [rdi+0x28]");
    }

    // -----------------------------------------------------------------------
    // movl to/from memory (from Increment test)
    // -----------------------------------------------------------------------
    #[test]
    fn test_movl_rax_mem_rsp() {
        // 8B 04 24 = movl rax,[rsp]
        assert_eq!(dis(&[0x8B, 0x04, 0x24]), "movl rax,[rsp]");
    }

    #[test]
    fn test_movl_mem_rsp_rax() {
        // 89 04 24 = movl [rsp],rax
        assert_eq!(dis(&[0x89, 0x04, 0x24]), "movl [rsp],rax");
    }

    #[test]
    fn test_movl_rax_mem_rsp_disp8() {
        // 8B 44 24 04 = movl rax,[rsp+0x4]
        assert_eq!(dis(&[0x8B, 0x44, 0x24, 0x04]), "movl rax,[rsp+0x4]");
    }

    #[test]
    fn test_movl_r8_mem_rsp_disp8() {
        // 44 8B 44 24 0C = movl r8,[rsp+0xc]
        assert_eq!(dis(&[0x44, 0x8B, 0x44, 0x24, 0x0C]), "movl r8,[rsp+0xc]");
    }

    // -----------------------------------------------------------------------
    // Increment/Decrement test (from upstream)
    // -----------------------------------------------------------------------
    #[test]
    fn test_increment_sequence() {
        // From the "Increment" test:
        // movl rax,0; push rax; incl [rsp]; incq [rsp]; movq rcx,[rsp];
        // incq rcx; pop rax; movq rax,rcx; ret
        let code: Vec<u8> = vec![
            0xB8, 0x00, 0x00, 0x00, 0x00,  // movl rax,0
            0x50,                            // push rax
            0xFF, 0x04, 0x24,                // incl [rsp]
            0x48, 0xFF, 0x04, 0x24,          // incq [rsp]
            0x48, 0x8B, 0x0C, 0x24,          // movq rcx,[rsp]
            0x48, 0xFF, 0xC1,                // incq rcx
            0x58,                            // pop rax
            0x48, 0x89, 0xC8,                // movq rax,rcx
            0xC3,                            // ret
        ];
        let result = dis_all(&code);
        assert_eq!(
            result,
            "movl rax,0\n\
             push rax\n\
             incl [rsp]\n\
             incq [rsp]\n\
             movq rcx,[rsp]\n\
             incq rcx\n\
             pop rax\n\
             movq rax,rcx\n\
             ret\n"
        );
    }

    // -----------------------------------------------------------------------
    // Decrement test (from upstream)
    // -----------------------------------------------------------------------
    #[test]
    fn test_decrement_sequence() {
        let code: Vec<u8> = vec![
            0xB8, 0x03, 0x00, 0x00, 0x00,  // movl rax,3
            0x50,                            // push rax
            0xFF, 0x0C, 0x24,                // decl [rsp]
            0x48, 0xFF, 0x0C, 0x24,          // decq [rsp]
            0x48, 0x8B, 0x0C, 0x24,          // movq rcx,[rsp]
            0x48, 0xFF, 0xC9,                // decq rcx
            0x58,                            // pop rax
            0x48, 0x89, 0xC8,                // movq rax,rcx
            0xC3,                            // ret
        ];
        let result = dis_all(&code);
        assert_eq!(
            result,
            "movl rax,3\n\
             push rax\n\
             decl [rsp]\n\
             decq [rsp]\n\
             movq rcx,[rsp]\n\
             decq rcx\n\
             pop rax\n\
             movq rax,rcx\n\
             ret\n"
        );
    }

    // -----------------------------------------------------------------------
    // XmmAlu multi-instruction test (from upstream)
    // -----------------------------------------------------------------------
    #[test]
    fn test_xmm_alu_sequence() {
        let code: Vec<u8> = vec![
            0xF3, 0x0F, 0x58, 0xC0,  // addss xmm0,xmm0
            0xF2, 0x0F, 0x58, 0xC0,  // addsd xmm0,xmm0
            0x0F, 0x58, 0xC0,        // addps xmm0,xmm0
            0x66, 0x0F, 0x58, 0xC0,  // addpd xmm0,xmm0
            0xF3, 0x0F, 0x5A, 0xC0,  // cvtss2sd xmm0,xmm0
            0xF2, 0x0F, 0x5A, 0xC0,  // cvtsd2ss xmm0,xmm0
            0x0F, 0x5A, 0xC0,        // cvtps2pd xmm0,xmm0
            0x66, 0x0F, 0x5A, 0xC0,  // cvtpd2ps xmm0,xmm0
            0xB8, 0x00, 0x00, 0x00, 0x00, // movl rax,0
            0xC3,                     // ret
        ];
        let result = dis_all(&code);
        assert_eq!(
            result,
            "addss xmm0,xmm0\n\
             addsd xmm0,xmm0\n\
             addps xmm0,xmm0\n\
             addpd xmm0,xmm0\n\
             cvtss2sd xmm0,xmm0\n\
             cvtsd2ss xmm0,xmm0\n\
             cvtps2pd xmm0,xmm0\n\
             cvtpd2ps xmm0,xmm0\n\
             movl rax,0\n\
             ret\n"
        );
    }

    // -----------------------------------------------------------------------
    // SignedMultiply test (from upstream)
    // -----------------------------------------------------------------------
    #[test]
    fn test_signed_multiply_sequence() {
        // movl rax,2; movl rcx,4; imull rax,rcx; imull rax,rax,0x3e8; ret
        let code: Vec<u8> = vec![
            0xB8, 0x02, 0x00, 0x00, 0x00,  // movl rax,2
            0xB9, 0x04, 0x00, 0x00, 0x00,  // movl rcx,4
            0x0F, 0xAF, 0xC1,              // imull rax,rcx
            0x69, 0xC0, 0xE8, 0x03, 0x00, 0x00, // imull rax,rax,0x3e8
            0xC3,                            // ret
        ];
        let result = dis_all(&code);
        assert_eq!(
            result,
            "movl rax,2\n\
             movl rcx,4\n\
             imull rax,rcx\n\
             imull rax,rax,0x3e8\n\
             ret\n"
        );
    }

    // -----------------------------------------------------------------------
    // Negate test (from upstream)
    // -----------------------------------------------------------------------
    #[test]
    fn test_negate_sequence() {
        // movl rcx,0x2a; negq rcx; movq rax,rcx; ret
        let code: Vec<u8> = vec![
            0xB9, 0x2A, 0x00, 0x00, 0x00,  // movl rcx,0x2a
            0x48, 0xF7, 0xD9,              // negq rcx
            0x48, 0x89, 0xC8,              // movq rax,rcx
            0xC3,                            // ret
        ];
        let result = dis_all(&code);
        assert_eq!(
            result,
            "movl rcx,0x2a\n\
             negq rcx\n\
             movq rax,rcx\n\
             ret\n"
        );
    }

    // -----------------------------------------------------------------------
    // Exchange test (from upstream)
    // -----------------------------------------------------------------------
    #[test]
    fn test_exchange_xchg_reg() {
        // xchgq rax,rdx; subq rax,rdx
        let code: Vec<u8> = vec![
            0x48, 0x87, 0xC2,  // xchgq rax,rdx (87 /r form)
            0x48, 0x29, 0xD0,  // subq rax,rdx
        ];
        let result = dis_all(&code);
        assert_eq!(
            result,
            "xchgq rax,rdx\n\
             subq rax,rdx\n"
        );
    }

    // -----------------------------------------------------------------------
    // MoveExtend test (from upstream)
    // -----------------------------------------------------------------------
    #[test]
    fn test_move_extend_sequence() {
        assert_eq!(dis(&[0x48, 0x0F, 0xB6, 0xC2]), "movzxbq rax,rdx");
        assert_eq!(dis(&[0x4C, 0x0F, 0xBF, 0xC2]), "movsxwq r8,rdx");
        assert_eq!(dis(&[0x48, 0x0F, 0xB7, 0xCA]), "movzxwq rcx,rdx");
    }

    // -----------------------------------------------------------------------
    // MoveExtend32 test (from upstream)
    // -----------------------------------------------------------------------
    #[test]
    fn test_movsxdq_rax_rax() {
        // 48 63 C0 = movsxdq rax,rax
        assert_eq!(dis(&[0x48, 0x63, 0xC0]), "movsxdq rax,rax");
    }

    // -----------------------------------------------------------------------
    // cmpb (byte compare from memory, from Cmpb test)
    // -----------------------------------------------------------------------
    #[test]
    fn test_cmpb_mem_rsp_imm() {
        // 80 3C 24 11 = cmpb [rsp],0x11
        assert_eq!(dis(&[0x80, 0x3C, 0x24, 0x11]), "cmpb [rsp],0x11");
    }

    // -----------------------------------------------------------------------
    // testb from memory (from Testb test)
    // -----------------------------------------------------------------------
    #[test]
    fn test_testb_mem_rsp_imm() {
        // F6 04 24 10 = testb [rsp],0x10
        // This is F6 /0 with modrm 04 (mod=00, rm=100->SIB) SIB=24(rsp)
        assert_eq!(dis(&[0xF6, 0x04, 0x24, 0x10]), "testb [rsp],0x10");
    }

    // -----------------------------------------------------------------------
    // movapd (from upstream)
    // -----------------------------------------------------------------------
    #[test]
    fn test_movapd_xmm0_xmm1() {
        // 66 0F 28 C1 = movapd xmm0,xmm1
        assert_eq!(dis(&[0x66, 0x0F, 0x28, 0xC1]), "movapd xmm0, xmm1");
    }

    // -----------------------------------------------------------------------
    // movdqa (from upstream)
    // -----------------------------------------------------------------------
    #[test]
    fn test_movdqa_xmm0_xmm1() {
        // 66 0F 6F C1 = movdqa xmm0,xmm1
        assert_eq!(dis(&[0x66, 0x0F, 0x6F, 0xC1]), "movdqa xmm0,xmm1");
    }

    // -----------------------------------------------------------------------
    // lock incq (from upstream patterns)
    // -----------------------------------------------------------------------
    #[test]
    fn test_lock_incq_mem_rax() {
        // F0 48 FF 00 = lock incq [rax]
        assert_eq!(dis(&[0xF0, 0x48, 0xFF, 0x00]), "lock incq [rax]");
    }

    // -----------------------------------------------------------------------
    // orl to memory (from Bitwise test)
    // -----------------------------------------------------------------------
    #[test]
    fn test_orl_mem_rdi_r10() {
        // 44 09 17 = orl [rdi],r10
        assert_eq!(dis(&[0x44, 0x09, 0x17]), "orl [rdi],r10");
    }

    // -----------------------------------------------------------------------
    // lea (from upstream patterns)
    // -----------------------------------------------------------------------
    #[test]
    fn test_leaq_rax_mem_rbp_disp8() {
        // 48 8D 45 08 = leaq rax,[rbp+0x8]
        assert_eq!(dis(&[0x48, 0x8D, 0x45, 0x08]), "leaq rax,[rbp+0x8]");
    }

    #[test]
    fn test_leaq_rax_mem_rsp_disp8() {
        // 48 8D 44 24 10 = leaq rax,[rsp+0x10]
        assert_eq!(dis(&[0x48, 0x8D, 0x44, 0x24, 0x10]), "leaq rax,[rsp+0x10]");
    }

    // -----------------------------------------------------------------------
    // enter instruction
    // -----------------------------------------------------------------------
    #[test]
    fn test_enter() {
        // C8 10 00 00 = enter 16, 0
        assert_eq!(dis(&[0xC8, 0x10, 0x00, 0x00]), "enter 16, 0");
    }

    // -----------------------------------------------------------------------
    // Forced 32-bit displacement forms (AddressBaseImm32 from upstream)
    // -----------------------------------------------------------------------
    #[test]
    fn test_addr_base_imm32_rsp_0() {
        // movq rax,[rsp+0] -- 48 8B 84 24 00 00 00 00
        assert_eq!(
            dis(&[0x48, 0x8B, 0x84, 0x24, 0x00, 0x00, 0x00, 0x00]),
            "movq rax,[rsp+0]"
        );
    }

    #[test]
    fn test_addr_base_imm32_rbp_0() {
        // movq rax,[rbp+0] -- 48 8B 85 00 00 00 00
        assert_eq!(
            dis(&[0x48, 0x8B, 0x85, 0x00, 0x00, 0x00, 0x00]),
            "movq rax,[rbp+0]"
        );
    }

    #[test]
    fn test_addr_base_imm32_rax_0() {
        // movq rax,[rax+0] -- 48 8B 80 00 00 00 00
        assert_eq!(
            dis(&[0x48, 0x8B, 0x80, 0x00, 0x00, 0x00, 0x00]),
            "movq rax,[rax+0]"
        );
    }

    #[test]
    fn test_addr_base_imm32_r10_0() {
        // movq rax,[r10+0] -- 49 8B 82 00 00 00 00
        assert_eq!(
            dis(&[0x49, 0x8B, 0x82, 0x00, 0x00, 0x00, 0x00]),
            "movq rax,[r10+0]"
        );
    }

    #[test]
    fn test_addr_base_imm32_r12_0() {
        // movq rax,[r12+0] -- 49 8B 84 24 00 00 00 00
        assert_eq!(
            dis(&[0x49, 0x8B, 0x84, 0x24, 0x00, 0x00, 0x00, 0x00]),
            "movq rax,[r12+0]"
        );
    }

    #[test]
    fn test_addr_base_imm32_r13_0() {
        // movq rax,[r13+0] -- 49 8B 85 00 00 00 00
        assert_eq!(
            dis(&[0x49, 0x8B, 0x85, 0x00, 0x00, 0x00, 0x00]),
            "movq rax,[r13+0]"
        );
    }

    #[test]
    fn test_addr_base_imm32_rsp_neg8() {
        // movq rax,[rsp-0x8] -- 48 8B 84 24 F8 FF FF FF
        assert_eq!(
            dis(&[0x48, 0x8B, 0x84, 0x24, 0xF8, 0xFF, 0xFF, 0xFF]),
            "movq rax,[rsp-0x8]"
        );
    }

    // -----------------------------------------------------------------------
    // movq to memory with immediate (from upstream patterns)
    // -----------------------------------------------------------------------
    #[test]
    fn test_movq_mem_rsp_imm_0() {
        // 48 C7 04 24 00 00 00 00 = movq [rsp],0
        assert_eq!(
            dis(&[0x48, 0xC7, 0x04, 0x24, 0x00, 0x00, 0x00, 0x00]),
            "movq [rsp],0"
        );
    }

    // -----------------------------------------------------------------------
    // addq rsp,8 (common stack manipulation)
    // -----------------------------------------------------------------------
    #[test]
    fn test_addq_rsp_8() {
        // 48 83 C4 08 = addq rsp,8
        assert_eq!(dis(&[0x48, 0x83, 0xC4, 0x08]), "addq rsp,8");
    }

    #[test]
    fn test_subq_rsp_8() {
        // 48 83 EC 08 = subq rsp,8
        assert_eq!(dis(&[0x48, 0x83, 0xEC, 0x08]), "subq rsp,8");
    }

    #[test]
    fn test_addq_rsp_imm32() {
        // 48 81 C4 00 10 00 00 = addq rsp,0x1000
        assert_eq!(
            dis(&[0x48, 0x81, 0xC4, 0x00, 0x10, 0x00, 0x00]),
            "addq rsp,0x1000"
        );
    }

    // -----------------------------------------------------------------------
    // cmpl (from LogicalOps, SignedDivide)
    // -----------------------------------------------------------------------
    #[test]
    fn test_cmpl_rax_0() {
        // 83 F8 00 = cmpl rax,0
        assert_eq!(dis(&[0x83, 0xF8, 0x00]), "cmpl rax,0");
    }

    #[test]
    fn test_cmpl_rax_8() {
        // 83 F8 08 = cmpl rax,8
        assert_eq!(dis(&[0x83, 0xF8, 0x08]), "cmpl rax,8");
    }

    #[test]
    fn test_cmpl_rcx_0() {
        // 83 F9 00 = cmpl rcx,0
        assert_eq!(dis(&[0x83, 0xF9, 0x00]), "cmpl rcx,0");
    }

    #[test]
    fn test_cmpl_mem_rsp_imm() {
        // 81 3C 24 FF 00 00 00 = cmpl [rsp],0xff (imm32 form)
        assert_eq!(
            dis(&[0x81, 0x3C, 0x24, 0xFF, 0x00, 0x00, 0x00]),
            "cmpl [rsp],0xff"
        );
    }

    // -----------------------------------------------------------------------
    // fwait (from upstream)
    // -----------------------------------------------------------------------
    #[test]
    fn test_fwait() {
        assert_eq!(dis(&[0x9B]), "fwait");
    }

    // -----------------------------------------------------------------------
    // movmskpd, movmskps (from upstream)
    // -----------------------------------------------------------------------
    #[test]
    fn test_movmskps_rax_xmm0() {
        // 0F 50 C0 = movmskps rax,xmm0
        assert_eq!(dis(&[0x0F, 0x50, 0xC0]), "movmskps rax,xmm0");
    }

    #[test]
    fn test_movmskpd_rax_xmm0() {
        // 66 0F 50 C0 = movmskpd rax,xmm0
        assert_eq!(dis(&[0x66, 0x0F, 0x50, 0xC0]), "movmskpd rax,xmm0");
    }

    // -----------------------------------------------------------------------
    // rdtsc, cpuid (from upstream)
    // -----------------------------------------------------------------------
    #[test]
    fn test_rdtsc() {
        // 0F 31 = rdtsc
        assert_eq!(dis(&[0x0F, 0x31]), "rdtsc");
    }

    #[test]
    fn test_cpuid() {
        // 0F A2 = cpuid
        assert_eq!(dis(&[0x0F, 0xA2]), "cpuid");
    }

    // -----------------------------------------------------------------------
    // movb (byte move)
    // -----------------------------------------------------------------------
    #[test]
    fn test_movb_mem_rax_cl() {
        // 88 08 = movb [rax],cl
        assert_eq!(dis(&[0x88, 0x08]), "movb [rax],cl");
    }

    #[test]
    fn test_movb_imm_to_mem() {
        // C6 00 42 = movb [rax],0x42
        assert_eq!(dis(&[0xC6, 0x00, 0x42]), "movb [rax],0x42");
    }

    // -----------------------------------------------------------------------
    // cmpq [rsp],imm (from LogicalOps64)
    // -----------------------------------------------------------------------
    #[test]
    fn test_cmpq_mem_rsp_imm8() {
        // cmpq [rsp],0xff using imm32 form (81 /7)
        assert_eq!(
            dis(&[0x48, 0x81, 0x3C, 0x24, 0xFF, 0x00, 0x00, 0x00]),
            "cmpq [rsp],0xff"
        );
    }

    // -----------------------------------------------------------------------
    // push immediate (from upstream patterns)
    // -----------------------------------------------------------------------
    #[test]
    fn test_push_imm32_0xff() {
        // 68 FF 00 00 00 = push 0xff
        assert_eq!(dis(&[0x68, 0xFF, 0x00, 0x00, 0x00]), "push 0xff");
    }

    #[test]
    fn test_push_imm32_0x7fffffff() {
        // 68 FF FF FF 7F = push 0x7fffffff
        assert_eq!(
            dis(&[0x68, 0xFF, 0xFF, 0xFF, 0x7F]),
            "push 0x7fffffff"
        );
    }

    // -----------------------------------------------------------------------
    // xorq rax,[rsp] (from Bitwise64)
    // -----------------------------------------------------------------------
    #[test]
    fn test_xorq_rax_mem_rsp() {
        // 48 33 04 24 = xorq rax,[rsp]
        assert_eq!(dis(&[0x48, 0x33, 0x04, 0x24]), "xorq rax,[rsp]");
    }

    // -----------------------------------------------------------------------
    // xorq [rsp],rcx (from Bitwise64)
    // -----------------------------------------------------------------------
    #[test]
    fn test_xorq_mem_rsp_rcx() {
        // 48 31 0C 24 = xorq [rsp],rcx
        assert_eq!(dis(&[0x48, 0x31, 0x0C, 0x24]), "xorq [rsp],rcx");
    }

    // -----------------------------------------------------------------------
    // orq rcx,[rsp] (from Bitwise64)
    // -----------------------------------------------------------------------
    #[test]
    fn test_orq_rcx_mem_rsp() {
        // 48 0B 0C 24 = orq rcx,[rsp]
        assert_eq!(dis(&[0x48, 0x0B, 0x0C, 0x24]), "orq rcx,[rsp]");
    }

    // -----------------------------------------------------------------------
    // shufpd (from PackedDoubleShuffle)
    // -----------------------------------------------------------------------
    #[test]
    fn test_shufpd_xmm10_xmm10() {
        // 66 45 0F C6 D2 33 = shufpd xmm10, xmm10 [33]
        assert_eq!(
            dis(&[0x66, 0x45, 0x0F, 0xC6, 0xD2, 0x33]),
            "shufpd xmm10, xmm10 [33]"
        );
    }

    // -----------------------------------------------------------------------
    // movq xmm from F3 prefix (from upstream movq SSE patterns)
    // -----------------------------------------------------------------------
    #[test]
    fn test_movq_xmm_f3_prefix() {
        // F3 0F 7E C1 = movq xmm0, xmm1 (F3 0F 7E)
        assert_eq!(dis(&[0xF3, 0x0F, 0x7E, 0xC1]), "movq xmm0, xmm1");
    }

    // -----------------------------------------------------------------------
    // movq xmm store form with 66 prefix (movq xmm/m64, xmm)
    // -----------------------------------------------------------------------
    #[test]
    fn test_movq_66_store_xmm() {
        // 66 0F D6 C1 = movq xmm1,xmm0
        assert_eq!(dis(&[0x66, 0x0F, 0xD6, 0xC1]), "movq xmm1,xmm0");
    }
}
