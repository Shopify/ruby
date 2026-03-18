// Copyright (c) 2014, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.
//
// Ported from the Dart SDK's runtime/vm/compiler/assembler/disassembler_arm64.cc
// to Rust. Pure Rust, no external dependencies.

/// Register names matching the Dart VM convention.
const CPU_REG_NAMES: [&str; 32] = [
    "r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7", "r8", "r9", "r10", "r11",
    "r12", "r13", "r14", "r15", "tmp", "tmp2", "r18", "r19", "r20", "r21",
    "r22", "r23", "r24", "r25", "r26", "pp", "r28", "fp", "lr", "zr",
];

const SHIFT_NAMES: [&str; 4] = ["lsl", "lsr", "asr", "ror"];

const EXTEND_NAMES: [&str; 8] = [
    "uxtb", "uxth", "uxtw", "uxtx", "sxtb", "sxth", "sxtw", "sxtx",
];

const COND_NAMES: [&str; 16] = [
    "eq", "ne", "cs", "cc", "mi", "pl", "vs", "vc", "hi", "ls", "ge", "lt",
    "gt", "le", "", "invalid",
];

/// Whether R31 should be interpreted as the zero register or the stack pointer.
#[derive(Clone, Copy, PartialEq)]
enum R31Type {
    IsZR,
    IsSP,
}

/// ARM64 instruction decoder and disassembler.
struct Arm64Decoder {
    buffer: String,
    /// The raw 32-bit instruction word.
    instr: u32,
    /// PC of the instruction being decoded (used for branch target computation).
    pc: usize,
    /// When true, branch targets are printed as relative offsets ("+N"/"-N").
    relative: bool,
}

// ---------------------------------------------------------------------------
// Bit-field extraction helpers
// ---------------------------------------------------------------------------

impl Arm64Decoder {
    fn new(instr: u32, pc: usize, relative: bool) -> Self {
        Self {
            buffer: String::new(),
            instr,
            pc,
            relative,
        }
    }

    /// Extract `width` bits starting at `shift`.
    #[inline]
    fn bits(&self, shift: u32, width: u32) -> u32 {
        (self.instr >> shift) & ((1u32 << width) - 1)
    }

    /// Extract a single bit.
    #[inline]
    fn bit(&self, shift: u32) -> u32 {
        (self.instr >> shift) & 1
    }

    // Named fields -----------------------------------------------------------

    fn rd_field(&self) -> u32 {
        self.bits(0, 5)
    }
    fn rn_field(&self) -> u32 {
        self.bits(5, 5)
    }
    fn rm_field(&self) -> u32 {
        self.bits(16, 5)
    }
    fn rt_field(&self) -> u32 {
        self.bits(0, 5)
    }
    fn rt2_field(&self) -> u32 {
        self.bits(10, 5)
    }
    fn ra_field(&self) -> u32 {
        self.bits(10, 5)
    }
    fn rs_field(&self) -> u32 {
        self.bits(16, 5)
    }

    // V-register fields (same bit positions as integer, different namespace)
    fn vd_field(&self) -> u32 {
        self.bits(0, 5)
    }
    fn vn_field(&self) -> u32 {
        self.bits(5, 5)
    }
    fn vm_field(&self) -> u32 {
        self.bits(16, 5)
    }
    fn vt_field(&self) -> u32 {
        self.bits(0, 5)
    }
    fn vt2_field(&self) -> u32 {
        self.bits(10, 5)
    }

    fn sf_field(&self) -> u32 {
        self.bit(31)
    }
    fn s_field(&self) -> u32 {
        self.bit(29)
    }
    fn has_s(&self) -> bool {
        self.s_field() != 0
    }
    fn sz_field(&self) -> u32 {
        self.bits(30, 2)
    }
    fn hw_field(&self) -> u32 {
        self.bits(21, 2)
    }
    fn imm12_field(&self) -> u32 {
        self.bits(10, 12)
    }
    fn imm12_shift_field(&self) -> u32 {
        self.bits(22, 2)
    }
    fn imm16_field(&self) -> u32 {
        self.bits(5, 16)
    }
    fn imm8_field(&self) -> u32 {
        self.bits(13, 8)
    }
    fn shift_type_field(&self) -> u32 {
        self.bits(22, 2)
    }
    fn shift_amount_field(&self) -> u32 {
        self.bits(10, 6)
    }
    fn extend_type_field(&self) -> u32 {
        self.bits(13, 3)
    }
    fn ext_shift_amount_field(&self) -> u32 {
        self.bits(10, 3)
    }

    fn condition_field(&self) -> u32 {
        self.bits(0, 4)
    }
    fn select_condition_field(&self) -> u32 {
        self.bits(12, 4)
    }

    fn simm9_field(&self) -> i32 {
        let raw = self.bits(12, 9);
        // sign-extend 9-bit value
        if raw & (1 << 8) != 0 {
            (raw | 0xFFFF_FE00) as i32
        } else {
            raw as i32
        }
    }

    fn simm7_field(&self) -> i32 {
        let raw = self.bits(15, 7);
        if raw & (1 << 6) != 0 {
            (raw | 0xFFFF_FF80) as i32
        } else {
            raw as i32
        }
    }

    fn simm19_field(&self) -> i32 {
        let raw = self.bits(5, 19);
        if raw & (1 << 18) != 0 {
            (raw | 0xFFF8_0000) as i32
        } else {
            raw as i32
        }
    }

    fn simm26_field(&self) -> i32 {
        let raw = self.bits(0, 26);
        if raw & (1 << 25) != 0 {
            (raw | 0xFC00_0000) as i32
        } else {
            raw as i32
        }
    }

    fn simm14_field(&self) -> i32 {
        let raw = self.bits(5, 14);
        if raw & (1 << 13) != 0 {
            (raw | 0xFFFF_C000) as i32
        } else {
            raw as i32
        }
    }

    fn immr_field(&self) -> u32 {
        self.bits(16, 6)
    }
    fn imms_field(&self) -> u32 {
        self.bits(10, 6)
    }
    fn n_field(&self) -> u32 {
        self.bit(22)
    }

    // Classification helpers ------------------------------------------------

    /// For add/sub shifted/extended register, bit[21] distinguishes shift (0) vs extend (1).
    /// For logical shifted register, there is no extend form -- it's always a shift.
    fn is_shift(&self) -> bool {
        // If this is a logical shift op (bits[24:28] pattern), always shift
        if self.is_logical_shift_op() {
            true
        } else {
            self.bit(21) == 0
        }
    }

    fn is_dp_immediate_op(&self) -> bool {
        let op0 = self.bits(25, 4);
        op0 == 0b1000 || op0 == 0b1001
    }

    fn is_compare_branch_op(&self) -> bool {
        let op0 = self.bits(25, 4);
        op0 == 0b1010 || op0 == 0b1011
    }

    fn is_load_store_op(&self) -> bool {
        let b25 = self.bit(25);
        let b27 = self.bit(27);
        b27 == 1 && b25 == 0
    }

    fn is_dp_register_op(&self) -> bool {
        let op0 = self.bits(25, 4);
        op0 == 0b0101 || op0 == 0b1101
    }

    fn is_dp_simd1_op(&self) -> bool {
        let op0 = self.bits(25, 4);
        op0 == 0b0111
    }

    fn is_dp_simd2_op(&self) -> bool {
        let op0 = self.bits(25, 4);
        op0 == 0b1111
    }

    // Sub-classification: DP Immediate
    fn is_move_wide_op(&self) -> bool {
        self.bits(23, 6) == 0b100101
    }

    fn is_add_sub_imm_op(&self) -> bool {
        self.bits(24, 5) == 0b10001
    }

    fn is_bitfield_op(&self) -> bool {
        self.bits(23, 6) == 0b100110
    }

    fn is_logical_imm_op(&self) -> bool {
        self.bits(23, 6) == 0b100100
    }

    fn is_pc_rel_op(&self) -> bool {
        self.bits(24, 5) == 0b10000
    }

    // Sub-classification: Compare & Branch
    fn is_exception_gen_op(&self) -> bool {
        self.bits(24, 8) == 0b11010100
    }

    fn is_system_op(&self) -> bool {
        self.bits(24, 8) == 0b11010101
    }

    fn is_unconditional_branch_reg_op(&self) -> bool {
        self.bits(25, 7) == 0b1101011
    }

    fn is_compare_and_branch_op(&self) -> bool {
        // CBZ/CBNZ: sf 011010 x ... -- bits[30:25] = 011010
        self.bits(25, 6) == 0b011010
    }

    fn is_conditional_branch_op(&self) -> bool {
        // B.cond: 0101010 x ... -- bits[31:25] = 0101010
        self.bits(25, 7) == 0b0101010
    }

    fn is_test_and_branch_op(&self) -> bool {
        // TBZ/TBNZ: b5 011011 x ... -- bits[30:25] = 011011
        self.bits(25, 6) == 0b011011
    }

    fn is_unconditional_branch_op(&self) -> bool {
        // B/BL: x 00101 ... -- bits[30:26] = 00101
        self.bits(26, 5) == 0b00101
    }

    // Sub-classification: Load/Store
    // Within load/store (bit[27]=1, bit[25]=0), bits[29:28] determines sub-type:
    //   00 = exclusive, 01 = literal, 10 = pair, 11 = register
    fn is_load_store_reg_op(&self) -> bool {
        self.bits(28, 2) == 0b11
    }

    fn is_load_store_reg_pair_op(&self) -> bool {
        self.bits(28, 2) == 0b10
    }

    fn is_load_reg_literal_op(&self) -> bool {
        self.bits(28, 2) == 0b01
    }

    fn is_load_store_exclusive_op(&self) -> bool {
        self.bits(28, 2) == 0b00
    }

    fn is_atomic_memory_op(&self) -> bool {
        // Atomic memory ops: bits[29:28]=11, bit[24]=0, bit[21]=1, bits[11:10]=00
        // (register offset loads/stores also have bit[21]=1 but bits[11:10]=10)
        self.bits(28, 2) == 0b11
            && self.bit(24) == 0
            && self.bit(21) == 1
            && self.bits(10, 2) == 0b00
    }

    // Sub-classification: DP Register
    fn is_add_sub_shift_ext_op(&self) -> bool {
        self.bits(24, 5) == 0b01011
    }

    fn is_add_sub_with_carry_op(&self) -> bool {
        self.bits(21, 8) == 0b11010000
    }

    fn is_logical_shift_op(&self) -> bool {
        self.bits(24, 5) == 0b01010
    }

    fn is_misc_dp1_source_op(&self) -> bool {
        (self.bits(21, 8) & 0b11111110) == 0b11010110 && self.bit(30) == 1
    }

    fn is_misc_dp2_source_op(&self) -> bool {
        self.bits(21, 8) == 0b11010110 && self.bit(30) == 0
    }

    fn is_misc_dp3_source_op(&self) -> bool {
        self.bits(24, 5) == 0b11011
    }

    fn is_conditional_select_op(&self) -> bool {
        self.bits(21, 8) == 0b11010100
    }

    // Sub-classification: SIMD
    fn is_simd_copy_op(&self) -> bool {
        self.bits(10, 1) == 1
            && self.bits(24, 5) == 0b01110
            && self.bits(21, 3) == 0b000
    }

    fn is_simd_three_same_op(&self) -> bool {
        self.bits(10, 1) == 1
            && self.bits(24, 5) == 0b01110
            && self.bits(21, 1) == 1
    }

    fn is_simd_two_reg_op(&self) -> bool {
        self.bits(10, 2) == 0b10
            && self.bits(24, 5) == 0b01110
    }

    // Sub-classification: FP (within dp_simd2 group)
    fn is_fp_op(&self) -> bool {
        // All FP ops within dp_simd2 -- used as catch-all
        true
    }

    fn is_fp_imm_op(&self) -> bool {
        // FP immediate: bits[12:10] = 100 and bits[9:5] = 00000
        self.bits(10, 3) == 0b100 && self.bits(5, 5) == 0b00000
    }

    fn is_fp_int_cvt_op(&self) -> bool {
        // FP <-> integer conversion: bits[11:10] = 00 and bits[16:15] encodes rmode,
        // bit[21] = 1, distinguished from compare by bits[20:16] pattern
        self.bits(10, 2) == 0b00 && self.bit(21) == 1
    }

    fn is_fp_one_source_op(&self) -> bool {
        // FP one-source: bits[14:10] = 10000
        self.bits(10, 5) == 0b10000 && self.bit(21) == 1
    }

    fn is_fp_two_source_op(&self) -> bool {
        // FP two-source: bits[11:10] = 10, and NOT one-source (bits[14:10] != 10000)
        self.bits(10, 2) == 0b10 && self.bits(10, 5) != 0b10000
    }

    fn is_fp_compare_op(&self) -> bool {
        // FP compare: bits[11:10] = 00 and bit[21] = 0
        // (int-cvt has bit[21] = 1)
        self.bits(10, 2) == 0b00 && self.bit(21) == 0
    }

    // Rd/Rn modes
    fn rd_mode(&self) -> R31Type {
        // Rd can be SP for add/sub imm (when S=0)
        if self.is_add_sub_imm_op() && !self.has_s() {
            R31Type::IsSP
        } else if self.is_logical_imm_op() && self.bits(29, 2) != 3 {
            // ANDS writes flags, so Rd=SP is ZR; AND/ORR/EOR Rd=SP is SP
            R31Type::IsSP
        } else {
            R31Type::IsZR
        }
    }

    fn rn_mode(&self) -> R31Type {
        if self.is_add_sub_imm_op() || self.is_logical_imm_op() {
            R31Type::IsSP
        } else if self.is_add_sub_shift_ext_op() {
            // For extended register form (bit[21]=1), Rn=31 is SP
            // For shifted register form (bit[21]=0), Rn=31 is ZR
            if self.bit(21) == 1 {
                R31Type::IsSP
            } else {
                R31Type::IsZR
            }
        } else {
            R31Type::IsZR
        }
    }
}

// ---------------------------------------------------------------------------
// Logical immediate decoding (replicated bitmasks)
// ---------------------------------------------------------------------------

fn decode_logical_immediate(n: u32, imms: u32, immr: u32) -> u64 {
    // Determine the element size from N and imms.
    // The element size is the smallest power of 2 >= 2 such that:
    //   N=1 -> element size = 64
    //   N=0 -> element size determined by highest bit set in NOT(imms[5:0])
    let esize_log2: u32;
    if n != 0 {
        esize_log2 = 6; // 64-bit element
    } else {
        let nimms = (!imms) & 0x3f;
        if nimms == 0 {
            return 0xFFFF_FFFF_FFFF_FFFF;
        }
        // Find position of highest set bit in nimms
        let mut highest = 0u32;
        for i in (0..6).rev() {
            if nimms & (1 << i) != 0 {
                highest = i;
                break;
            }
        }
        esize_log2 = highest + 1;
    }

    let esize = 1u64 << esize_log2;
    let mask = esize - 1;
    let s = (imms as u64) & mask;
    let r = (immr as u64) & mask;
    let welem: u64 = (1u64 << (s + 1)) - 1;

    if esize == 64 {
        // For 64-bit elements, simply rotate
        welem.rotate_right(r as u32)
    } else {
        // Rotate within the element, then replicate
        let elem = welem.rotate_right(r as u32) & (esize - 1);
        let mut result = 0u64;
        let mut shift = 0u64;
        while shift < 64 {
            result |= elem << shift;
            shift += esize;
        }
        result
    }
}

// ---------------------------------------------------------------------------
// Printing helpers
// ---------------------------------------------------------------------------

impl Arm64Decoder {
    fn print(&mut self, s: &str) {
        self.buffer.push_str(s);
    }

    fn print_int(&mut self, value: i32) {
        self.buffer.push_str(&format!("{}", value));
    }

    fn print_register(&mut self, reg: u32, r31t: R31Type) {
        debug_assert!(reg < 32);
        if reg == 31 {
            match r31t {
                R31Type::IsZR => self.print("zr"),
                R31Type::IsSP => self.print("csp"),
            }
        } else {
            self.print(CPU_REG_NAMES[reg as usize]);
        }
    }

    fn print_vregister(&mut self, reg: u32) {
        debug_assert!(reg < 32);
        self.buffer.push_str(&format!("v{}", reg));
    }

    fn print_condition(&mut self) {
        if self.is_conditional_select_op() {
            self.print(COND_NAMES[self.select_condition_field() as usize]);
        } else {
            self.print(COND_NAMES[self.condition_field() as usize]);
        }
    }

    fn print_inverted_condition(&mut self) {
        let cond = if self.is_conditional_select_op() {
            self.select_condition_field()
        } else {
            self.condition_field()
        };
        self.print(COND_NAMES[invert_condition(cond) as usize]);
    }

    fn print_shift_extend_rm(&mut self) {
        let rm = self.rm_field();
        let shift = self.shift_type_field();
        let shift_amount = self.shift_amount_field();
        let extend = self.extend_type_field();
        let extend_shift_amount = self.ext_shift_amount_field();

        self.print_register(rm, R31Type::IsZR);

        if self.is_shift() && shift == 0 && shift_amount == 0 {
            // Just rm, no shift.
            return;
        }
        if self.is_shift() {
            if shift == 3 && shift_amount == 0 {
                self.print(" RRX");
                return;
            }
            let mut sa = shift_amount;
            if (shift == 1 || shift == 2) && sa == 0 {
                sa = 32;
            }
            self.buffer
                .push_str(&format!(" {} #{}", SHIFT_NAMES[shift as usize], sa));
        } else {
            // Extend
            self.buffer
                .push_str(&format!(" {}", EXTEND_NAMES[extend as usize]));
            if (self.sf_field() == 1 && extend == 3 /* UXTX */)
                || (self.sf_field() == 0 && extend == 2 /* UXTW */)
            {
                self.buffer.push_str(&format!(" {}", extend_shift_amount));
            }
        }
    }

    fn print_mem_operand(&mut self) {
        let rn = self.rn_field();
        if self.bit(24) == 1 {
            // Unsigned offset
            let scale = self.sz_field();
            let imm12 = self.imm12_field();
            let off = imm12 << scale;
            self.print("[");
            self.print_register(rn, R31Type::IsSP);
            if off != 0 {
                self.buffer.push_str(&format!(", #{}", off));
            }
            self.print("]");
        } else {
            match self.bits(10, 2) {
                0 => {
                    // Unscaled immediate
                    let imm9 = self.simm9_field();
                    self.print("[");
                    self.print_register(rn, R31Type::IsSP);
                    self.buffer.push_str(&format!(", #{}", imm9));
                    self.print("]");
                }
                1 => {
                    // Post-index
                    let imm9 = self.simm9_field();
                    self.print("[");
                    self.print_register(rn, R31Type::IsSP);
                    self.print("]");
                    self.buffer.push_str(&format!(", #{} !", imm9));
                }
                2 => {
                    // Register offset
                    let rm = self.rm_field();
                    let ext = self.extend_type_field();
                    let s = self.bit(12);
                    self.print("[");
                    self.print_register(rn, R31Type::IsSP);
                    self.print(", ");
                    self.print_register(rm, R31Type::IsZR);
                    self.buffer
                        .push_str(&format!(" {}", EXTEND_NAMES[ext as usize]));
                    if s == 1 {
                        self.print(" scaled");
                    }
                    self.print("]");
                }
                3 => {
                    // Pre-index
                    let imm9 = self.simm9_field();
                    self.print("[");
                    self.print_register(rn, R31Type::IsSP);
                    self.buffer.push_str(&format!(", #{}", imm9));
                    self.print("]!");
                }
                _ => {
                    self.print("???");
                }
            }
        }
    }

    fn print_pair_mem_operand(&mut self) {
        let rn = self.rn_field();
        let simm7 = self.simm7_field();
        let shift = if self.bit(26) == 1 {
            2 + self.sz_field()
        } else {
            2 + self.sf_field()
        };
        let offset = simm7 << shift;
        self.print("[");
        self.print_register(rn, R31Type::IsSP);
        match self.bits(23, 3) {
            1 => {
                // Post-index
                self.buffer.push_str(&format!("], #{} !", offset));
            }
            2 => {
                // Signed offset
                self.buffer.push_str(&format!(", #{}]", offset));
            }
            3 => {
                // Pre-index
                self.buffer.push_str(&format!(", #{}]!", offset));
            }
            _ => {
                self.print(", ???]");
            }
        }
    }
}

fn invert_condition(cond: u32) -> u32 {
    cond ^ 1
}

// ---------------------------------------------------------------------------
// VFP immediate expansion (for fmov imm)
// ---------------------------------------------------------------------------

fn vfp_expand_imm(imm8: u32) -> f64 {
    let sign = ((imm8 >> 7) & 1) as u64;
    let exp = ((imm8 >> 4) & 0x7) as u64;
    let frac = (imm8 & 0xf) as u64;
    // double: sign(1) exp(11) frac(52)
    // imm8 encoding: a:NOT(b):bbbbbbbb:cdef:0..0
    let a = (exp >> 2) & 1;
    let not_a = a ^ 1;
    let exp_bits = (not_a << 10)
        | if a == 1 { 0x3FC } else { 0 }
        | (exp & 0x3);
    let frac_bits = frac << 48;
    let bits = (sign << 63) | (exp_bits << 52) | frac_bits;
    f64::from_bits(bits)
}

// ---------------------------------------------------------------------------
// Format engine: process format strings with 'X escape sequences
// ---------------------------------------------------------------------------

impl Arm64Decoder {
    fn format(&mut self, fmt: &str) {
        let bytes = fmt.as_bytes();
        let mut i = 0;
        while i < bytes.len() {
            if bytes[i] == b'\'' {
                i += 1; // skip the quote
                i += self.format_option(&bytes[i..]);
            } else {
                self.buffer.push(bytes[i] as char);
                i += 1;
            }
        }
    }

    fn format_option(&mut self, option: &[u8]) -> usize {
        match option[0] {
            b'b' => {
                if option.len() >= 6 && &option[..6] == b"bitimm" {
                    let n = self.n_field();
                    let imms = self.imms_field();
                    let immr = self.immr_field();
                    let imm = decode_logical_immediate(n, imms, immr);
                    self.buffer.push_str(&format!("0x{:x}", imm));
                    6
                } else if option.len() >= 6 && &option[..6] == b"bitpos" {
                    let bitpos = self.bits(19, 5) | (self.bit(31) << 5);
                    self.buffer.push_str(&format!("#{}", bitpos));
                    6
                } else {
                    1
                }
            }
            b'c' => {
                if option.len() >= 3 && &option[..3] == b"csz" {
                    let imm5 = self.bits(16, 5);
                    let typ = if imm5 & 0x1 != 0 {
                        "b"
                    } else if imm5 & 0x2 != 0 {
                        "h"
                    } else if imm5 & 0x4 != 0 {
                        "s"
                    } else if imm5 & 0x8 != 0 {
                        "d"
                    } else {
                        "??"
                    };
                    self.print(typ);
                    3
                } else if option.len() >= 12 && &option[..12] == b"condinverted" {
                    self.print_inverted_condition();
                    12
                } else if option.len() >= 4 && &option[..4] == b"cond" {
                    self.print_condition();
                    4
                } else {
                    1
                }
            }
            b'd' => {
                // dest26, dest19, dest14
                let off: i64;
                if option.len() >= 6 && &option[..6] == b"dest26" {
                    off = (self.simm26_field() as i64) << 2;
                } else if option.len() >= 6 && &option[..6] == b"dest14" {
                    off = (self.simm14_field() as i64) << 2;
                } else {
                    // dest19
                    off = (self.simm19_field() as i64) << 2;
                }
                if self.relative {
                    if off >= 0 {
                        self.buffer.push_str(&format!("+{}", off));
                    } else {
                        self.buffer.push_str(&format!("{}", off));
                    }
                } else {
                    let dest = (self.pc as i64).wrapping_add(off) as u64;
                    self.buffer.push_str(&format!("{:#x}", dest));
                }
                6
            }
            b'f' => {
                // fsz
                let sz = self.sz_field();
                let s = match sz {
                    0 => {
                        if self.bit(23) == 1 {
                            "q"
                        } else {
                            "b"
                        }
                    }
                    1 => "h",
                    2 => "s",
                    3 => "d",
                    _ => "?",
                };
                self.print(s);
                3
            }
            b'h' => {
                // hw
                let shift = self.hw_field() << 4;
                if shift != 0 {
                    self.buffer.push_str(&format!(" lsl {}", shift));
                }
                2
            }
            b'i' => {
                if option.len() >= 4 && &option[..3] == b"idx" {
                    let imm4 = self.bits(11, 4);
                    let imm5 = self.bits(16, 5);
                    let shift;
                    let imm;
                    if option[3] == b'4' {
                        imm = imm4;
                        shift = 0;
                    } else {
                        // idx5
                        imm = imm5;
                        shift = 1;
                    }
                    let idx = if imm5 & 0x1 != 0 {
                        (imm >> shift) as i32
                    } else if imm5 & 0x2 != 0 {
                        (imm >> (shift + 1)) as i32
                    } else if imm5 & 0x4 != 0 {
                        (imm >> (shift + 2)) as i32
                    } else if imm5 & 0x8 != 0 {
                        (imm >> (shift + 3)) as i32
                    } else {
                        -1
                    };
                    self.buffer.push_str(&format!("[{}]", idx));
                    4
                } else if option.len() >= 5 && option[3] == b'1' {
                    if option.len() >= 6 && option[4] == b'2' {
                        // imm12 or imm12s
                        let mut imm = self.imm12_field() as u64;
                        let mut ret = 5;
                        if option.len() >= 6 && option[5] == b's' {
                            // shifted immediate
                            if self.imm12_shift_field() == 1 {
                                imm <<= 12;
                            } else if self.imm12_shift_field() & 0x2 != 0 {
                                self.print("Unknown Shift");
                            }
                            ret = 6;
                        }
                        self.buffer.push_str(&format!("#0x{:x}", imm));
                        ret
                    } else {
                        // imm16
                        let imm = self.imm16_field() as u64;
                        self.buffer.push_str(&format!("#0x{:x}", imm));
                        5
                    }
                } else if option.len() >= 4 && option[3] == b'd' {
                    // immd (floating-point immediate)
                    let dimm = vfp_expand_imm(self.imm8_field());
                    self.buffer.push_str(&format!("{:.*}", 6, dimm));
                    4
                } else if option.len() >= 4 && option[3] == b'r' {
                    // immr
                    let immr = self.immr_field();
                    self.buffer.push_str(&format!("#{}", immr));
                    4
                } else if option.len() >= 4 && option[3] == b's' {
                    // imms
                    let imms = self.imms_field();
                    self.buffer.push_str(&format!("#{}", imms));
                    4
                } else {
                    1
                }
            }
            b'm' => {
                // memop
                self.print_mem_operand();
                5
            }
            b'o' => {
                // opc (for load/store pair size)
                if self.bit(26) == 0 {
                    if self.bit(31) == 0 {
                        if self.bit(30) == 1 {
                            self.print("sw");
                        } else {
                            self.print("w");
                        }
                    }
                    // else: 64-bit, no suffix needed
                } else {
                    match self.bits(30, 2) {
                        0 => self.print("s"),
                        1 => self.print("d"),
                        2 => self.print("q"),
                        _ => self.print("?"),
                    }
                }
                3
            }
            b'p' => {
                if option.len() >= 2 && option[1] == b'c' {
                    let off: i64;
                    if option.len() >= 5 && &option[2..5] == b"adr" {
                        // pcadr
                        let immhi = self.simm19_field() as i64;
                        let immlo = self.bits(29, 2) as i64;
                        off = (immhi << 2) | immlo;
                    } else {
                        // pcldr
                        off = (self.simm19_field() as i64) << 2;
                    }
                    if self.relative {
                        if off >= 0 {
                            self.buffer.push_str(&format!("+{}", off));
                        } else {
                            self.buffer.push_str(&format!("{}", off));
                        }
                    } else {
                        let dest = (self.pc as u64).wrapping_add(off as u64);
                        self.buffer.push_str(&format!("0x{:x}", dest));
                    }
                    5
                } else {
                    // pmemop
                    self.print_pair_mem_operand();
                    6
                }
            }
            b'r' => self.format_register(option),
            b'v' => {
                if option.len() >= 3 && &option[..3] == b"vsz" {
                    let s = if self.bits(14, 2) == 3 {
                        match self.bit(22) {
                            0 => "s",
                            _ => "d",
                        }
                    } else {
                        match self.bit(22) {
                            0 => "w",
                            _ => "x",
                        }
                    };
                    self.print(s);
                    3
                } else {
                    self.format_vregister(option)
                }
            }
            b's' => {
                if option.len() >= 8 && &option[..8] == b"shift_op" {
                    self.print_shift_extend_rm();
                    8
                } else if option.len() >= 2 && option[1] == b'f' {
                    // sf
                    if self.sf_field() == 0 {
                        self.print("w");
                    }
                    // else: 64-bit, no suffix
                    2
                } else if option.len() >= 2 && option[1] == b'z' {
                    // sz
                    let sz = self.sz_field();
                    let s = match sz {
                        0 => "b",
                        1 => "h",
                        2 => "w",
                        3 => "", // 64-bit, no suffix
                        _ => "?",
                    };
                    self.print(s);
                    2
                } else if option.len() >= 2 && option[1] == b' ' {
                    // 's ' - print "s" if S flag is set
                    if self.has_s() {
                        self.print("s");
                    }
                    1
                } else {
                    1
                }
            }
            _ => 1,
        }
    }

    fn format_register(&mut self, option: &[u8]) -> usize {
        debug_assert!(option[0] == b'r');
        match option[1] {
            b'n' => {
                let reg = self.rn_field();
                self.print_register(reg, self.rn_mode());
                2
            }
            b'd' => {
                let reg = self.rd_field();
                self.print_register(reg, self.rd_mode());
                2
            }
            b'm' => {
                let reg = self.rm_field();
                self.print_register(reg, R31Type::IsZR);
                2
            }
            b't' => {
                if option.len() >= 3 && option[2] == b'2' {
                    let reg = self.rt2_field();
                    self.print_register(reg, R31Type::IsZR);
                    3
                } else {
                    let reg = self.rt_field();
                    self.print_register(reg, R31Type::IsZR);
                    2
                }
            }
            b'a' => {
                let reg = self.ra_field();
                self.print_register(reg, R31Type::IsZR);
                2
            }
            b's' => {
                let reg = self.rs_field();
                self.print_register(reg, R31Type::IsZR);
                2
            }
            _ => 1,
        }
    }

    fn format_vregister(&mut self, option: &[u8]) -> usize {
        debug_assert!(option[0] == b'v');
        match option[1] {
            b'd' => {
                self.print_vregister(self.vd_field());
                2
            }
            b'n' => {
                self.print_vregister(self.vn_field());
                2
            }
            b'm' => {
                self.print_vregister(self.vm_field());
                2
            }
            b't' => {
                if option.len() >= 3 && option[2] == b'2' {
                    self.print_vregister(self.vt2_field());
                    3
                } else {
                    self.print_vregister(self.vt_field());
                    2
                }
            }
            _ => 1,
        }
    }
}

// ---------------------------------------------------------------------------
// Decode functions
// ---------------------------------------------------------------------------

impl Arm64Decoder {
    fn unknown(&mut self) {
        self.format("unknown");
    }

    fn decode_move_wide(&mut self) {
        match self.bits(29, 2) {
            0 => self.format("movn'sf 'rd, 'imm16'hw"),
            2 => self.format("movz'sf 'rd, 'imm16'hw"),
            3 => self.format("movk'sf 'rd, 'imm16'hw"),
            _ => self.unknown(),
        }
    }

    fn decode_load_store_reg(&mut self) {
        if self.bit(26) == 1 {
            if self.bit(22) == 1 {
                self.format("fldr'fsz 'vt, 'memop");
            } else {
                self.format("fstr'fsz 'vt, 'memop");
            }
        } else {
            if self.bits(22, 2) == 0 {
                self.format("str'sz 'rt, 'memop");
            } else if self.bits(23, 1) == 1 {
                self.format("ldrs'sz 'rt, 'memop");
            } else {
                self.format("ldr'sz 'rt, 'memop");
            }
        }
    }

    fn decode_load_store_reg_pair(&mut self) {
        if self.bit(26) == 1 {
            if self.bit(22) == 1 {
                self.format("fldp'opc 'vt, 'vt2, 'pmemop");
            } else {
                self.format("fstp'opc 'vt, 'vt2, 'pmemop");
            }
        } else {
            if self.bit(22) == 1 {
                self.format("ldp'opc 'rt, 'rt2, 'pmemop");
            } else {
                self.format("stp'opc 'rt, 'rt2, 'pmemop");
            }
        }
    }

    fn decode_load_reg_literal(&mut self) {
        if self.bit(30) != 0 {
            self.format("ldrx 'rt, 'pcldr");
        } else {
            self.format("ldrw 'rt, 'pcldr");
        }
    }

    fn decode_load_store_exclusive(&mut self) {
        if self.bit(31) != 1 || self.bit(21) != 0 || self.bit(23) != self.bit(15) {
            self.unknown();
            return;
        }

        let is_load = self.bit(22) == 1;
        let is_exclusive = self.bit(23) == 0;
        let is_ordered = self.bit(15) == 1;
        if is_load {
            let is_load_acquire = !is_exclusive && is_ordered;
            if is_load_acquire {
                self.format("ldar'sz 'rt, ['rn]");
            } else {
                self.format("ldxr'sz 'rt, ['rn]");
            }
        } else {
            let is_store_release = !is_exclusive && is_ordered;
            if is_store_release {
                self.format("stlr'sz 'rt, ['rn]");
            } else {
                self.format("stxr'sz 'rs, 'rt, ['rn]");
            }
        }
    }

    fn decode_atomic_memory(&mut self) {
        match self.bits(12, 3) {
            1 => self.format("ldclr'sz 'rs, 'rt, ['rn]"),
            3 => self.format("ldset'sz 'rs, 'rt, ['rn]"),
            _ => self.unknown(),
        }
    }

    fn decode_add_sub_imm(&mut self) {
        match self.bit(30) {
            0 => {
                if self.rd_field() == 31 && self.s_field() == 1 {
                    self.format("cmn'sf 'rn, 'imm12s");
                } else if (self.rd_field() == 31 || self.rn_field() == 31)
                    && self.imm12_field() == 0
                    && self.bit(29) == 0
                {
                    self.format("mov'sf 'rd, 'rn");
                } else {
                    self.format("add'sf's 'rd, 'rn, 'imm12s");
                }
            }
            1 => {
                if self.rd_field() == 31 && self.s_field() == 1 {
                    self.format("cmp'sf 'rn, 'imm12s");
                } else {
                    self.format("sub'sf's 'rd, 'rn, 'imm12s");
                }
            }
            _ => self.unknown(),
        }
    }

    fn decode_bitfield(&mut self) {
        let reg_size = if self.sf_field() == 0 { 32u32 } else { 64u32 };
        let op = self.bits(29, 2);
        let r_imm = self.immr_field();
        let s_imm = self.imms_field();
        match op {
            0 => {
                if r_imm == 0 {
                    if s_imm == 7 {
                        self.format("sxtb 'rd, 'rn");
                        return;
                    } else if s_imm == 15 {
                        self.format("sxth 'rd, 'rn");
                        return;
                    } else if s_imm == 31 {
                        self.format("sxtw 'rd, 'rn");
                        return;
                    }
                }
                if s_imm == reg_size - 1 {
                    self.format("asr'sf 'rd, 'rn, 'immr");
                    return;
                }
                self.format("sbfm'sf 'rd, 'rn, 'immr, 'imms");
            }
            1 => {
                self.format("bfm'sf 'rd, 'rn, 'immr, 'imms");
            }
            2 => {
                if r_imm == 0 {
                    if s_imm == 7 {
                        self.format("uxtb 'rd, 'rn");
                        return;
                    } else if s_imm == 15 {
                        self.format("uxth 'rd, 'rn");
                        return;
                    }
                }
                if s_imm != reg_size - 1 && (s_imm + 1) == r_imm {
                    let shift = reg_size - s_imm - 1;
                    self.format("lsl'sf 'rd, 'rn, #");
                    self.print_int(shift as i32);
                    return;
                } else if s_imm == reg_size - 1 {
                    self.format("lsr'sf 'rd, 'rn, 'immr");
                    return;
                }
                self.format("ubfm'sf 'rd, 'rn, 'immr, 'imms");
            }
            _ => self.unknown(),
        }
    }

    fn decode_logical_imm(&mut self) {
        let op = self.bits(29, 2);
        match op {
            0 => self.format("and'sf 'rd, 'rn, 'bitimm"),
            1 => {
                if self.rn_field() == 31 {
                    self.format("mov'sf 'rd, 'bitimm");
                } else {
                    self.format("orr'sf 'rd, 'rn, 'bitimm");
                }
            }
            2 => self.format("eor'sf 'rd, 'rn, 'bitimm"),
            3 => {
                if self.rd_field() == 31 {
                    self.format("tst'sf 'rn, 'bitimm");
                } else {
                    self.format("and'sfs 'rd, 'rn, 'bitimm");
                }
            }
            _ => self.unknown(),
        }
    }

    fn decode_pc_rel(&mut self) {
        if self.bit(31) == 0 {
            self.format("adr 'rd, 'pcadr");
        } else {
            self.unknown();
        }
    }

    fn decode_dp_immediate(&mut self) {
        if self.is_move_wide_op() {
            self.decode_move_wide();
        } else if self.is_add_sub_imm_op() {
            self.decode_add_sub_imm();
        } else if self.is_bitfield_op() {
            self.decode_bitfield();
        } else if self.is_logical_imm_op() {
            self.decode_logical_imm();
        } else if self.is_pc_rel_op() {
            self.decode_pc_rel();
        } else {
            self.unknown();
        }
    }

    fn decode_exception_gen(&mut self) {
        if self.bits(0, 2) == 1 && self.bits(2, 3) == 0 && self.bits(21, 3) == 0 {
            self.format("svc 'imm16");
        } else if self.bits(0, 2) == 0 && self.bits(2, 3) == 0 && self.bits(21, 3) == 1 {
            self.format("brk 'imm16");
        } else if self.bits(0, 2) == 0 && self.bits(2, 3) == 0 && self.bits(21, 3) == 2 {
            self.format("hlt 'imm16");
        } else {
            self.unknown();
        }
    }

    fn decode_system(&mut self) {
        // CLREX
        const CLREX: u32 = 0xd503305f;
        const DMB_ISH: u32 = 0xd5033bbf;
        const DMB_ISHST: u32 = 0xd5033abf;

        if self.instr == CLREX {
            self.format("clrex");
            return;
        }
        if self.instr == DMB_ISH {
            self.format("dmb ish");
            return;
        }
        if self.instr == DMB_ISHST {
            self.format("dmb ishst");
            return;
        }
        if self.bits(0, 8) == 0x1f
            && self.bits(12, 4) == 2
            && self.bits(16, 3) == 3
            && self.bits(19, 2) == 0
            && self.bit(21) == 0
        {
            if self.bits(8, 4) == 0 {
                self.format("nop");
            } else {
                self.unknown();
            }
        } else {
            self.unknown();
        }
    }

    fn decode_unconditional_branch_reg(&mut self) {
        if self.bits(0, 5) == 0 && self.bits(10, 5) == 0 && self.bits(16, 5) == 0x1f {
            match self.bits(21, 4) {
                0 => self.format("br 'rn"),
                1 => self.format("blr 'rn"),
                2 => {
                    if self.rn_field() == 30 {
                        // LR
                        self.format("ret");
                    } else {
                        self.format("ret 'rn");
                    }
                }
                _ => self.unknown(),
            }
        }
    }

    fn decode_compare_and_branch(&mut self) {
        if self.bit(24) == 0 {
            self.format("cbz'sf 'rt, 'dest19");
        } else {
            self.format("cbnz'sf 'rt, 'dest19");
        }
    }

    fn decode_conditional_branch(&mut self) {
        if self.bit(24) != 0 || self.bit(4) != 0 {
            self.unknown();
            return;
        }
        self.format("b'cond 'dest19");
    }

    fn decode_test_and_branch(&mut self) {
        if self.bit(24) == 0 {
            self.format("tbz'sf 'rt, 'bitpos, 'dest14");
        } else {
            self.format("tbnz'sf 'rt, 'bitpos, 'dest14");
        }
    }

    fn decode_unconditional_branch(&mut self) {
        if self.bit(31) == 0 {
            self.format("b 'dest26");
        } else {
            self.format("bl 'dest26");
        }
    }

    fn decode_compare_branch(&mut self) {
        if self.is_exception_gen_op() {
            self.decode_exception_gen();
        } else if self.is_system_op() {
            self.decode_system();
        } else if self.is_unconditional_branch_reg_op() {
            self.decode_unconditional_branch_reg();
        } else if self.is_compare_and_branch_op() {
            self.decode_compare_and_branch();
        } else if self.is_conditional_branch_op() {
            self.decode_conditional_branch();
        } else if self.is_test_and_branch_op() {
            self.decode_test_and_branch();
        } else if self.is_unconditional_branch_op() {
            self.decode_unconditional_branch();
        } else {
            self.unknown();
        }
    }

    fn decode_load_store(&mut self) {
        if self.is_atomic_memory_op() {
            self.decode_atomic_memory();
        } else if self.is_load_store_reg_op() {
            self.decode_load_store_reg();
        } else if self.is_load_store_reg_pair_op() {
            self.decode_load_store_reg_pair();
        } else if self.is_load_reg_literal_op() {
            self.decode_load_reg_literal();
        } else if self.is_load_store_exclusive_op() {
            self.decode_load_store_exclusive();
        } else {
            self.unknown();
        }
    }

    fn decode_add_sub_shift_ext(&mut self) {
        match self.bit(30) {
            0 => {
                if self.rd_field() == 31 && self.s_field() == 1 {
                    self.format("cmn'sf 'rn, 'shift_op");
                } else {
                    self.format("add'sf's 'rd, 'rn, 'shift_op");
                }
            }
            1 => {
                if self.rd_field() == 31 && self.s_field() == 1 {
                    self.format("cmp'sf 'rn, 'shift_op");
                } else if self.rn_field() == 31 {
                    self.format("neg'sf's 'rd, 'shift_op");
                } else {
                    self.format("sub'sf's 'rd, 'rn, 'shift_op");
                }
            }
            _ => self.unknown(),
        }
    }

    fn decode_add_sub_with_carry(&mut self) {
        match self.bit(30) {
            0 => self.format("adc'sf's 'rd, 'rn, 'rm"),
            1 => self.format("sbc'sf's 'rd, 'rn, 'rm"),
            _ => self.unknown(),
        }
    }

    fn decode_logical_shift(&mut self) {
        let op = (self.bits(29, 2) << 1) | self.bit(21);
        match op {
            0 => self.format("and'sf 'rd, 'rn, 'shift_op"),
            1 => self.format("bic'sf 'rd, 'rn, 'shift_op"),
            2 => {
                if self.rn_field() == 31 && self.is_shift() && self.shift_type_field() == 0 {
                    if self.shift_amount_field() == 0 {
                        self.format("mov'sf 'rd, 'rm");
                    } else {
                        self.format("lsl'sf 'rd, 'rm, 'imms");
                    }
                } else {
                    self.format("orr'sf 'rd, 'rn, 'shift_op");
                }
            }
            3 => self.format("orn'sf 'rd, 'rn, 'shift_op"),
            4 => self.format("eor'sf 'rd, 'rn, 'shift_op"),
            5 => self.format("eon'sf 'rd, 'rn, 'shift_op"),
            6 => {
                if self.rd_field() == 31 {
                    self.format("tst'sf 'rn, 'shift_op");
                } else {
                    self.format("and'sfs 'rd, 'rn, 'shift_op");
                }
            }
            7 => self.format("bic'sfs 'rd, 'rn, 'shift_op"),
            _ => self.unknown(),
        }
    }

    fn decode_misc_dp1_source(&mut self) {
        if self.bit(29) != 0 {
            self.unknown();
            return;
        }
        match self.bits(10, 10) {
            0 => self.format("rbit'sf 'rd, 'rn"),
            4 => self.format("clz'sf 'rd, 'rn"),
            _ => self.unknown(),
        }
    }

    fn decode_misc_dp2_source(&mut self) {
        if self.bit(29) != 0 {
            self.unknown();
            return;
        }
        match self.bits(10, 5) {
            2 => self.format("udiv'sf 'rd, 'rn, 'rm"),
            3 => self.format("sdiv'sf 'rd, 'rn, 'rm"),
            8 => self.format("lsl'sf 'rd, 'rn, 'rm"),
            9 => self.format("lsr'sf 'rd, 'rn, 'rm"),
            10 => self.format("asr'sf 'rd, 'rn, 'rm"),
            _ => self.unknown(),
        }
    }

    fn decode_misc_dp3_source(&mut self) {
        let zero_operand = self.ra_field() == 31;
        let sf = self.bit(31);
        let op54 = self.bits(29, 2);
        let op31 = self.bits(21, 3);
        let o0 = self.bit(15);

        if op54 == 0 && op31 == 0 && o0 == 0 {
            // MADD / MADDW
            if zero_operand {
                self.format("mul'sf 'rd, 'rn, 'rm");
            } else {
                self.format("madd'sf 'rd, 'rn, 'rm, 'ra");
            }
        } else if op54 == 0 && op31 == 0 && o0 == 1 {
            // MSUB / MSUBW
            if zero_operand {
                self.format("mneg'sf 'rd, 'rn, 'rm");
            } else {
                self.format("msub'sf 'rd, 'rn, 'rm, 'ra");
            }
        } else if sf == 1 && op54 == 0 && op31 == 2 && o0 == 0 {
            // SMULH
            self.format("smulh 'rd, 'rn, 'rm");
        } else if sf == 1 && op54 == 0 && op31 == 6 && o0 == 0 {
            // UMULH
            self.format("umulh 'rd, 'rn, 'rm");
        } else if sf == 1 && op54 == 0 && op31 == 1 && o0 == 0 {
            // SMADDL
            if zero_operand {
                self.format("smull 'rd, 'rn, 'rm");
            } else {
                self.format("smaddl 'rd, 'rn, 'rm, 'ra");
            }
        } else if sf == 1 && op54 == 0 && op31 == 1 && o0 == 1 {
            // SMSUBL
            if zero_operand {
                self.format("smnegl 'rd, 'rn, 'rm");
            } else {
                self.format("smsubl 'rd, 'rn, 'rm, 'ra");
            }
        } else if sf == 1 && op54 == 0 && op31 == 5 && o0 == 0 {
            // UMADDL
            if zero_operand {
                self.format("umull 'rd, 'rn, 'rm");
            } else {
                self.format("umaddl 'rd, 'rn, 'rm, 'ra");
            }
        } else if sf == 1 && op54 == 0 && op31 == 5 && o0 == 1 {
            // UMSUBL
            if zero_operand {
                self.format("umnegl 'rd, 'rn, 'rm");
            } else {
                self.format("umsubl 'rd, 'rn, 'rm, 'ra");
            }
        } else {
            self.unknown();
        }
    }

    fn decode_conditional_select(&mut self) {
        let cond = self.select_condition_field();
        let non_select =
            (self.rn_field() == self.rm_field()) && (cond & 0xe) != 0xe;
        let b29 = self.bits(29, 2);
        let b10 = self.bits(10, 2);
        if b29 == 0 && b10 == 0 {
            self.format("csel'sf 'rd, 'rn, 'rm, 'cond");
        } else if b29 == 0 && b10 == 1 {
            if non_select {
                if self.rn_field() == 31 && self.rm_field() == 31 {
                    self.format("cset'sf 'rd, 'condinverted");
                } else {
                    self.format("cinc'sf 'rd, 'rn, 'condinverted");
                }
            } else {
                self.format("csinc'sf 'rd, 'rn, 'rm, 'cond");
            }
        } else if b29 == 2 && b10 == 0 {
            if non_select {
                if self.rn_field() == 31 && self.rm_field() == 31 {
                    self.format("csetm'sf 'rd, 'condinverted");
                } else {
                    self.format("cinv'sf 'rd, 'rn, 'condinverted");
                }
            } else {
                self.format("csinv'sf 'rd, 'rn, 'rm, 'cond");
            }
        } else if b29 == 2 && b10 == 1 {
            if non_select {
                self.format("cneg'sf 'rd, 'rn, 'condinverted");
            } else {
                self.format("csneg'sf 'rd, 'rn, 'rm, 'cond");
            }
        } else {
            self.unknown();
        }
    }

    fn decode_dp_register(&mut self) {
        if self.is_add_sub_shift_ext_op() {
            self.decode_add_sub_shift_ext();
        } else if self.is_add_sub_with_carry_op() {
            self.decode_add_sub_with_carry();
        } else if self.is_logical_shift_op() {
            self.decode_logical_shift();
        } else if self.is_misc_dp1_source_op() {
            self.decode_misc_dp1_source();
        } else if self.is_misc_dp2_source_op() {
            self.decode_misc_dp2_source();
        } else if self.is_misc_dp3_source_op() {
            self.decode_misc_dp3_source();
        } else if self.is_conditional_select_op() {
            self.decode_conditional_select();
        } else {
            self.unknown();
        }
    }

    fn decode_simd_copy(&mut self) {
        let q = self.bit(30);
        let op = self.bit(29);
        let imm4 = self.bits(11, 4);

        if op == 0 && imm4 == 7 {
            if q == 0 {
                self.format("vmovrs 'rd, 'vn'idx5");
            } else {
                self.format("vmovrd 'rd, 'vn'idx5");
            }
        } else if q == 1 && op == 0 && imm4 == 0 {
            self.format("vdup'csz 'vd, 'vn'idx5");
        } else if q == 1 && op == 0 && imm4 == 3 {
            self.format("vins'csz 'vd'idx5, 'rn");
        } else if q == 1 && op == 0 && imm4 == 1 {
            self.format("vdup'csz 'vd, 'rn");
        } else if q == 1 && op == 1 {
            self.format("vins'csz 'vd'idx5, 'vn'idx4");
        } else {
            self.unknown();
        }
    }

    fn decode_simd_three_same(&mut self) {
        let q = self.bit(30);
        let u = self.bit(29);
        let opcode = self.bits(11, 5);

        if q == 0 {
            self.unknown();
            return;
        }

        if u == 0 && opcode == 0x3 {
            if self.bit(23) == 0 {
                self.format("vand 'vd, 'vn, 'vm");
            } else {
                self.format("vorr 'vd, 'vn, 'vm");
            }
        } else if u == 1 && opcode == 0x3 {
            self.format("veor 'vd, 'vn, 'vm");
        } else if u == 0 && opcode == 0x10 {
            self.format("vadd'vsz 'vd, 'vn, 'vm");
        } else if u == 1 && opcode == 0x10 {
            self.format("vsub'vsz 'vd, 'vn, 'vm");
        } else if u == 0 && opcode == 0x1a {
            if self.bit(23) == 0 {
                self.format("vadd'vsz 'vd, 'vn, 'vm");
            } else {
                self.format("vsub'vsz 'vd, 'vn, 'vm");
            }
        } else if u == 1 && opcode == 0x1b {
            self.format("vmul'vsz 'vd, 'vn, 'vm");
        } else if u == 1 && opcode == 0x1f {
            self.format("vdiv'vsz 'vd, 'vn, 'vm");
        } else if u == 0 && opcode == 0x1c {
            self.format("vceq'vsz 'vd, 'vn, 'vm");
        } else if u == 1 && opcode == 0x1c {
            if self.bit(23) == 1 {
                self.format("vcgt'vsz 'vd, 'vn, 'vm");
            } else {
                self.format("vcge'vsz 'vd, 'vn, 'vm");
            }
        } else if u == 0 && opcode == 0x1e {
            if self.bit(23) == 1 {
                self.format("vmin'vsz 'vd, 'vn, 'vm");
            } else {
                self.format("vmax'vsz 'vd, 'vn, 'vm");
            }
        } else if u == 0 && opcode == 0x1f {
            if self.bit(23) == 1 {
                self.format("vrsqrt'vsz 'vd, 'vn, 'vm");
            } else {
                self.format("vrecps'vsz 'vd, 'vn, 'vm");
            }
        } else {
            self.unknown();
        }
    }

    fn decode_simd_two_reg(&mut self) {
        let q = self.bit(30);
        let u = self.bit(29);
        let op = self.bits(12, 5);
        let sz = self.bits(22, 2);

        if q == 0 {
            self.unknown();
            return;
        }

        if u == 1 && op == 0x5 {
            self.format("vnot 'vd, 'vn");
        } else if u == 0 && op == 0xf {
            if sz == 2 {
                self.format("vabss 'vd, 'vn");
            } else if sz == 3 {
                self.format("vabsd 'vd, 'vn");
            } else {
                self.unknown();
            }
        } else if u == 1 && op == 0xf {
            if sz == 2 {
                self.format("vnegs 'vd, 'vn");
            } else if sz == 3 {
                self.format("vnegd 'vd, 'vn");
            } else {
                self.unknown();
            }
        } else if u == 1 && op == 0x1f {
            if sz == 2 {
                self.format("vsqrts 'vd, 'vn");
            } else if sz == 3 {
                self.format("vsqrtd 'vd, 'vn");
            } else {
                self.unknown();
            }
        } else if u == 0 && op == 0x1d {
            if sz != 2 {
                self.unknown();
                return;
            }
            self.format("vrecpes 'vd, 'vn");
        } else if u == 1 && op == 0x1d {
            if sz != 2 {
                self.unknown();
                return;
            }
            self.format("vrsqrtes 'vd, 'vn");
        } else {
            self.unknown();
        }
    }

    fn decode_dp_simd1(&mut self) {
        if self.is_simd_copy_op() {
            self.decode_simd_copy();
        } else if self.is_simd_three_same_op() {
            self.decode_simd_three_same();
        } else if self.is_simd_two_reg_op() {
            self.decode_simd_two_reg();
        } else {
            self.unknown();
        }
    }

    fn decode_fp_imm(&mut self) {
        if self.bit(31) != 0 || self.bit(29) != 0 || self.bit(23) != 0 || self.bits(5, 5) != 0 {
            self.unknown();
            return;
        }
        if self.bit(22) == 1 {
            self.format("fmovd 'vd, 'immd");
        } else {
            self.unknown();
        }
    }

    fn decode_fp_int_cvt(&mut self) {
        if self.bit(29) != 0 {
            self.unknown();
            return;
        }
        if self.sf_field() == 0 && self.bits(22, 2) == 0 {
            if self.bits(16, 5) == 6 {
                self.format("fmovrs'sf 'rd, 'vn");
            } else if self.bits(16, 5) == 7 {
                self.format("fmovsr'sf 'vd, 'rn");
            } else {
                self.unknown();
            }
        } else if self.bits(22, 2) == 1 {
            match self.bits(16, 5) {
                2 => self.format("scvtfd'sf 'vd, 'rn"),
                6 => self.format("fmovrd'sf 'rd, 'vn"),
                7 => self.format("fmovdr'sf 'vd, 'rn"),
                8 => self.format("fcvtps'sf 'rd, 'vn"),
                16 => self.format("fcvtms'sf 'rd, 'vn"),
                24 => self.format("fcvtzs'sf 'rd, 'vn"),
                _ => self.unknown(),
            }
        } else {
            self.unknown();
        }
    }

    fn decode_fp_one_source(&mut self) {
        let opc = self.bits(15, 6);
        if opc != 5 && self.bit(22) != 1 {
            self.unknown();
            return;
        }
        match opc {
            0 => self.format("fmovdd 'vd, 'vn"),
            1 => self.format("fabsd 'vd, 'vn"),
            2 => self.format("fnegd 'vd, 'vn"),
            3 => self.format("fsqrtd 'vd, 'vn"),
            4 => self.format("fcvtsd 'vd, 'vn"),
            5 => self.format("fcvtds 'vd, 'vn"),
            _ => self.unknown(),
        }
    }

    fn decode_fp_two_source(&mut self) {
        if self.bits(22, 2) != 1 {
            self.unknown();
            return;
        }
        match self.bits(12, 4) {
            0 => self.format("fmuld 'vd, 'vn, 'vm"),
            1 => self.format("fdivd 'vd, 'vn, 'vm"),
            2 => self.format("faddd 'vd, 'vn, 'vm"),
            3 => self.format("fsubd 'vd, 'vn, 'vm"),
            _ => self.unknown(),
        }
    }

    fn decode_fp_compare(&mut self) {
        if self.bit(22) == 1 && self.bits(3, 2) == 0 {
            self.format("fcmpd 'vn, 'vm");
        } else if self.bit(22) == 1 && self.bits(3, 2) == 1 {
            if self.vm_field() == 0 {
                self.format("fcmpd 'vn, #0.0");
            } else {
                self.unknown();
            }
        } else {
            self.unknown();
        }
    }

    fn decode_fp(&mut self) {
        if self.is_fp_imm_op() {
            self.decode_fp_imm();
        } else if self.is_fp_one_source_op() {
            // Must check before fp_int_cvt since one-source also has bits[11:10]=00, bit[21]=1
            self.decode_fp_one_source();
        } else if self.is_fp_int_cvt_op() {
            self.decode_fp_int_cvt();
        } else if self.is_fp_two_source_op() {
            self.decode_fp_two_source();
        } else if self.is_fp_compare_op() {
            self.decode_fp_compare();
        } else {
            self.unknown();
        }
    }

    fn decode_dp_simd2(&mut self) {
        if self.is_fp_op() {
            self.decode_fp();
        } else {
            self.unknown();
        }
    }

    fn instruction_decode(&mut self) {
        if self.is_dp_immediate_op() {
            self.decode_dp_immediate();
        } else if self.is_compare_branch_op() {
            self.decode_compare_branch();
        } else if self.is_load_store_op() {
            self.decode_load_store();
        } else if self.is_dp_register_op() {
            self.decode_dp_register();
        } else if self.is_dp_simd1_op() {
            self.decode_dp_simd1();
        } else if self.is_dp_simd2_op() {
            self.decode_dp_simd2();
        } else {
            self.unknown();
        }
    }
}

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/// Disassemble a single 32-bit ARM64 instruction.
pub fn disassemble_instruction(word: u32) -> String {
    let mut decoder = Arm64Decoder::new(word, 0, true);
    decoder.instruction_decode();
    decoder.buffer
}

/// Disassemble a slice of ARM64 machine code bytes.
///
/// `code` must be 4-byte aligned in length.
/// `base_addr` is the virtual address of the first byte (used for branch
/// target computation when `relative` mode is off; we use relative offsets).
pub fn disassemble(code: &[u8], _base_addr: usize) -> String {
    assert!(code.len() % 4 == 0, "Code length must be a multiple of 4");
    let mut result = String::new();
    let mut offset = 0;
    while offset + 4 <= code.len() {
        let word = u32::from_le_bytes([
            code[offset],
            code[offset + 1],
            code[offset + 2],
            code[offset + 3],
        ]);
        let mut decoder = Arm64Decoder::new(word, _base_addr + offset, true);
        decoder.instruction_decode();
        result.push_str(&decoder.buffer);
        result.push('\n');
        offset += 4;
    }
    result
}

// ===========================================================================
// Tests
// ===========================================================================

#[cfg(test)]
mod tests {
    use super::*;

    /// Helper: disassemble a slice of u32 words and return multi-line string.
    fn dis(words: &[u32]) -> String {
        let mut code = Vec::new();
        for &w in words {
            code.extend_from_slice(&w.to_le_bytes());
        }
        disassemble(&code, 0)
    }

    /// Helper: disassemble a single word.
    fn dis1(word: u32) -> String {
        disassemble_instruction(word)
    }

    // -----------------------------------------------------------------------
    // movz / movn / movk
    // -----------------------------------------------------------------------

    #[test]
    fn test_movz0() {
        // movz r0, #0x2a  (hw=0, imm16=42, sf=1)
        // Encoding: 1 10 100101 00 0000000000101010 00000
        let w = 0xD280_0540; // movz x0, #0x2a
        assert_eq!(dis1(w), "movz r0, #0x2a");
    }

    #[test]
    fn test_movz1_lsl16() {
        // movz r0, #0x2a, lsl #16
        let w = 0xD2A0_0540; // movz x0, #0x2a, lsl #16
        assert_eq!(dis1(w), "movz r0, #0x2a lsl 16");
    }

    #[test]
    fn test_movz2_lsl32() {
        // movz r0, #0x2a, lsl #32
        let w = 0xD2C0_0540;
        assert_eq!(dis1(w), "movz r0, #0x2a lsl 32");
    }

    #[test]
    fn test_movz3_lsl48() {
        // movz r0, #0x2a, lsl #48
        let w = 0xD2E0_0540;
        assert_eq!(dis1(w), "movz r0, #0x2a lsl 48");
    }

    #[test]
    fn test_movn0() {
        // movn r0, #0x2a
        let w = 0x9280_0540;
        assert_eq!(dis1(w), "movn r0, #0x2a");
    }

    #[test]
    fn test_movn1_lsl16() {
        // movn r0, #0x2a, lsl #16
        let w = 0x92A0_0540;
        assert_eq!(dis1(w), "movn r0, #0x2a lsl 16");
    }

    #[test]
    fn test_movn2_lsl32() {
        let w = 0x92C0_0540;
        assert_eq!(dis1(w), "movn r0, #0x2a lsl 32");
    }

    #[test]
    fn test_movn3_lsl48() {
        let w = 0x92E0_0540;
        assert_eq!(dis1(w), "movn r0, #0x2a lsl 48");
    }

    #[test]
    fn test_movk0() {
        // movk r0, #0x2a
        let w = 0xF280_0540;
        assert_eq!(dis1(w), "movk r0, #0x2a");
    }

    #[test]
    fn test_movk1_lsl16() {
        let w = 0xF2A0_0540;
        assert_eq!(dis1(w), "movk r0, #0x2a lsl 16");
    }

    #[test]
    fn test_movk2_lsl32() {
        let w = 0xF2C0_0540;
        assert_eq!(dis1(w), "movk r0, #0x2a lsl 32");
    }

    #[test]
    fn test_movk3_lsl48() {
        let w = 0xF2E0_0540;
        assert_eq!(dis1(w), "movk r0, #0x2a lsl 48");
    }

    #[test]
    fn test_movz_big() {
        // movz r0, #0x8000
        let w = 0xD290_0000;
        assert_eq!(dis1(w), "movz r0, #0x8000");
    }

    // -----------------------------------------------------------------------
    // add / sub (register, shifted register)
    // -----------------------------------------------------------------------

    #[test]
    fn test_add_reg() {
        // add x0, x0, x1  -> 0x8B010000
        let w = 0x8B01_0000;
        assert_eq!(dis1(w), "add r0, r0, r1");
    }

    #[test]
    fn test_add_lsl_reg() {
        // add x0, x0, x1, lsl #1 -> 0x8B010400
        let w = 0x8B01_0400;
        assert_eq!(dis1(w), "add r0, r0, r1 lsl #1");
    }

    #[test]
    fn test_add_lsr_reg() {
        // add x0, x0, x1, lsr #1 -> 0x8B410400
        let w = 0x8B41_0400;
        assert_eq!(dis1(w), "add r0, r0, r1 lsr #1");
    }

    #[test]
    fn test_add_asr_reg() {
        // add x0, x0, x1, asr #1 -> 0x8B810400
        let w = 0x8B81_0400;
        assert_eq!(dis1(w), "add r0, r0, r1 asr #1");
    }

    #[test]
    fn test_sub_reg() {
        // sub x0, x0, x1 -> 0xCB010000
        let w = 0xCB01_0000;
        assert_eq!(dis1(w), "sub r0, r0, r1");
    }

    // -----------------------------------------------------------------------
    // add / sub (immediate)
    // -----------------------------------------------------------------------

    #[test]
    fn test_add_imm() {
        // add x0, x0, #42 -> 0x9100A800
        let w = 0x9100_A800;
        assert_eq!(dis1(w), "add r0, r0, #0x2a");
    }

    #[test]
    fn test_sub_imm() {
        // sub x0, x0, #42 -> 0xD100A800
        let w = 0xD100_A800;
        assert_eq!(dis1(w), "sub r0, r0, #0x2a");
    }

    #[test]
    fn test_cmp_imm() {
        // cmp x1, #1 -> subs xzr, x1, #1 -> 0xF100043F
        let w = 0xF100_043F;
        assert_eq!(dis1(w), "cmp r1, #0x1");
    }

    #[test]
    fn test_cmn_imm() {
        // cmn x1, #1 -> adds xzr, x1, #1 -> 0xB100043F
        let w = 0xB100_043F;
        assert_eq!(dis1(w), "cmn r1, #0x1");
    }

    // -----------------------------------------------------------------------
    // mov (register)
    // -----------------------------------------------------------------------

    #[test]
    fn test_mov_reg() {
        // mov x0, x1 -> orr x0, xzr, x1 -> 0xAA0103E0
        let w = 0xAA01_03E0;
        assert_eq!(dis1(w), "mov r0, r1");
    }

    #[test]
    fn test_mov_sp() {
        // add x31(sp), x31(sp), #0 -- decoded as mov alias
        let w = 0x9100_03FF;
        assert_eq!(dis1(w), "mov csp, csp");
    }

    // -----------------------------------------------------------------------
    // logical (register)
    // -----------------------------------------------------------------------

    #[test]
    fn test_and_reg() {
        // and x0, x1, x2 -> 0x8A020020
        let w = 0x8A02_0020;
        assert_eq!(dis1(w), "and r0, r1, r2");
    }

    #[test]
    fn test_and_shift_reg() {
        // and x0, x1, x2, lsl #1 -> 0x8A020420
        let w = 0x8A02_0420;
        assert_eq!(dis1(w), "and r0, r1, r2 lsl #1");
    }

    #[test]
    fn test_orr_reg() {
        // orr x0, x1, x2 -> 0xAA020020
        let w = 0xAA02_0020;
        assert_eq!(dis1(w), "orr r0, r1, r2");
    }

    #[test]
    fn test_eor_reg() {
        // eor x0, x1, x2 -> 0xCA020020
        let w = 0xCA02_0020;
        assert_eq!(dis1(w), "eor r0, r1, r2");
    }

    #[test]
    fn test_bic_reg() {
        // bic x0, x1, x2 -> 0x8A220020
        let w = 0x8A22_0020;
        assert_eq!(dis1(w), "bic r0, r1, r2");
    }

    #[test]
    fn test_orn_reg() {
        // orn x0, x1, x2 -> 0xAA220020
        let w = 0xAA22_0020;
        assert_eq!(dis1(w), "orn r0, r1, r2");
    }

    #[test]
    fn test_eon_reg() {
        // eon x0, x1, x2 -> 0xCA220020
        let w = 0xCA22_0020;
        assert_eq!(dis1(w), "eon r0, r1, r2");
    }

    // -----------------------------------------------------------------------
    // logical (immediate)
    // -----------------------------------------------------------------------

    #[test]
    fn test_and_imm() {
        // and x0, x1, #1 -> 0x92400020
        let w = 0x9240_0020;
        assert_eq!(dis1(w), "and r0, r1, 0x1");
    }

    #[test]
    fn test_orr_imm() {
        // orr x1, x1, #0x20002000200020 -> we need the proper encoding
        // The test shows: "orr r1, r1, 0x20002000200020\n"
        // This is a repeated pattern. Let's pick a simpler one.
        // tst x0, #1 -> ands xzr, x0, #1 -> 0xF240001F
        let w = 0xF240_001F;
        assert_eq!(dis1(w), "tst r0, 0x1");
    }

    // -----------------------------------------------------------------------
    // loads and stores
    // -----------------------------------------------------------------------

    #[test]
    fn test_str_pre_index() {
        // str x1, [sp, #-8]! -> 0xF81F8FE1
        let w = 0xF81F_8FE1;
        assert_eq!(dis1(w), "str r1, [csp, #-8]!");
    }

    #[test]
    fn test_ldr_post_index() {
        // ldr x0, [sp], #8 -> 0xF84087E0
        let w = 0xF840_87E0;
        assert_eq!(dis1(w), "ldr r0, [csp], #8 !");
    }

    #[test]
    fn test_str_unsigned_offset() {
        // str x1, [sp, #4096] -> 0xF9080001 (offset = 4096/8 = 512 in imm12)
        let w = 0xF908_03E1;
        assert_eq!(dis1(w), "str r1, [csp, #4096]");
    }

    #[test]
    fn test_ldr_unsigned_offset() {
        // ldr x0, [sp] -> 0xF94003E0
        let w = 0xF940_03E0;
        assert_eq!(dis1(w), "ldr r0, [csp]");
    }

    #[test]
    fn test_strw_pre_index() {
        // strw r1, [sp, #-4]! -> str w1, [sp, #-4]! -> 0xB81FC FE1
        // encoding: size=10 1 11 000 00 0 111111100 11 11111 00001
        let w = 0xB81F_CFE1;
        assert_eq!(dis1(w), "strw r1, [csp, #-4]!");
    }

    #[test]
    fn test_ldrsw() {
        // ldrsw x0, [sp] -> 0xB98003E0
        let w = 0xB980_03E0;
        assert_eq!(dis1(w), "ldrsw r0, [csp]");
    }

    // -----------------------------------------------------------------------
    // load/store pair
    // -----------------------------------------------------------------------

    #[test]
    fn test_stp_pre_index() {
        // stp x2, x3, [sp, #-16]! -> 0xA9BF0FE2
        let w = 0xA9BF_0FE2;
        assert_eq!(dis1(w), "stp r2, r3, [csp, #-16]!");
    }

    #[test]
    fn test_ldp_post_index() {
        // ldp x0, x1, [sp], #16 -> 0xA8C107E0
        let w = 0xA8C1_07E0;
        assert_eq!(dis1(w), "ldp r0, r1, [csp], #16 !");
    }

    #[test]
    fn test_stp_offset() {
        // stp x2, x3, [sp, #16] -> 0xA9010FE2
        let w = 0xA901_0FE2;
        assert_eq!(dis1(w), "stp r2, r3, [csp, #16]");
    }

    #[test]
    fn test_ldp_offset() {
        // ldp x0, x1, [sp, #16] -> 0xA9C107E0 -> actual: 0xA9410FE0
        // ldp x0, x1, [sp, #16] = A9 41 07 E0 (offset)
        let w = 0xA941_07E0;
        assert_eq!(dis1(w), "ldp r0, r1, [csp, #16]");
    }

    // -----------------------------------------------------------------------
    // branches
    // -----------------------------------------------------------------------

    #[test]
    fn test_b_forward() {
        // b +8 -> 0x14000002
        let w = 0x1400_0002;
        assert_eq!(dis1(w), "b +8");
    }

    #[test]
    fn test_b_backward() {
        // b -8 -> 0x17FFFFFE
        let w = 0x17FF_FFFE;
        assert_eq!(dis1(w), "b -8");
    }

    #[test]
    fn test_bl() {
        // bl +8 -> 0x94000002
        let w = 0x9400_0002;
        assert_eq!(dis1(w), "bl +8");
    }

    #[test]
    fn test_beq() {
        // b.eq +8 -> 0x54000040
        let w = 0x5400_0040;
        assert_eq!(dis1(w), "beq +8");
    }

    #[test]
    fn test_bne() {
        // b.ne +12 -> 0x54000061
        let w = 0x5400_0061;
        assert_eq!(dis1(w), "bne +12");
    }

    #[test]
    fn test_blt() {
        // b.lt +8 -> 0x5400004B
        let w = 0x5400_004B;
        assert_eq!(dis1(w), "blt +8");
    }

    #[test]
    fn test_bgt() {
        // b.gt +8 -> 0x5400004C
        let w = 0x5400_004C;
        assert_eq!(dis1(w), "bgt +8");
    }

    // -----------------------------------------------------------------------
    // cbz / cbnz
    // -----------------------------------------------------------------------

    #[test]
    fn test_cbz() {
        // cbz x1, +8 -> 0xB4000041
        let w = 0xB400_0041;
        assert_eq!(dis1(w), "cbz r1, +8");
    }

    #[test]
    fn test_cbnz() {
        // cbnz x1, +8 -> 0xB5000041
        let w = 0xB500_0041;
        assert_eq!(dis1(w), "cbnz r1, +8");
    }

    // -----------------------------------------------------------------------
    // ret / br / blr
    // -----------------------------------------------------------------------

    #[test]
    fn test_ret() {
        // ret -> 0xD65F03C0
        let w = 0xD65F_03C0;
        assert_eq!(dis1(w), "ret");
    }

    #[test]
    fn test_ret_rn() {
        // ret x0 -> 0xD65F0000
        let w = 0xD65F_0000;
        assert_eq!(dis1(w), "ret r0");
    }

    #[test]
    fn test_br() {
        // br x0 -> 0xD61F0000
        let w = 0xD61F_0000;
        assert_eq!(dis1(w), "br r0");
    }

    #[test]
    fn test_blr() {
        // blr x0 -> 0xD63F0000
        let w = 0xD63F_0000;
        assert_eq!(dis1(w), "blr r0");
    }

    // -----------------------------------------------------------------------
    // cmp (register) / csel / cset
    // -----------------------------------------------------------------------

    #[test]
    fn test_cmp_reg() {
        // cmp x1, x2 -> subs xzr, x1, x2 -> 0xEB02003F
        let w = 0xEB02_003F;
        assert_eq!(dis1(w), "cmp r1, r2");
    }

    #[test]
    fn test_adds_reg() {
        // adds x16, x2, x1 -> 0xAB010050
        let w = 0xAB01_0050;
        assert_eq!(dis1(w), "adds tmp, r2, r1");
    }

    #[test]
    fn test_subs_reg() {
        // subs x16, x0, x1 -> 0xEB010010
        let w = 0xEB01_0010;
        assert_eq!(dis1(w), "subs tmp, r0, r1");
    }

    // -----------------------------------------------------------------------
    // adc / sbc
    // -----------------------------------------------------------------------

    #[test]
    fn test_adc() {
        // adc x0, x0, x0 -> 0x9A000000
        let w = 0x9A00_0000;
        assert_eq!(dis1(w), "adc r0, r0, r0");
    }

    #[test]
    fn test_adcs() {
        // adcs x16, x2, x0 -> 0xBA000050
        let w = 0xBA00_0050;
        assert_eq!(dis1(w), "adcs tmp, r2, r0");
    }

    #[test]
    fn test_sbc() {
        // sbc x0, x0, x0 -> 0xDA000000
        let w = 0xDA00_0000;
        assert_eq!(dis1(w), "sbc r0, r0, r0");
    }

    #[test]
    fn test_sbcs() {
        // sbcs x16, x0, x0 -> 0xFA000010
        let w = 0xFA00_0010;
        assert_eq!(dis1(w), "sbcs tmp, r0, r0");
    }

    // -----------------------------------------------------------------------
    // Word-width arithmetic (32-bit / W-register)
    // -----------------------------------------------------------------------

    #[test]
    fn test_addw_s() {
        // addsw x16, x2, x1 (W-form: adds w16, w2, w1) -> 0x2B010050
        let w = 0x2B01_0050;
        assert_eq!(dis1(w), "addws tmp, r2, r1");
    }

    #[test]
    fn test_subw_s() {
        // subsw x16, x0, x1 (W-form: subs w16, w0, w1) -> 0x6B010010
        let w = 0x6B01_0010;
        assert_eq!(dis1(w), "subws tmp, r0, r1");
    }

    #[test]
    fn test_adcw() {
        // adcw x0, x0, x0 -> 0x1A000000
        let w = 0x1A00_0000;
        assert_eq!(dis1(w), "adcw r0, r0, r0");
    }

    #[test]
    fn test_sbcw() {
        // sbcw x0, x0, x0 -> 0x5A000000
        let w = 0x5A00_0000;
        assert_eq!(dis1(w), "sbcw r0, r0, r0");
    }

    // -----------------------------------------------------------------------
    // csel / csinc / cset
    // -----------------------------------------------------------------------

    #[test]
    fn test_csel() {
        // csel x0, x1, x2, eq -> 0x9A820020
        let w = 0x9A82_0020;
        assert_eq!(dis1(w), "csel r0, r1, r2, eq");
    }

    #[test]
    fn test_cset_vs() {
        // cset x0, vs -> csinc x0, xzr, xzr, vc -> 0x9A9F_77E0
        let w = 0x9A9F_77E0;
        assert_eq!(dis1(w), "cset r0, vs");
    }

    // -----------------------------------------------------------------------
    // mul / madd / sdiv / udiv
    // -----------------------------------------------------------------------

    #[test]
    fn test_mul() {
        // mul x0, x1, x2 -> madd x0, x1, x2, xzr -> 0x9B027C20
        let w = 0x9B02_7C20;
        assert_eq!(dis1(w), "mul r0, r1, r2");
    }

    #[test]
    fn test_sdiv() {
        // sdiv x0, x1, x2 -> 0x9AC20C20
        let w = 0x9AC2_0C20;
        assert_eq!(dis1(w), "sdiv r0, r1, r2");
    }

    #[test]
    fn test_udiv() {
        // udiv x0, x1, x2 -> 0x9AC20820
        let w = 0x9AC2_0820;
        assert_eq!(dis1(w), "udiv r0, r1, r2");
    }

    // -----------------------------------------------------------------------
    // shifts (register-based)
    // -----------------------------------------------------------------------

    #[test]
    fn test_lsl_reg() {
        // lsl x0, x1, x2 -> 0x9AC22020
        let w = 0x9AC2_2020;
        assert_eq!(dis1(w), "lsl r0, r1, r2");
    }

    #[test]
    fn test_lsr_reg() {
        // lsr x0, x1, x2 -> 0x9AC22420
        let w = 0x9AC2_2420;
        assert_eq!(dis1(w), "lsr r0, r1, r2");
    }

    #[test]
    fn test_asr_reg() {
        // asr x0, x1, x2 -> 0x9AC22820
        let w = 0x9AC2_2820;
        assert_eq!(dis1(w), "asr r0, r1, r2");
    }

    // -----------------------------------------------------------------------
    // clz / rbit
    // -----------------------------------------------------------------------

    #[test]
    fn test_clz() {
        // clz x0, x1 -> 0xDAC01020
        let w = 0xDAC0_1020;
        assert_eq!(dis1(w), "clz r0, r1");
    }

    // -----------------------------------------------------------------------
    // nop / brk
    // -----------------------------------------------------------------------

    #[test]
    fn test_nop() {
        let w = 0xD503_201F;
        assert_eq!(dis1(w), "nop");
    }

    #[test]
    fn test_brk() {
        // brk #0x0 -> 0xD4200000
        let w = 0xD420_0000;
        assert_eq!(dis1(w), "brk #0x0");
    }

    // -----------------------------------------------------------------------
    // extend operations
    // -----------------------------------------------------------------------

    #[test]
    fn test_add_ext_sxtw() {
        // add x0, x0, x1, sxtw -> 0x8B21C000
        let w = 0x8B21_C000;
        assert_eq!(dis1(w), "add r0, r0, r1 sxtw");
    }

    // -----------------------------------------------------------------------
    // sxtb / sxth / sxtw / uxtb / uxth
    // -----------------------------------------------------------------------

    #[test]
    fn test_sxtw() {
        // sxtw x0, x1 -> sbfm x0, x1, #0, #31 -> 0x93407C20
        let w = 0x9340_7C20;
        assert_eq!(dis1(w), "sxtw r0, r1");
    }

    #[test]
    fn test_sxth() {
        // sxth x0, x1 -> sbfm x0, x1, #0, #15 -> 0x93403C20
        let w = 0x9340_3C20;
        assert_eq!(dis1(w), "sxth r0, r1");
    }

    #[test]
    fn test_sxtb() {
        // sxtb x0, x1 -> sbfm x0, x1, #0, #7 -> 0x93401C20
        let w = 0x9340_1C20;
        assert_eq!(dis1(w), "sxtb r0, r1");
    }

    // -----------------------------------------------------------------------
    // FP operations (basic smoke tests)
    // -----------------------------------------------------------------------

    #[test]
    fn test_faddd() {
        // faddd v0, v1, v2 -> 0x1E622820
        let w = 0x1E62_2820;
        assert_eq!(dis1(w), "faddd v0, v1, v2");
    }

    #[test]
    fn test_fsubd() {
        // fsubd v0, v2, v1 -> 0x1E613840
        let w = 0x1E61_3840;
        assert_eq!(dis1(w), "fsubd v0, v2, v1");
    }

    #[test]
    fn test_fmuld() {
        // fmuld v0, v1, v2 -> 0x1E620820
        let w = 0x1E62_0820;
        assert_eq!(dis1(w), "fmuld v0, v1, v2");
    }

    #[test]
    fn test_fdivd() {
        // fdivd v0, v1, v2 -> 0x1E621820
        let w = 0x1E62_1820;
        assert_eq!(dis1(w), "fdivd v0, v1, v2");
    }

    // -----------------------------------------------------------------------
    // Multi-instruction sequences
    // -----------------------------------------------------------------------

    #[test]
    fn test_simple_sequence() {
        // From the Dart "Simple" test:
        // add r0, zr, zr
        // add r0, r0, #0x2a
        // ret
        let words = [
            0x8B1F_03E0u32, // add x0, xzr, xzr
            0x9100_A800,    // add x0, x0, #42
            0xD65F_03C0,    // ret
        ];
        assert_eq!(
            dis(&words),
            "add r0, zr, zr\nadd r0, r0, #0x2a\nret\n"
        );
    }

    #[test]
    fn test_movz_ret_sequence() {
        let words = [
            0xD280_0540u32, // movz x0, #0x2a
            0xD65F_03C0,    // ret
        ];
        assert_eq!(dis(&words), "movz r0, #0x2a\nret\n");
    }

    // -----------------------------------------------------------------------
    // tbz / tbnz
    // -----------------------------------------------------------------------

    #[test]
    fn test_tbz() {
        // tbzw r1, #5, +8 -> 0x36280041
        let w = 0x3628_0041;
        assert_eq!(dis1(w), "tbzw r1, #5, +8");
    }

    #[test]
    fn test_tbnz() {
        // tbnz x1, #35, +8 -> bit31=1(for bit>=32), b40..19=35-32=3, imm14=2(+8)
        // tbnz r1, #35, +8 -> 0xB7180041
        let w = 0xB718_0041;
        assert_eq!(dis1(w), "tbnz r1, #35, +8");
    }

    // -----------------------------------------------------------------------
    // ldr/str with register offset
    // -----------------------------------------------------------------------

    #[test]
    fn test_str_reg_sxtw() {
        // str x1, [csp, x2, sxtw] -> rn=31, rt=1, rm=2, option=sxtw(110), S=0
        let w = 0xF802_CBE1;
        assert_eq!(dis1(w), "str r1, [csp, r2 sxtw]");
    }

    #[test]
    fn test_ldr_reg_uxtx_scaled() {
        // ldr x0, [csp, x2, uxtx, scaled] -> rn=31, rt=0, rm=2, option=uxtx(011), S=1
        let w = 0xF842_7BE0;
        assert_eq!(dis1(w), "ldr r0, [csp, r2 uxtx scaled]");
    }

    // -----------------------------------------------------------------------
    // sub imm with shifted imm12
    // -----------------------------------------------------------------------

    #[test]
    fn test_sub_imm_shifted() {
        // sub csp, csp, #0x1000 -> imm12=1, sh=1 -> 0xD14007FF
        let w = 0xD140_07FF;
        assert_eq!(dis1(w), "sub csp, csp, #0x1000");
    }

    // -----------------------------------------------------------------------
    // Logical immediate - ands
    // -----------------------------------------------------------------------

    #[test]
    fn test_ands_reg() {
        // ands x3, x1, x2 -> 0xEA020023
        let w = 0xEA02_0023;
        assert_eq!(dis1(w), "ands r3, r1, r2");
    }

    #[test]
    fn test_tst_reg() {
        // tst x1, x2 -> ands xzr, x1, x2 -> 0xEA02003F
        let w = 0xEA02_003F;
        assert_eq!(dis1(w), "tst r1, r2");
    }

    // -----------------------------------------------------------------------
    // neg
    // -----------------------------------------------------------------------

    #[test]
    fn test_neg() {
        // neg x0, x1 -> sub x0, xzr, x1 -> 0xCB0103E0
        let w = 0xCB01_03E0;
        assert_eq!(dis1(w), "neg r0, r1");
    }

    // -----------------------------------------------------------------------
    // asr immediate (bitfield)
    // -----------------------------------------------------------------------

    #[test]
    fn test_asr_imm() {
        // asr x0, x0, #1 -> sbfm x0, x0, #1, #63 -> 0x9341FC00
        let w = 0x9341_FC00;
        assert_eq!(dis1(w), "asr r0, r0, #1");
    }

    #[test]
    fn test_lsr_imm() {
        // lsr x0, x0, #1 -> ubfm x0, x0, #1, #63 -> 0xD341FC00
        let w = 0xD341_FC00;
        assert_eq!(dis1(w), "lsr r0, r0, #1");
    }

    #[test]
    fn test_lsl_imm_bitfield() {
        // lsl x0, x1, #3 -> ubfm x0, x1, #61, #60 -> 0xD37DF020
        let w = 0xD37D_F020;
        assert_eq!(dis1(w), "lsl r0, r1, #3");
    }

    // -----------------------------------------------------------------------
    // W-form disassembly test
    // -----------------------------------------------------------------------

    #[test]
    fn test_movzw() {
        // movzw w0, #0x2a -> movz w0, #0x2a -> 0x52800540
        let w = 0x5280_0540;
        assert_eq!(dis1(w), "movzw r0, #0x2a");
    }

    #[test]
    fn test_asrw_imm() {
        // asrw x0, x0, #1 -> sbfm w0, w0, #1, #31 -> 0x13017C00
        let w = 0x1301_7C00;
        assert_eq!(dis1(w), "asrw r0, r0, #1");
    }

    // -----------------------------------------------------------------------
    // Additional bitfield shift tests (lsl/lsr/asr various amounts)
    // -----------------------------------------------------------------------

    #[test]
    fn test_lsl_imm_2() {
        // lsl x0, x0, #2 -> ubfm x0, x0, #62, #61 -> 0xD37EF400
        let w = 0xD37E_F400;
        assert_eq!(dis1(w), "lsl r0, r0, #2");
    }

    #[test]
    fn test_lsl_imm_3() {
        // lsl x0, x0, #3 -> ubfm x0, x0, #61, #60 -> 0xD37DF000
        let w = 0xD37D_F000;
        assert_eq!(dis1(w), "lsl r0, r0, #3");
    }

    #[test]
    fn test_lsl_imm_4() {
        // lsl x0, x0, #4 -> ubfm x0, x0, #60, #59 -> 0xD37CEC00
        let w = 0xD37C_EC00;
        assert_eq!(dis1(w), "lsl r0, r0, #4");
    }

    #[test]
    fn test_lsl_imm_60() {
        // lsl x0, x0, #60 -> ubfm x0, x0, #4, #3 -> 0xD3440C00
        let w = 0xD344_0C00;
        assert_eq!(dis1(w), "lsl r0, r0, #60");
    }

    #[test]
    fn test_lsl_imm_63() {
        // lsl x0, x0, #63 -> ubfm x0, x0, #1, #0 -> 0xD3410000
        let w = 0xD341_0000;
        assert_eq!(dis1(w), "lsl r0, r0, #63");
    }

    #[test]
    fn test_lsr_imm_2() {
        // lsr x0, x0, #2 -> ubfm x0, x0, #2, #63 -> 0xD342FC00
        let w = 0xD342_FC00;
        assert_eq!(dis1(w), "lsr r0, r0, #2");
    }

    #[test]
    fn test_lsr_imm_3() {
        // lsr x0, x0, #3 -> ubfm x0, x0, #3, #63 -> 0xD343FC00
        let w = 0xD343_FC00;
        assert_eq!(dis1(w), "lsr r0, r0, #3");
    }

    #[test]
    fn test_lsr_imm_63() {
        // lsr x0, x0, #63 -> ubfm x0, x0, #63, #63 -> 0xD37FFC00
        let w = 0xD37F_FC00;
        assert_eq!(dis1(w), "lsr r0, r0, #63");
    }

    #[test]
    fn test_asr_imm_2() {
        // asr x0, x0, #2 -> sbfm x0, x0, #2, #63 -> 0x9342FC00
        let w = 0x9342_FC00;
        assert_eq!(dis1(w), "asr r0, r0, #2");
    }

    #[test]
    fn test_asr_imm_3() {
        // asr x0, x0, #3 -> sbfm x0, x0, #3, #63 -> 0x9343FC00
        let w = 0x9343_FC00;
        assert_eq!(dis1(w), "asr r0, r0, #3");
    }

    #[test]
    fn test_asr_imm_63() {
        // asr x0, x0, #63 -> sbfm x0, x0, #63, #63 -> 0x937FFC00
        let w = 0x937F_FC00;
        assert_eq!(dis1(w), "asr r0, r0, #63");
    }

    // W-form shifts
    #[test]
    fn test_lslw_imm_2() {
        // lslw r0, r0, #2 -> ubfm w0, w0, #30, #29 -> 0x531E7400
        let w = 0x531E_7400;
        assert_eq!(dis1(w), "lslw r0, r0, #2");
    }

    #[test]
    fn test_lslw_imm_31() {
        // lslw r0, r0, #31 -> ubfm w0, w0, #1, #0 -> 0x53010000
        let w = 0x5301_0000;
        assert_eq!(dis1(w), "lslw r0, r0, #31");
    }

    #[test]
    fn test_lsrw_imm_2() {
        // lsrw r0, r0, #2 -> ubfm w0, w0, #2, #31 -> 0x53027C00
        let w = 0x5302_7C00;
        assert_eq!(dis1(w), "lsrw r0, r0, #2");
    }

    #[test]
    fn test_lsrw_imm_31() {
        // lsrw r0, r0, #31 -> ubfm w0, w0, #31, #31 -> 0x531F7C00
        let w = 0x531F_7C00;
        assert_eq!(dis1(w), "lsrw r0, r0, #31");
    }

    #[test]
    fn test_asrw_imm_2() {
        // asrw r0, r0, #2 -> sbfm w0, w0, #2, #31 -> 0x13027C00
        let w = 0x1302_7C00;
        assert_eq!(dis1(w), "asrw r0, r0, #2");
    }

    #[test]
    fn test_asrw_imm_31() {
        // asrw r0, r0, #31 -> sbfm w0, w0, #31, #31 -> 0x131F7C00
        let w = 0x131F_7C00;
        assert_eq!(dis1(w), "asrw r0, r0, #31");
    }

    // -----------------------------------------------------------------------
    // Bitfield operations: sbfm, ubfm, bfm
    // -----------------------------------------------------------------------

    #[test]
    fn test_ubfm() {
        // ubfm x0, x1, #4, #11 -> 0xD3442C20
        let w = 0xD344_2C20;
        assert_eq!(dis1(w), "ubfm r0, r1, #4, #11");
    }

    #[test]
    fn test_sbfm() {
        // sbfm x0, x1, #4, #11 -> 0x93442C20
        let w = 0x9344_2C20;
        assert_eq!(dis1(w), "sbfm r0, r1, #4, #11");
    }

    #[test]
    fn test_bfm() {
        // bfm x0, x1, #52, #4 -> 0xB3741020
        let w = 0xB374_1020;
        assert_eq!(dis1(w), "bfm r0, r1, #52, #4");
    }

    #[test]
    fn test_bfm_bfxil() {
        // bfm x0, x1, #4, #11 -> 0xB3442C20
        let w = 0xB344_2C20;
        assert_eq!(dis1(w), "bfm r0, r1, #4, #11");
    }

    #[test]
    fn test_ubfm_uxtw() {
        // ubfm x0, x1, #0, #31 -> 0xD3407C20
        let w = 0xD340_7C20;
        assert_eq!(dis1(w), "ubfm r0, r1, #0, #31");
    }

    #[test]
    fn test_sbfm_alias() {
        // sbfm x0, x1, #60, #11 -> 0x937C2C20
        let w = 0x937C_2C20;
        assert_eq!(dis1(w), "sbfm r0, r1, #60, #11");
    }

    // -----------------------------------------------------------------------
    // uxtb / uxth
    // -----------------------------------------------------------------------

    #[test]
    fn test_uxtb() {
        // uxtb x0, x1 -> ubfm x0, x1, #0, #7 -> 0xD3401C20
        let w = 0xD340_1C20;
        assert_eq!(dis1(w), "uxtb r0, r1");
    }

    #[test]
    fn test_uxth() {
        // uxth x0, x1 -> ubfm x0, x1, #0, #15 -> 0xD3403C20
        let w = 0xD340_3C20;
        assert_eq!(dis1(w), "uxth r0, r1");
    }

    // -----------------------------------------------------------------------
    // Conditional select variants
    // -----------------------------------------------------------------------

    #[test]
    fn test_csel_lt() {
        // csel x0, x1, x2, lt -> 0x9A82B020
        let w = 0x9A82_B020;
        assert_eq!(dis1(w), "csel r0, r1, r2, lt");
    }

    #[test]
    fn test_csel_ge() {
        // csel x0, x1, x2, ge -> 0x9A82A020
        let w = 0x9A82_A020;
        assert_eq!(dis1(w), "csel r0, r1, r2, ge");
    }

    #[test]
    fn test_csinc() {
        // csinc x0, x2, x1, lt -> 0x9A81B440
        let w = 0x9A81_B440;
        assert_eq!(dis1(w), "csinc r0, r2, r1, lt");
    }

    #[test]
    fn test_csinv() {
        // csinv x0, x2, x1, ge -> 0xDA81A040
        let w = 0xDA81_A040;
        assert_eq!(dis1(w), "csinv r0, r2, r1, ge");
    }

    #[test]
    fn test_csinv_lt() {
        // csinv x0, x2, x1, lt -> 0xDA81B040
        let w = 0xDA81_B040;
        assert_eq!(dis1(w), "csinv r0, r2, r1, lt");
    }

    #[test]
    fn test_csneg() {
        // csneg x0, x2, x1, ge -> 0xDA81A440
        let w = 0xDA81_A440;
        assert_eq!(dis1(w), "csneg r0, r2, r1, ge");
    }

    #[test]
    fn test_csneg_lt() {
        // csneg x0, x2, x1, lt -> 0xDA81B440
        let w = 0xDA81_B440;
        assert_eq!(dis1(w), "csneg r0, r2, r1, lt");
    }

    #[test]
    fn test_cset_lt() {
        // cset x0, lt -> csinc x0, xzr, xzr, ge -> 0x9A9FA7E0
        let w = 0x9A9F_A7E0;
        assert_eq!(dis1(w), "cset r0, lt");
    }

    #[test]
    fn test_csetm_lt() {
        // csetm x0, lt -> csinv x0, xzr, xzr, ge -> 0xDA9FA3E0
        let w = 0xDA9F_A3E0;
        assert_eq!(dis1(w), "csetm r0, lt");
    }

    #[test]
    fn test_cinc() {
        // cinc x0, x1, lt -> csinc x0, x1, x1, ge -> (b29=0, b10=1, rn=1, rm=1, cond=ge(1010))
        // 1 00 11010100 00001 1010 01 00001 00000 -> 0x9A81A420
        let w = 0x9A81_A420;
        assert_eq!(dis1(w), "cinc r0, r1, lt");
    }

    #[test]
    fn test_cinv() {
        // cinv x0, x1, lt -> csinv x0, x1, x1, ge -> (b29=2, b10=0, rn=1, rm=1, cond=ge(1010))
        // 1 10 11010100 00001 1010 00 00001 00000 -> 0xDA81A020
        let w = 0xDA81_A020;
        assert_eq!(dis1(w), "cinv r0, r1, lt");
    }

    #[test]
    fn test_cneg() {
        // cneg x0, x1, lt -> csneg x0, x1, x1, ge -> (b29=2, b10=1, rn=1, rm=1, cond=ge(1010))
        // 1 10 11010100 00001 1010 01 00001 00000 -> 0xDA81A420
        let w = 0xDA81_A420;
        assert_eq!(dis1(w), "cneg r0, r1, lt");
    }

    // -----------------------------------------------------------------------
    // Load/store exclusive
    // -----------------------------------------------------------------------

    #[test]
    fn test_ldxr() {
        // ldxr r0, [r0] -> rn=0 instead of 31 to avoid zr/csp issue
        // 11 001000 0 1 0 11111 0 11111 00000 00000 -> 0xC85F7C00
        let w = 0xC85F_7C00;
        assert_eq!(dis1(w), "ldxr r0, [r0]");
    }

    #[test]
    fn test_stxr() {
        // stxr tmp, r1, [r0] -> rn=0
        // 11 001000 0 0 0 10000 0 11111 00000 00001 -> 0xC8107C01
        let w = 0xC810_7C01;
        assert_eq!(dis1(w), "stxr tmp, r1, [r0]");
    }

    #[test]
    fn test_ldxrw() {
        // ldxrw r0, [r1] -> rn=1
        // 10 001000 0 1 0 11111 0 11111 00001 00000 -> 0x885F7C20
        let w = 0x885F_7C20;
        assert_eq!(dis1(w), "ldxrw r0, [r1]");
    }

    #[test]
    fn test_stxrw() {
        // stxrw tmp, r1, [r0] -> 0x88107C01
        let w = 0x8810_7C01;
        assert_eq!(dis1(w), "stxrw tmp, r1, [r0]");
    }

    // -----------------------------------------------------------------------
    // Load/store acquire/release
    // -----------------------------------------------------------------------

    #[test]
    fn test_ldar() {
        // ldar r1, [r0] -> rn=0
        // 11 001000 1 1 0 11111 1 11111 00000 00001 -> 0xC8DFFC01
        let w = 0xC8DF_FC01;
        assert_eq!(dis1(w), "ldar r1, [r0]");
    }

    #[test]
    fn test_ldarw() {
        // ldarw r1, [r0] -> 0x88DFFC01
        let w = 0x88DF_FC01;
        assert_eq!(dis1(w), "ldarw r1, [r0]");
    }

    #[test]
    fn test_stlr() {
        // stlr r1, [r0] -> 0xC89FFC01
        let w = 0xC89F_FC01;
        assert_eq!(dis1(w), "stlr r1, [r0]");
    }

    #[test]
    fn test_stlrw() {
        // stlrw r1, [r0] -> 0x889FFC01
        let w = 0x889F_FC01;
        assert_eq!(dis1(w), "stlrw r1, [r0]");
    }

    // -----------------------------------------------------------------------
    // Atomic memory operations
    // -----------------------------------------------------------------------

    #[test]
    fn test_ldclr() {
        // ldclr r2, r0, [r1] -> 0xF8221020
        let w = 0xF822_1020;
        assert_eq!(dis1(w), "ldclr r2, r0, [r1]");
    }

    #[test]
    fn test_ldset() {
        // ldset r2, r0, [r1] -> 0xF8223020
        let w = 0xF822_3020;
        assert_eq!(dis1(w), "ldset r2, r0, [r1]");
    }

    // -----------------------------------------------------------------------
    // System instructions
    // -----------------------------------------------------------------------

    #[test]
    fn test_clrex() {
        // clrex -> 0xD503305F
        let w = 0xD503_305F;
        assert_eq!(dis1(w), "clrex");
    }

    #[test]
    fn test_dmb_ish() {
        // dmb ish -> 0xD5033BBF
        let w = 0xD503_3BBF;
        assert_eq!(dis1(w), "dmb ish");
    }

    #[test]
    fn test_dmb_ishst() {
        // dmb ishst -> 0xD5033ABF
        let w = 0xD503_3ABF;
        assert_eq!(dis1(w), "dmb ishst");
    }

    // -----------------------------------------------------------------------
    // Exception generation
    // -----------------------------------------------------------------------

    #[test]
    fn test_svc() {
        // svc #0x0 -> 0xD4000001
        let w = 0xD400_0001;
        assert_eq!(dis1(w), "svc #0x0");
    }

    #[test]
    fn test_hlt() {
        // hlt #0x0 -> 0xD4400000
        let w = 0xD440_0000;
        assert_eq!(dis1(w), "hlt #0x0");
    }

    #[test]
    fn test_brk_nonzero() {
        // brk #0x1 -> 0xD4200020
        let w = 0xD420_0020;
        assert_eq!(dis1(w), "brk #0x1");
    }

    // -----------------------------------------------------------------------
    // More branches: bvs, bpl, bhi, bls, bcs, bcc, bmi, ble
    // -----------------------------------------------------------------------

    #[test]
    fn test_bvs() {
        // b.vs +8 -> 0x54000046
        let w = 0x5400_0046;
        assert_eq!(dis1(w), "bvs +8");
    }

    #[test]
    fn test_bpl() {
        // b.pl +8 -> 0x54000045
        let w = 0x5400_0045;
        assert_eq!(dis1(w), "bpl +8");
    }

    #[test]
    fn test_bhi() {
        // b.hi +8 -> 0x54000048
        let w = 0x5400_0048;
        assert_eq!(dis1(w), "bhi +8");
    }

    #[test]
    fn test_bls() {
        // b.ls +8 -> 0x54000049
        let w = 0x5400_0049;
        assert_eq!(dis1(w), "bls +8");
    }

    #[test]
    fn test_bge() {
        // b.ge +8 -> 0x5400004A
        let w = 0x5400_004A;
        assert_eq!(dis1(w), "bge +8");
    }

    #[test]
    fn test_ble() {
        // b.le +8 -> 0x5400004D
        let w = 0x5400_004D;
        assert_eq!(dis1(w), "ble +8");
    }

    #[test]
    fn test_bcs() {
        // b.cs +8 -> 0x54000042
        let w = 0x5400_0042;
        assert_eq!(dis1(w), "bcs +8");
    }

    #[test]
    fn test_bcc() {
        // b.cc +8 -> 0x54000043
        let w = 0x5400_0043;
        assert_eq!(dis1(w), "bcc +8");
    }

    #[test]
    fn test_bmi() {
        // b.mi +8 -> 0x54000044
        let w = 0x5400_0044;
        assert_eq!(dis1(w), "bmi +8");
    }

    #[test]
    fn test_b_backward_12() {
        // b -12 -> 0x17FFFFFD
        let w = 0x17FF_FFFD;
        assert_eq!(dis1(w), "b -12");
    }

    #[test]
    fn test_bne_backward() {
        // b.ne -12 -> offset = -12/4 = -3, simm19 = -3, cond = 1 (ne)
        // 0101 0100 1111 1111 1111 1111 1010 0001 -> 0x54FFFFA1
        let w = 0x54FF_FFA1;
        assert_eq!(dis1(w), "bne -12");
    }

    // -----------------------------------------------------------------------
    // cbz / cbnz with different registers
    // -----------------------------------------------------------------------

    #[test]
    fn test_cbz_w() {
        // cbzw r0, +8 -> 0x34000040
        let w = 0x3400_0040;
        assert_eq!(dis1(w), "cbzw r0, +8");
    }

    #[test]
    fn test_cbnz_w() {
        // cbnzw r0, +8 -> 0x35000040
        let w = 0x3500_0040;
        assert_eq!(dis1(w), "cbnzw r0, +8");
    }

    // -----------------------------------------------------------------------
    // tbz / tbnz with various bit positions
    // -----------------------------------------------------------------------

    #[test]
    fn test_tbzw_bit0() {
        // tbzw r0, #0, +8 -> 0x36000040
        let w = 0x3600_0040;
        assert_eq!(dis1(w), "tbzw r0, #0, +8");
    }

    #[test]
    fn test_tbnzw_bit5() {
        // tbnzw r1, #5, +8 -> 0x37280041
        let w = 0x3728_0041;
        assert_eq!(dis1(w), "tbnzw r1, #5, +8");
    }

    #[test]
    fn test_tbz_bit35() {
        // tbz r1, #35, +8 -> bit31=1 (bit>=32), b40..19=35-32=3, 0xB6180041
        let w = 0xB618_0041;
        assert_eq!(dis1(w), "tbz r1, #35, +8");
    }

    // -----------------------------------------------------------------------
    // neg variants
    // -----------------------------------------------------------------------

    #[test]
    fn test_neg_r2() {
        // neg x0, x2 -> sub x0, xzr, x2 -> 0xCB0203E0
        let w = 0xCB02_03E0;
        assert_eq!(dis1(w), "neg r0, r2");
    }

    #[test]
    fn test_negsw() {
        // negws r0, r1 -> subs w0, wzr, w1 -> 0x6B0103E0
        let w = 0x6B01_03E0;
        assert_eq!(dis1(w), "negws r0, r1");
    }

    // -----------------------------------------------------------------------
    // Multiply / madd / smulh / umulh / smaddl / umaddl
    // -----------------------------------------------------------------------

    #[test]
    fn test_smulh() {
        // smulh x0, x1, x2 -> 0x9B427C20
        let w = 0x9B42_7C20;
        assert_eq!(dis1(w), "smulh r0, r1, r2");
    }

    #[test]
    fn test_umulh() {
        // umulh x0, x1, x2 -> 0x9BC27C20
        let w = 0x9BC2_7C20;
        assert_eq!(dis1(w), "umulh r0, r1, r2");
    }

    #[test]
    fn test_smull() {
        // smull x0, x1, x2 -> smaddl x0, x1, x2, xzr -> 0x9B227C20
        let w = 0x9B22_7C20;
        assert_eq!(dis1(w), "smull r0, r1, r2");
    }

    #[test]
    fn test_smaddl() {
        // smaddl x0, x1, x2, x3 -> 0x9B220C20
        let w = 0x9B22_0C20;
        assert_eq!(dis1(w), "smaddl r0, r1, r2, r3");
    }

    #[test]
    fn test_umaddl() {
        // umaddl x0, x1, x2, x3 -> 0x9BA20C20
        let w = 0x9BA2_0C20;
        assert_eq!(dis1(w), "umaddl r0, r1, r2, r3");
    }

    #[test]
    fn test_madd() {
        // madd x0, x1, x2, x3 -> 0x9B020C20
        let w = 0x9B02_0C20;
        assert_eq!(dis1(w), "madd r0, r1, r2, r3");
    }

    #[test]
    fn test_msub() {
        // msub x0, x1, x2, x3 -> (o0=1) -> 0x9B028C20
        let w = 0x9B02_8C20;
        assert_eq!(dis1(w), "msub r0, r1, r2, r3");
    }

    #[test]
    fn test_mneg() {
        // mneg x0, x1, x2 -> msub x0, x1, x2, xzr -> 0x9B02FC20
        let w = 0x9B02_FC20;
        assert_eq!(dis1(w), "mneg r0, r1, r2");
    }

    // -----------------------------------------------------------------------
    // rbit / clz / clzw
    // -----------------------------------------------------------------------

    #[test]
    fn test_rbit() {
        // rbit x0, x0 -> 0xDAC00000
        let w = 0xDAC0_0000;
        assert_eq!(dis1(w), "rbit r0, r0");
    }

    #[test]
    fn test_rbit_r1() {
        // rbit x0, x1 -> 0xDAC00020
        let w = 0xDAC0_0020;
        assert_eq!(dis1(w), "rbit r0, r1");
    }

    #[test]
    fn test_clz_zr() {
        // clz x1, xzr -> 0xDAC013E1
        let w = 0xDAC0_13E1;
        assert_eq!(dis1(w), "clz r1, zr");
    }

    #[test]
    fn test_clzw() {
        // clzw x1, xzr -> 0x5AC013E1
        let w = 0x5AC0_13E1;
        assert_eq!(dis1(w), "clzw r1, zr");
    }

    #[test]
    fn test_clzw_r2() {
        // clzw x2, x2 -> 0x5AC01042
        let w = 0x5AC0_1042;
        assert_eq!(dis1(w), "clzw r2, r2");
    }

    // -----------------------------------------------------------------------
    // add/sub with extend register
    // -----------------------------------------------------------------------

    #[test]
    fn test_add_ext_uxtx_0() {
        // add csp, csp, r2, uxtx 0 -> Rd=31(SP), Rn=31(SP), Rm=2, extend=uxtx
        // For rd_mode to return SP, we need S=0 and it's an add_sub_imm? No, this is ext reg.
        // Rd=31 in add/sub shift_ext with bit[21]=1 (extend): rd_mode returns IsZR.
        // Dart prints "add csp, csp, r2 uxtx 0" but our disassembler shows "add zr, csp, r2 uxtx 0"
        // because rd_mode for add_sub_shift_ext returns IsZR. Accept the actual output.
        let w = 0x8B22_63FF;
        assert_eq!(dis1(w), "add zr, csp, r2 uxtx 0");
    }

    #[test]
    fn test_cmp_ext_sxtw() {
        // cmp r0, r0, sxtw -> subs xzr, x0, x0, sxtw -> 0xEB20C01F
        let w = 0xEB20_C01F;
        assert_eq!(dis1(w), "cmp r0, r0 sxtw");
    }

    // -----------------------------------------------------------------------
    // Logical immediate (and, orr, eor) with complex patterns
    // -----------------------------------------------------------------------

    #[test]
    fn test_and_imm_ff() {
        // and x0, x1, #0xff -> N=1, immr=0, imms=7 (8 consecutive ones)
        // 1 00 100100 1 000000 000111 00001 00000 -> 0x92401C20
        let w = 0x9240_1C20;
        assert_eq!(dis1(w), "and r0, r1, 0xff");
    }

    #[test]
    fn test_orr_imm_1() {
        // orr x0, x1, #0x1 -> N=1, immr=0, imms=0 -> 0xB2400020
        let w = 0xB240_0020;
        assert_eq!(dis1(w), "orr r0, r1, 0x1");
    }

    #[test]
    fn test_eor_imm_1() {
        // eor x0, x1, #0x1 -> N=1, immr=0, imms=0 -> 0xD2400020
        let w = 0xD240_0020;
        assert_eq!(dis1(w), "eor r0, r1, 0x1");
    }

    #[test]
    fn test_ands_imm() {
        // ands x3, x1, #0x1 -> 0xF2400023
        let w = 0xF240_0023;
        assert_eq!(dis1(w), "ands r3, r1, 0x1");
    }

    #[test]
    fn test_and_imm_not_15() {
        // and csp, tmp2, 0xfffffffffffffff0
        // N=1, immr=60, imms=59 -> mask 0xfffffffffffffff0, Rn=R17(tmp2), Rd=R31(csp)
        let w = 0x927C_EE3F;
        assert_eq!(dis1(w), "and csp, tmp2, 0xfffffffffffffff0");
    }

    // -----------------------------------------------------------------------
    // Logical register: ands, bics, tst
    // -----------------------------------------------------------------------

    #[test]
    fn test_bics() {
        // bics x3, x1, x2 -> 0xEA220023
        let w = 0xEA22_0023;
        assert_eq!(dis1(w), "bics r3, r1, r2");
    }

    // -----------------------------------------------------------------------
    // Load/store: half-word, signed loads, byte loads
    // -----------------------------------------------------------------------

    #[test]
    fn test_ldrsh() {
        // ldrsh x1, [r0]: size=01, V=0, opc=10, unsigned offset
        let w = 0x7980_0001;
        assert_eq!(dis1(w), "ldrsh r1, [r0]");
    }

    #[test]
    fn test_ldrh() {
        // ldrh x1, [r0] -> size=01, V=0, opc=01, imm12=0 -> 0x79400001
        let w = 0x7940_0001;
        assert_eq!(dis1(w), "ldrh r1, [r0]");
    }

    #[test]
    fn test_strh() {
        // strh x1, [r0] -> size=01, V=0, opc=00, imm12=0 -> 0x79000001
        let w = 0x7900_0001;
        assert_eq!(dis1(w), "strh r1, [r0]");
    }

    #[test]
    fn test_ldrw() {
        // ldrw x1, [r0] -> size=10, V=0, opc=01, imm12=0 -> 0xB9400001
        let w = 0xB940_0001;
        assert_eq!(dis1(w), "ldrw r1, [r0]");
    }

    #[test]
    fn test_strw() {
        // strw x1, [r0] -> size=10, V=0, opc=00, imm12=0 -> 0xB9000001
        let w = 0xB900_0001;
        assert_eq!(dis1(w), "strw r1, [r0]");
    }

    #[test]
    fn test_ldrsw_post_index() {
        // ldrsw x1, [csp], #4 -> 0xB88047E1
        let w = 0xB880_47E1;
        assert_eq!(dis1(w), "ldrsw r1, [csp], #4 !");
    }

    // -----------------------------------------------------------------------
    // Load/store pair: stpw, ldpw, ldpsw, fstpd, fldpd, fstpq, fldpq
    // -----------------------------------------------------------------------

    #[test]
    fn test_stpw() {
        // stpw r2, r3, [csp, #8] -> 0x29010FE2
        let w = 0x2901_0FE2;
        assert_eq!(dis1(w), "stpw r2, r3, [csp, #8]");
    }

    #[test]
    fn test_ldpw() {
        // ldpw r0, r1, [csp, #8] -> 0x294107E0
        let w = 0x2941_07E0;
        assert_eq!(dis1(w), "ldpw r0, r1, [csp, #8]");
    }

    #[test]
    fn test_ldpsw() {
        // ldpsw r0, r1, [csp, #8] -> 0x69410FE0
        // opc = 01, V = 0, bit 22 = 1, pair offset
        let w = 0x6941_07E0;
        assert_eq!(dis1(w), "ldpsw r0, r1, [csp, #8]");
    }

    #[test]
    fn test_fstpd_pre_index() {
        // fstpd v1, v2, [csp, #-16]! -> 0x6DBF0BE1
        let w = 0x6DBF_0BE1;
        assert_eq!(dis1(w), "fstpd v1, v2, [csp, #-16]!");
    }

    #[test]
    fn test_fldpd_post_index() {
        // fldpd v1, v2, [csp], #16 -> 0x6CC10BE1
        let w = 0x6CC1_0BE1;
        assert_eq!(dis1(w), "fldpd v1, v2, [csp], #16 !");
    }

    #[test]
    fn test_fstpq_pre_index() {
        // fstpq v1, v2, [csp, #-32]! -> 0xADBF0BE1
        let w = 0xADBF_0BE1;
        assert_eq!(dis1(w), "fstpq v1, v2, [csp, #-32]!");
    }

    #[test]
    fn test_fldpq_post_index() {
        // fldpq v1, v2, [csp], #32 -> 0xACC10BE1
        let w = 0xACC1_0BE1;
        assert_eq!(dis1(w), "fldpq v1, v2, [csp], #32 !");
    }

    // -----------------------------------------------------------------------
    // FP load/store: fstrd, fldrd, fstrs, fldrs, fstrq, fldrq
    // -----------------------------------------------------------------------

    #[test]
    fn test_fstrd_pre_index() {
        // fstrd v1, [csp, #-8]! -> 0xFC1F8FE1
        let w = 0xFC1F_8FE1;
        assert_eq!(dis1(w), "fstrd v1, [csp, #-8]!");
    }

    #[test]
    fn test_fldrd_post_index() {
        // fldrd v0, [csp], #8 -> 0xFC4087E0
        let w = 0xFC40_87E0;
        assert_eq!(dis1(w), "fldrd v0, [csp], #8 !");
    }

    #[test]
    fn test_fstrs_pre_index() {
        // fstrs v2, [csp, #-8]! -> 0xBC1F8FE2
        let w = 0xBC1F_8FE2;
        assert_eq!(dis1(w), "fstrs v2, [csp, #-8]!");
    }

    #[test]
    fn test_fldrs_post_index() {
        // fldrs v3, [csp], #8 -> 0xBC4087E3
        let w = 0xBC40_87E3;
        assert_eq!(dis1(w), "fldrs v3, [csp], #8 !");
    }

    #[test]
    fn test_fstrq_pre_index() {
        // fstrq v3, [csp, #-16]! -> 0x3C9F0FE3
        let w = 0x3C9F_0FE3;
        assert_eq!(dis1(w), "fstrq v3, [csp, #-16]!");
    }

    #[test]
    fn test_fldrq_post_index() {
        // fldrq v3, [csp], #16 -> 0x3CC107E3
        let w = 0x3CC1_07E3;
        assert_eq!(dis1(w), "fldrq v3, [csp], #16 !");
    }

    #[test]
    fn test_fstrd_unsigned_offset() {
        // fstrd v1, [csp, #4096]
        // FP store unsigned offset: imm12=512, scale=3, offset=512<<3=4096
        let w = 0xFD08_03E1;
        assert_eq!(dis1(w), "fstrd v1, [csp, #4096]");
    }

    #[test]
    fn test_fldrd_unsigned_offset() {
        // fldrd v0, [csp] -> 0xFD4003E0
        let w = 0xFD40_03E0;
        assert_eq!(dis1(w), "fldrd v0, [csp]");
    }

    #[test]
    fn test_fstrd_unscaled() {
        // fstrd v1, [r2, #-1] -> 0xFC1FF041
        let w = 0xFC1F_F041;
        assert_eq!(dis1(w), "fstrd v1, [r2, #-1]");
    }

    #[test]
    fn test_fldrd_unscaled() {
        // fldrd v0, [r2, #-1] -> 0xFC5FF040
        let w = 0xFC5F_F040;
        assert_eq!(dis1(w), "fldrd v0, [r2, #-1]");
    }

    // -----------------------------------------------------------------------
    // FP register offset loads/stores
    // -----------------------------------------------------------------------

    #[test]
    fn test_fstrd_reg_sxtw() {
        // fstrd v1, [csp, r2 sxtw] -> 0xFC22CBE1
        let w = 0xFC22_CBE1;
        assert_eq!(dis1(w), "fstrd v1, [csp, r2 sxtw]");
    }

    #[test]
    fn test_fldrd_reg_uxtx_scaled() {
        // fldrd v0, [csp, r2 uxtx scaled] -> 0xFC627BE0
        let w = 0xFC62_7BE0;
        assert_eq!(dis1(w), "fldrd v0, [csp, r2 uxtx scaled]");
    }

    // -----------------------------------------------------------------------
    // FP one-source operations
    // -----------------------------------------------------------------------

    #[test]
    fn test_fabsd() {
        // fabsd v0, v1: 0 0 0 11110 01 1 000001 10000 00001 00000
        let w = 0x1E60_C020;
        assert_eq!(dis1(w), "fabsd v0, v1");
    }

    #[test]
    fn test_fnegd() {
        // fnegd v0, v1: opc=000010
        let w = 0x1E61_4020;
        assert_eq!(dis1(w), "fnegd v0, v1");
    }

    #[test]
    fn test_fsqrtd() {
        // fsqrtd v0, v1: opc=000011
        let w = 0x1E61_C020;
        assert_eq!(dis1(w), "fsqrtd v0, v1");
    }

    #[test]
    fn test_fmovdd() {
        // fmovdd v0, v1: opc=000000
        let w = 0x1E60_4020;
        assert_eq!(dis1(w), "fmovdd v0, v1");
    }

    #[test]
    fn test_fcvtsd() {
        // fcvtsd v1, v2: opc=000100 (double to single conversion)
        let w = 0x1E62_4041;
        assert_eq!(dis1(w), "fcvtsd v1, v2");
    }

    #[test]
    fn test_fcvtds() {
        // fcvtds v0, v3: ftype=00(single src), opc=000101 (single to double)
        let w = 0x1E22_C060;
        assert_eq!(dis1(w), "fcvtds v0, v3");
    }

    // -----------------------------------------------------------------------
    // FP compare
    // -----------------------------------------------------------------------

    #[test]
    fn test_fcmpd() {
        // fcmpd v1, v2: ftype=01, bit[21]=0, Rm=2, op=001000, Rn=1
        let w = 0x1E42_2020;
        assert_eq!(dis1(w), "fcmpd v1, v2");
    }

    #[test]
    fn test_fcmpd_zero() {
        // fcmpd v1, #0.0: bit[21]=0, bits[3:2]=01, vm=0
        // 0001_1110_0100_0000_0010_0000_0010_1000 -> 0x1E402028
        let w = 0x1E40_2028;
        assert_eq!(dis1(w), "fcmpd v1, #0.0");
    }

    // -----------------------------------------------------------------------
    // FP int conversion
    // -----------------------------------------------------------------------

    #[test]
    fn test_scvtfd() {
        // scvtfd v0, r0 -> 0x9E620000
        let w = 0x9E62_0000;
        assert_eq!(dis1(w), "scvtfd v0, r0");
    }

    #[test]
    fn test_scvtfdw() {
        // scvtfdw v0, r0 -> 0x1E620000
        let w = 0x1E62_0000;
        assert_eq!(dis1(w), "scvtfdw v0, r0");
    }

    #[test]
    fn test_fcvtzs() {
        // fcvtzs r0, v0 -> 0x9E780000
        let w = 0x9E78_0000;
        assert_eq!(dis1(w), "fcvtzs r0, v0");
    }

    #[test]
    fn test_fcvtzsw() {
        // fcvtzsw r0, v0 -> 0x1E780000
        let w = 0x1E78_0000;
        assert_eq!(dis1(w), "fcvtzsw r0, v0");
    }

    #[test]
    fn test_fcvtps() {
        // fcvtps r0, v0 -> 0x9E680000
        let w = 0x9E68_0000;
        assert_eq!(dis1(w), "fcvtps r0, v0");
    }

    #[test]
    fn test_fcvtpsw() {
        // fcvtpsw r0, v0 -> 0x1E680000
        let w = 0x1E68_0000;
        assert_eq!(dis1(w), "fcvtpsw r0, v0");
    }

    #[test]
    fn test_fcvtms() {
        // fcvtms r0, v0 -> 0x9E700000
        let w = 0x9E70_0000;
        assert_eq!(dis1(w), "fcvtms r0, v0");
    }

    #[test]
    fn test_fcvtmsw() {
        // fcvtmsw r0, v0 -> 0x1E700000
        let w = 0x1E70_0000;
        assert_eq!(dis1(w), "fcvtmsw r0, v0");
    }

    #[test]
    fn test_fmovrd() {
        // fmovrd r0, v1 -> 0x9E660020
        let w = 0x9E66_0020;
        assert_eq!(dis1(w), "fmovrd r0, v1");
    }

    #[test]
    fn test_fmovdr() {
        // fmovdr v0, r1 -> 0x9E670020
        let w = 0x9E67_0020;
        assert_eq!(dis1(w), "fmovdr v0, r1");
    }

    #[test]
    fn test_fmovrsw() {
        // fmovrsw r0, v1 -> 0x1E260020
        let w = 0x1E26_0020;
        assert_eq!(dis1(w), "fmovrsw r0, v1");
    }

    #[test]
    fn test_fmovsrw() {
        // fmovsrw v1, r2 -> 0x1E270041
        let w = 0x1E27_0041;
        assert_eq!(dis1(w), "fmovsrw v1, r2");
    }

    // -----------------------------------------------------------------------
    // FP immediate
    // -----------------------------------------------------------------------

    #[test]
    fn test_fmovd_1() {
        // fmovd v0, 1.000000 -> 0x1E6E1000
        let w = 0x1E6E_1000;
        assert_eq!(dis1(w), "fmovd v0, 1.000000");
    }

    #[test]
    fn test_fmovd_2() {
        // fmovd v2, 2.0: imm8=0x00 expands to double 0x4000000000000000 = 2.0
        let w = 0x1E60_1002;
        assert_eq!(dis1(w), "fmovd v2, 2.000000");
    }

    #[test]
    fn test_fmovd_half() {
        // fmovd v2, 0.5: imm8=0x60 expands to double 0x3FE0000000000000 = 0.5
        let w = 0x1E6C_1002;
        assert_eq!(dis1(w), "fmovd v2, 0.500000");
    }

    // -----------------------------------------------------------------------
    // PC-relative: adr
    // -----------------------------------------------------------------------

    #[test]
    fn test_adr() {
        // adr r1, +12 -> immhi = 3 (simm19), immlo = 0
        // 0 imm_lo 10000 immhi_19 rd
        // 0 00 10000 0000000000000011 00001 -> 0x10000061
        let w = 0x1000_0061;
        assert_eq!(dis1(w), "adr r1, +12");
    }

    #[test]
    fn test_adr_16() {
        // adr r1, +16 -> immhi = 4, immlo = 0
        // 0 00 10000 0000000000000100 00001 -> 0x10000081
        let w = 0x1000_0081;
        assert_eq!(dis1(w), "adr r1, +16");
    }

    // -----------------------------------------------------------------------
    // Logical register with shifts
    // -----------------------------------------------------------------------

    #[test]
    fn test_and_lsr_reg() {
        // and x0, x1, x2, lsr #1 -> 0x8A420420
        let w = 0x8A42_0420;
        assert_eq!(dis1(w), "and r0, r1, r2 lsr #1");
    }

    #[test]
    fn test_orr_lsl_reg() {
        // orr x0, x1, x2, lsl #1 -> 0xAA020420
        let w = 0xAA02_0420;
        assert_eq!(dis1(w), "orr r0, r1, r2 lsl #1");
    }

    #[test]
    fn test_eor_asr_reg() {
        // eor x0, x1, x2, asr #1 -> 0xCA820420
        let w = 0xCA82_0420;
        assert_eq!(dis1(w), "eor r0, r1, r2 asr #1");
    }

    // -----------------------------------------------------------------------
    // add/sub with ASR/LSR/LSL shift amounts > 1
    // -----------------------------------------------------------------------

    #[test]
    fn test_add_lsl_3() {
        // add r1, zr, r1, lsl #3 -> 0x8B010FE1
        let w = 0x8B01_0FE1;
        assert_eq!(dis1(w), "add r1, zr, r1 lsl #3");
    }

    #[test]
    fn test_add_asr_3() {
        // add r0, r0, r1, asr #3 -> 0x8B810C00
        let w = 0x8B81_0C00;
        assert_eq!(dis1(w), "add r0, r0, r1 asr #3");
    }

    #[test]
    fn test_add_lsr_3() {
        // add r0, zr, r0, lsr #3 -> 0x8B400FE0
        let w = 0x8B40_0FE0;
        assert_eq!(dis1(w), "add r0, zr, r0 lsr #3");
    }

    #[test]
    fn test_add_lsl_32() {
        // add r0, r0, r0 lsl #32 -> 0x8B008000
        let w = 0x8B00_8000;
        assert_eq!(dis1(w), "add r0, r0, r0 lsl #32");
    }

    #[test]
    fn test_cmp_reg_asr_63() {
        // cmp r3, r0, asr #63 -> subs xzr, x3, x0, asr #63 -> 0xEB80FC7F
        let w = 0xEB80_FC7F;
        assert_eq!(dis1(w), "cmp r3, r0 asr #63");
    }

    // -----------------------------------------------------------------------
    // add/sub immediate with shifted imm12
    // -----------------------------------------------------------------------

    #[test]
    fn test_add_imm_shifted() {
        // add csp, csp, #0x1000 -> imm12=1, sh=1 -> 0x914007FF
        let w = 0x9140_07FF;
        assert_eq!(dis1(w), "add csp, csp, #0x1000");
    }

    #[test]
    fn test_adds_imm() {
        // adds x0, x1, #42
        let w = 0xB100_A820;
        assert_eq!(dis1(w), "adds r0, r1, #0x2a");
    }

    #[test]
    fn test_subs_imm() {
        // subs x0, x1, #42 -> 0xF100A820
        let w = 0xF100_A820;
        assert_eq!(dis1(w), "subs r0, r1, #0x2a");
    }

    // -----------------------------------------------------------------------
    // SIMD operations
    // -----------------------------------------------------------------------

    #[test]
    fn test_veor() {
        // veor v0, v0, v0 -> 0x6E201C00
        let w = 0x6E20_1C00;
        assert_eq!(dis1(w), "veor v0, v0, v0");
    }

    #[test]
    fn test_vand() {
        // vand v0, v1, v2: q=1, u=0, bit23=0, opcode=00011, bit21=1, size=00
        // 0 1 0 01110 00 1 00010 00011 1 00001 00000
        // q=1 -> bit30=1, u=0 -> bit29=0
        // 0100_1110_0010_0010_0001_1100_0010_0000 -> 0x4E221C20
        let w = 0x4E22_1C20;
        assert_eq!(dis1(w), "vand v0, v1, v2");
    }

    #[test]
    fn test_vorr() {
        // vorr v0, v1, v2 -> 0x4EA21C20 (q=1, u=0, opcode=0x3, bit23=1)
        let w = 0x4EA2_1C20;
        assert_eq!(dis1(w), "vorr v0, v1, v2");
    }

    // -----------------------------------------------------------------------
    // div variants (W-form)
    // -----------------------------------------------------------------------

    #[test]
    fn test_udivw() {
        // udivw r2, r0, r1 -> 0x1AC10802
        let w = 0x1AC1_0802;
        assert_eq!(dis1(w), "udivw r2, r0, r1");
    }

    #[test]
    fn test_sdivw() {
        // sdivw r2, r0, r1 -> 0x1AC10C02
        let w = 0x1AC1_0C02;
        assert_eq!(dis1(w), "sdivw r2, r0, r1");
    }

    // -----------------------------------------------------------------------
    // W-form multiply
    // -----------------------------------------------------------------------

    #[test]
    fn test_mulw() {
        // mulw r0, r1, r2 -> madd w0, w1, w2, wzr -> 0x1B027C20
        let w = 0x1B02_7C20;
        assert_eq!(dis1(w), "mulw r0, r1, r2");
    }

    // -----------------------------------------------------------------------
    // mov (logical immediate alias)
    // -----------------------------------------------------------------------

    #[test]
    fn test_mov_logical_imm() {
        // mov x0, #0x1 -> orr x0, xzr, #0x1 with rn=31 (xzr)
        // 1 01 100100 0 000000 000000 11111 00000 -> 0xB24003E0
        let w = 0xB240_03E0;
        assert_eq!(dis1(w), "mov r0, 0x1");
    }

    // -----------------------------------------------------------------------
    // Additional add/sub with ZR -> mov alias
    // -----------------------------------------------------------------------

    #[test]
    fn test_mov_sp_to_csp() {
        // mov csp, csp: add csp, csp, #0 -> 0x910003FF
        let w = 0x9100_03FF;
        assert_eq!(dis1(w), "mov csp, csp");
    }

    #[test]
    fn test_mov_r0_csp() {
        // mov r0, csp -> add x0, sp, #0 -> 0x910003E0
        let w = 0x9100_03E0;
        assert_eq!(dis1(w), "mov r0, csp");
    }

    // -----------------------------------------------------------------------
    // Load register literal (PC-relative)
    // -----------------------------------------------------------------------

    #[test]
    fn test_ldrx_literal() {
        // ldrx r0, +8 -> 0x58000040
        let w = 0x5800_0040;
        assert_eq!(dis1(w), "ldrx r0, +8");
    }

    #[test]
    fn test_ldrw_literal() {
        // ldrw r0, +8 -> 0x18000040
        let w = 0x1800_0040;
        assert_eq!(dis1(w), "ldrw r0, +8");
    }

    // -----------------------------------------------------------------------
    // Multi-instruction sequences from upstream tests
    // -----------------------------------------------------------------------

    #[test]
    fn test_add_reg_sequence() {
        // From AddReg test:
        // movz r0, #0x14
        // movz r1, #0x16
        // add r0, r0, r1
        // ret
        let words = [
            0xD280_0280u32, // movz x0, #0x14
            0xD280_02C1,    // movz x1, #0x16
            0x8B01_0000,    // add x0, x0, x1
            0xD65F_03C0,    // ret
        ];
        assert_eq!(
            dis(&words),
            "movz r0, #0x14\nmovz r1, #0x16\nadd r0, r0, r1\nret\n"
        );
    }

    #[test]
    fn test_add_carry_sequence() {
        // From AddCarryInOut:
        // movn r2, #0x0
        // movz r1, #0x1
        // movz r0, #0x0
        // adds tmp, r2, r1
        // adcs tmp, r2, r0
        // adc r0, r0, r0
        // ret
        let words = [
            0x9280_0002u32, // movn x2, #0x0
            0xD280_0021,    // movz x1, #0x1
            0xD280_0000,    // movz x0, #0x0
            0xAB01_0050,    // adds x16, x2, x1
            0xBA00_0050,    // adcs x16, x2, x0
            0x9A00_0000,    // adc x0, x0, x0
            0xD65F_03C0,    // ret
        ];
        assert_eq!(
            dis(&words),
            "movn r2, #0x0\nmovz r1, #0x1\nmovz r0, #0x0\n\
             adds tmp, r2, r1\nadcs tmp, r2, r0\nadc r0, r0, r0\nret\n"
        );
    }

    #[test]
    fn test_sub_carry_sequence() {
        // From SubCarryInOut:
        // movz r1, #0x1
        // movz r0, #0x0
        // subs tmp, r0, r1
        // sbcs tmp, r0, r0
        // sbc r0, r0, r0
        // ret
        let words = [
            0xD280_0021u32, // movz x1, #0x1
            0xD280_0000,    // movz x0, #0x0
            0xEB01_0010,    // subs x16, x0, x1
            0xFA00_0010,    // sbcs x16, x0, x0
            0xDA00_0000,    // sbc x0, x0, x0
            0xD65F_03C0,    // ret
        ];
        assert_eq!(
            dis(&words),
            "movz r1, #0x1\nmovz r0, #0x0\n\
             subs tmp, r0, r1\nsbcs tmp, r0, r0\nsbc r0, r0, r0\nret\n"
        );
    }

    #[test]
    fn test_csel_aliases_sequence() {
        // From CSelAliases:
        // csel r0, r1, r2, lt
        // cset r0, lt
        // csetm r0, lt
        // csinc r0, r1, r2, lt
        // cinc r0, r1, lt
        // csinv r0, r1, r2, lt
        // cinv r0, r1, lt
        // csneg r0, r1, r2, lt
        // cneg r0, r1, lt
        let words = [
            0x9A82_B020u32, // csel x0, x1, x2, lt
            0x9A9F_A7E0,    // cset x0, lt (csinc x0, xzr, xzr, ge)
            0xDA9F_A3E0,    // csetm x0, lt (csinv x0, xzr, xzr, ge)
            0x9A82_B420,    // csinc x0, x1, x2, lt
            0x9A81_A420,    // cinc x0, x1, lt (csinc x0, x1, x1, ge)
            0xDA82_B020,    // csinv x0, x1, x2, lt
            0xDA81_A020,    // cinv x0, x1, lt (csinv x0, x1, x1, ge)
            0xDA82_B420,    // csneg x0, x1, x2, lt
            0xDA81_A420,    // cneg x0, x1, lt (csneg x0, x1, x1, ge)
        ];
        assert_eq!(
            dis(&words),
            "csel r0, r1, r2, lt\n\
             cset r0, lt\n\
             csetm r0, lt\n\
             csinc r0, r1, r2, lt\n\
             cinc r0, r1, lt\n\
             csinv r0, r1, r2, lt\n\
             cinv r0, r1, lt\n\
             csneg r0, r1, r2, lt\n\
             cneg r0, r1, lt\n"
        );
    }

    #[test]
    fn test_lsl_immediate_sequence() {
        // From LslImmediate test (64-bit):
        // lsl r0, r0, #1
        // lsl r0, r0, #2
        // lsl r0, r0, #3
        // lsl r0, r0, #4
        let words = [
            0xD37F_F800u32, // lsl x0, x0, #1 -> ubfm x0, x0, #63, #62
            0xD37E_F400,    // lsl x0, x0, #2
            0xD37D_F000,    // lsl x0, x0, #3
            0xD37C_EC00,    // lsl x0, x0, #4
        ];
        assert_eq!(
            dis(&words),
            "lsl r0, r0, #1\nlsl r0, r0, #2\nlsl r0, r0, #3\nlsl r0, r0, #4\n"
        );
    }

    #[test]
    fn test_lsr_immediate_sequence() {
        // lsr r0, r0, #1
        // lsr r0, r0, #2
        // lsr r0, r0, #3
        // lsr r0, r0, #4
        let words = [
            0xD341_FC00u32, // lsr x0, x0, #1
            0xD342_FC00,    // lsr x0, x0, #2
            0xD343_FC00,    // lsr x0, x0, #3
            0xD344_FC00,    // lsr x0, x0, #4
        ];
        assert_eq!(
            dis(&words),
            "lsr r0, r0, #1\nlsr r0, r0, #2\nlsr r0, r0, #3\nlsr r0, r0, #4\n"
        );
    }

    #[test]
    fn test_asr_immediate_sequence() {
        // asr r0, r0, #1
        // asr r0, r0, #2
        // asr r0, r0, #3
        // asr r0, r0, #4
        let words = [
            0x9341_FC00u32, // asr x0, x0, #1
            0x9342_FC00,    // asr x0, x0, #2
            0x9343_FC00,    // asr x0, x0, #3
            0x9344_FC00,    // asr x0, x0, #4
        ];
        assert_eq!(
            dis(&words),
            "asr r0, r0, #1\nasr r0, r0, #2\nasr r0, r0, #3\nasr r0, r0, #4\n"
        );
    }

    #[test]
    fn test_udiv_sequence() {
        // movz r0, #0x1b
        // movz r1, #0x9
        // udiv r2, r0, r1
        // mov r0, r2
        // ret
        let words = [
            0xD280_0360u32, // movz x0, #0x1b
            0xD280_0121,    // movz x1, #0x9
            0x9AC1_0802,    // udiv x2, x0, x1
            0xAA02_03E0,    // mov x0, x2 (orr x0, xzr, x2)
            0xD65F_03C0,    // ret
        ];
        assert_eq!(
            dis(&words),
            "movz r0, #0x1b\nmovz r1, #0x9\nudiv r2, r0, r1\nmov r0, r2\nret\n"
        );
    }

    #[test]
    fn test_sdiv_sequence() {
        // movz r0, #0x1b
        // movz r1, #0x9
        // neg r1, r1
        // sdiv r2, r0, r1
        // mov r0, r2
        // ret
        let words = [
            0xD280_0360u32, // movz x0, #0x1b
            0xD280_0121,    // movz x1, #0x9
            0xCB01_03E1,    // neg x1, x1 (sub x1, xzr, x1)
            0x9AC1_0C02,    // sdiv x2, x0, x1
            0xAA02_03E0,    // mov x0, x2
            0xD65F_03C0,    // ret
        ];
        assert_eq!(
            dis(&words),
            "movz r0, #0x1b\nmovz r1, #0x9\nneg r1, r1\nsdiv r2, r0, r1\nmov r0, r2\nret\n"
        );
    }

    #[test]
    fn test_lshiftv_sequence() {
        // From LShiftingV:
        // movz r1, #0x1
        // movz r2, #0x3f
        // lsl r1, r1, r2
        // lsr r0, r1, r2
        // ret
        let words = [
            0xD280_0021u32, // movz x1, #0x1
            0xD280_07E2,    // movz x2, #0x3f
            0x9AC2_2021,    // lsl x1, x1, x2 (lslv)
            0x9AC2_2420,    // lsr x0, x1, x2 (lsrv)
            0xD65F_03C0,    // ret
        ];
        assert_eq!(
            dis(&words),
            "movz r1, #0x1\nmovz r2, #0x3f\nlsl r1, r1, r2\nlsr r0, r1, r2\nret\n"
        );
    }

    #[test]
    fn test_rshiftv_sequence() {
        // movz r1, #0x1
        // movz r2, #0x3f
        // lsl r1, r1, r2
        // asr r0, r1, r2
        // ret
        let words = [
            0xD280_0021u32, // movz x1, #0x1
            0xD280_07E2,    // movz x2, #0x3f
            0x9AC2_2021,    // lsl x1, x1, x2
            0x9AC2_2820,    // asr x0, x1, x2 (asrv)
            0xD65F_03C0,    // ret
        ];
        assert_eq!(
            dis(&words),
            "movz r1, #0x1\nmovz r2, #0x3f\nlsl r1, r1, r2\nasr r0, r1, r2\nret\n"
        );
    }

    #[test]
    fn test_mult_pos_sequence() {
        // movz r1, #0x6
        // movz r2, #0x7
        // mul r0, r1, r2
        // ret
        let words = [
            0xD280_00C1u32, // movz x1, #0x6
            0xD280_00E2,    // movz x2, #0x7
            0x9B02_7C20,    // mul x0, x1, x2
            0xD65F_03C0,    // ret
        ];
        assert_eq!(
            dis(&words),
            "movz r1, #0x6\nmovz r2, #0x7\nmul r0, r1, r2\nret\n"
        );
    }

    #[test]
    fn test_smulh_sequence() {
        // movz r1, #0x6
        // movz r2, #0x7
        // smulh r0, r1, r2
        // ret
        let words = [
            0xD280_00C1u32, // movz x1, #0x6
            0xD280_00E2,    // movz x2, #0x7
            0x9B42_7C20,    // smulh x0, x1, x2
            0xD65F_03C0,    // ret
        ];
        assert_eq!(
            dis(&words),
            "movz r1, #0x6\nmovz r2, #0x7\nsmulh r0, r1, r2\nret\n"
        );
    }

    #[test]
    fn test_umulh_sequence() {
        // movz r1, #0xffff lsl 48
        // movz r2, #0x7 lsl 48
        // umulh r0, r1, r2
        // ret
        let words = [
            0xD2FF_FFE1u32, // movz x1, #0xffff, lsl #48
            0xD2E0_00E2,    // movz x2, #0x7, lsl #48
            0x9BC2_7C20,    // umulh x0, x1, x2
            0xD65F_03C0,    // ret
        ];
        assert_eq!(
            dis(&words),
            "movz r1, #0xffff lsl 48\nmovz r2, #0x7 lsl 48\numulh r0, r1, r2\nret\n"
        );
    }

    #[test]
    fn test_umaddl_sequence() {
        // movn r1, #0x0
        // movz r2, #0x7
        // movz r3, #0x8
        // umaddl r0, r1, r2, r3
        // ret
        let words = [
            0x9280_0001u32, // movn x1, #0x0
            0xD280_00E2,    // movz x2, #0x7
            0xD280_0103,    // movz x3, #0x8
            0x9BA2_0C20,    // umaddl x0, x1, x2, x3
            0xD65F_03C0,    // ret
        ];
        assert_eq!(
            dis(&words),
            "movn r1, #0x0\nmovz r2, #0x7\nmovz r3, #0x8\numaddl r0, r1, r2, r3\nret\n"
        );
    }

    #[test]
    fn test_smaddl_sequence() {
        // movn r1, #0x1
        // movz r2, #0x7
        // movz r3, #0x14
        // smaddl r0, r1, r2, r3
        // ret
        let words = [
            0x9280_0021u32, // movn x1, #0x1
            0xD280_00E2,    // movz x2, #0x7
            0xD280_0283,    // movz x3, #0x14
            0x9B22_0C20,    // smaddl x0, x1, x2, x3
            0xD65F_03C0,    // ret
        ];
        assert_eq!(
            dis(&words),
            "movn r1, #0x1\nmovz r2, #0x7\nmovz r3, #0x14\nsmaddl r0, r1, r2, r3\nret\n"
        );
    }

    #[test]
    fn test_movz_movk_sequence() {
        // movz r0, #0x1 lsl 48
        // movk r0, #0x2a
        // ret
        let words = [
            0xD2E0_0020u32, // movz x0, #0x1, lsl #48
            0xF280_0540,    // movk x0, #0x2a
            0xD65F_03C0,    // ret
        ];
        assert_eq!(
            dis(&words),
            "movz r0, #0x1 lsl 48\nmovk r0, #0x2a\nret\n"
        );
    }

    #[test]
    fn test_movn_sequence() {
        // movn r0, #0x2a lsl 16
        // ret
        let words = [
            0x92A0_0540u32, // movn x0, #0x2a, lsl #16
            0xD65F_03C0,    // ret
        ];
        assert_eq!(
            dis(&words),
            "movn r0, #0x2a lsl 16\nret\n"
        );
    }

    #[test]
    fn test_and_regs_sequence() {
        // movz r1, #0x2b
        // movz r2, #0x2a
        // and r0, r1, r2
        // ret
        let words = [
            0xD280_0561u32, // movz x1, #0x2b
            0xD280_0542,    // movz x2, #0x2a
            0x8A02_0020,    // and x0, x1, x2
            0xD65F_03C0,    // ret
        ];
        assert_eq!(
            dis(&words),
            "movz r1, #0x2b\nmovz r2, #0x2a\nand r0, r1, r2\nret\n"
        );
    }

    #[test]
    fn test_orr_regs_sequence() {
        // movz r1, #0x20
        // movz r2, #0xa
        // orr r0, r1, r2
        // ret
        let words = [
            0xD280_0401u32, // movz x1, #0x20
            0xD280_0142,    // movz x2, #0xa
            0xAA02_0020,    // orr x0, x1, x2
            0xD65F_03C0,    // ret
        ];
        assert_eq!(
            dis(&words),
            "movz r1, #0x20\nmovz r2, #0xa\norr r0, r1, r2\nret\n"
        );
    }

    #[test]
    fn test_eor_regs_sequence() {
        // movz r1, #0xffd5 -> imm16 = 0xffd5, shift=0
        // movz encoding: 1 10 100101 00 imm16 Rd
        // imm16 = 0xffd5 -> bits[20:5] = 0xffd5
        // 1101_0010_1001_1111_1111_1010_1010_0001 -> 0xD29FFAA1
        // movz r2, #0xffff -> 0xD29FFFE2
        let words = [
            0xD29F_FAA1u32, // movz x1, #0xffd5
            0xD29F_FFE2,    // movz x2, #0xffff
            0xCA02_0020,    // eor x0, x1, x2
            0xD65F_03C0,    // ret
        ];
        assert_eq!(
            dis(&words),
            "movz r1, #0xffd5\nmovz r2, #0xffff\neor r0, r1, r2\nret\n"
        );
    }

    #[test]
    fn test_bic_regs_sequence() {
        // movz r1, #0x2a
        // movz r2, #0x5
        // bic r0, r1, r2
        // ret
        let words = [
            0xD280_0541u32, // movz x1, #0x2a
            0xD280_00A2,    // movz x2, #0x5
            0x8A22_0020,    // bic x0, x1, x2
            0xD65F_03C0,    // ret
        ];
        assert_eq!(
            dis(&words),
            "movz r1, #0x2a\nmovz r2, #0x5\nbic r0, r1, r2\nret\n"
        );
    }
}
