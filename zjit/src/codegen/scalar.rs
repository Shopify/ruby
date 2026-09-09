//! Scalar lowering.

use crate::backend::lir::{self, asm_ccall, Assembler, Opnd};
use crate::cruby::*;
use crate::hir::{FrameState, Function};
use crate::hir::SideExitReason::*;
use super::{gen_prepare_leaf_call_with_gc, JITState};
use super::guards::side_exit;

pub(super) fn gen_unbox_fixnum(asm: &mut Assembler, val: Opnd) -> Opnd {
    asm.rshift(val, Opnd::UImm(1))
}

pub(super) fn gen_fixnum_bit_check(asm: &mut Assembler, val: Opnd, index: u8) -> Opnd {
    let bit_test: u64 = 0x01 << (index + 1);
    asm.test(val, bit_test.into());
    asm.csel_z(Qtrue.into(), Qfalse.into())
}

pub(super) fn gen_fixnum_add(jit: &mut JITState, asm: &mut Assembler, function: &Function, left: lir::Opnd, right: lir::Opnd, state: &FrameState) -> lir::Opnd {
    // Add left + right and test for overflow
    let left_untag = asm.sub(left, Opnd::Imm(1));
    let out_val = asm.add(left_untag, right);
    asm.jo(jit, side_exit(jit, function, state, FixnumAddOverflow));

    out_val
}

pub(super) fn gen_fixnum_sub(jit: &mut JITState, asm: &mut Assembler, function: &Function, left: lir::Opnd, right: lir::Opnd, state: &FrameState) -> lir::Opnd {
    // Subtract left - right and test for overflow
    let val_untag = asm.sub(left, right);
    asm.jo(jit, side_exit(jit, function, state, FixnumSubOverflow));
    asm.add(val_untag, Opnd::Imm(1))
}

pub(super) fn gen_fixnum_mult(jit: &mut JITState, asm: &mut Assembler, function: &Function, left: lir::Opnd, right: lir::Opnd, state: &FrameState) -> lir::Opnd {
    // Do some bitwise gymnastics to handle tag bits
    // x * y is translated to (x >> 1) * (y - 1) + 1
    let left_untag = asm.rshift(left, Opnd::UImm(1));
    let right_untag = asm.sub(right, Opnd::UImm(1));
    let out_val = asm.mul(left_untag, right_untag);

    // Test for overflow
    asm.jo_mul(jit, side_exit(jit, function, state, FixnumMultOverflow));
    asm.add(out_val, Opnd::UImm(1))
}

pub(super) fn gen_fixnum_div(jit: &mut JITState, asm: &mut Assembler, function: &Function, left: lir::Opnd, right: lir::Opnd, state: &FrameState) -> lir::Opnd {
    gen_prepare_leaf_call_with_gc(asm, state);

    // Side exit if rhs is 0
    asm.cmp(right, Opnd::from(VALUE::fixnum_from_usize(0)));
    asm.je(jit, side_exit(jit, function, state, FixnumDivByZero));
    asm_ccall!(asm, rb_jit_fix_div_fix, left, right)
}

pub(super) fn gen_float_add(asm: &mut Assembler, recv: lir::Opnd, other: lir::Opnd, state: &FrameState) -> lir::Opnd {
    gen_prepare_leaf_call_with_gc(asm, state);
    asm_ccall!(asm, rb_float_plus, recv, other)
}

pub(super) fn gen_float_sub(asm: &mut Assembler, recv: lir::Opnd, other: lir::Opnd, state: &FrameState) -> lir::Opnd {
    gen_prepare_leaf_call_with_gc(asm, state);
    asm_ccall!(asm, rb_float_minus, recv, other)
}

pub(super) fn gen_float_mul(asm: &mut Assembler, recv: lir::Opnd, other: lir::Opnd, state: &FrameState) -> lir::Opnd {
    gen_prepare_leaf_call_with_gc(asm, state);
    asm_ccall!(asm, rb_float_mul, recv, other)
}

pub(super) fn gen_float_div(asm: &mut Assembler, recv: lir::Opnd, other: lir::Opnd, state: &FrameState) -> lir::Opnd {
    gen_prepare_leaf_call_with_gc(asm, state);
    asm_ccall!(asm, rb_float_div, recv, other)
}

pub(super) fn gen_float_to_int(asm: &mut Assembler, recv: lir::Opnd, state: &FrameState) -> lir::Opnd {
    gen_prepare_leaf_call_with_gc(asm, state);
    asm_ccall!(asm, rb_flo_to_i, recv)
}

pub(super) fn gen_fixnum_eq(asm: &mut Assembler, left: lir::Opnd, right: lir::Opnd) -> lir::Opnd {
    asm.cmp(left, right);
    asm.csel_e(Qtrue.into(), Qfalse.into())
}

pub(super) fn gen_fixnum_neq(asm: &mut Assembler, left: lir::Opnd, right: lir::Opnd) -> lir::Opnd {
    asm.cmp(left, right);
    asm.csel_ne(Qtrue.into(), Qfalse.into())
}

pub(super) fn gen_fixnum_lt(asm: &mut Assembler, left: lir::Opnd, right: lir::Opnd) -> lir::Opnd {
    asm.cmp(left, right);
    asm.csel_l(Qtrue.into(), Qfalse.into())
}

pub(super) fn gen_fixnum_le(asm: &mut Assembler, left: lir::Opnd, right: lir::Opnd) -> lir::Opnd {
    asm.cmp(left, right);
    asm.csel_le(Qtrue.into(), Qfalse.into())
}

pub(super) fn gen_fixnum_gt(asm: &mut Assembler, left: lir::Opnd, right: lir::Opnd) -> lir::Opnd {
    asm.cmp(left, right);
    asm.csel_g(Qtrue.into(), Qfalse.into())
}

pub(super) fn gen_fixnum_ge(asm: &mut Assembler, left: lir::Opnd, right: lir::Opnd) -> lir::Opnd {
    asm.cmp(left, right);
    asm.csel_ge(Qtrue.into(), Qfalse.into())
}

pub(super) fn gen_fixnum_and(asm: &mut Assembler, left: lir::Opnd, right: lir::Opnd) -> lir::Opnd {
    asm.and(left, right)
}

pub(super) fn gen_fixnum_or(asm: &mut Assembler, left: lir::Opnd, right: lir::Opnd) -> lir::Opnd {
    asm.or(left, right)
}

pub(super) fn gen_int_or(asm: &mut Assembler, left: lir::Opnd, right: lir::Opnd) -> lir::Opnd {
    asm.or(left, right)
}

pub(super) fn gen_fixnum_xor(asm: &mut Assembler, left: lir::Opnd, right: lir::Opnd) -> lir::Opnd {
    // XOR and then re-tag the resulting fixnum
    let out_val = asm.xor(left, right);
    asm.add(out_val, Opnd::UImm(1))
}

pub(super) fn gen_fixnum_lshift(jit: &mut JITState, asm: &mut Assembler, function: &Function, left: lir::Opnd, shift_amount: u64, state: &FrameState) -> lir::Opnd {
    // Shift amount is known statically to be in the range [0, 63]
    assert!(shift_amount < 64);
    let in_val = asm.sub(left, Opnd::UImm(1));  // Drop tag bit
    let out_val = asm.lshift(in_val, shift_amount.into());
    let unshifted = asm.rshift(out_val, shift_amount.into());
    asm.cmp(in_val, unshifted);
    asm.jne(jit, side_exit(jit, function, state, FixnumLShiftOverflow));
    // Re-tag the output value
    let out_val = asm.add(out_val, 1.into());
    out_val
}

pub(super) fn gen_fixnum_rshift(asm: &mut Assembler, left: lir::Opnd, shift_amount: u64) -> lir::Opnd {
    // Shift amount is known statically to be in the range [0, 63]
    assert!(shift_amount < 64);
    let result = asm.rshift(left, shift_amount.into());
    // Re-tag the output value
    asm.or(result, 1.into())
}

pub(super) fn gen_fixnum_mod(jit: &mut JITState, asm: &mut Assembler, function: &Function, left: lir::Opnd, right: lir::Opnd, state: &FrameState) -> lir::Opnd {
    // Check for left % 0, which raises ZeroDivisionError
    asm.cmp(right, Opnd::from(VALUE::fixnum_from_usize(0)));
    asm.je(jit, side_exit(jit, function, state, FixnumModByZero));
    asm_ccall!(asm, rb_fix_mod_fix, left, right)
}

pub(super) fn gen_fixnum_aref(asm: &mut Assembler, recv: lir::Opnd, index: lir::Opnd) -> lir::Opnd {
    asm_ccall!(asm, rb_fix_aref, recv, index)
}

pub(super) fn gen_is_bit_equal(asm: &mut Assembler, left: lir::Opnd, right: lir::Opnd) -> lir::Opnd {
    asm.cmp(left, right);
    asm.csel_e(Opnd::Imm(1), Opnd::Imm(0))
}

pub(super) fn gen_is_bit_not_equal(asm: &mut Assembler, left: lir::Opnd, right: lir::Opnd) -> lir::Opnd {
    asm.cmp(left, right);
    asm.csel_ne(Opnd::Imm(1), Opnd::Imm(0))
}

pub(super) fn gen_box_bool(asm: &mut Assembler, val: lir::Opnd) -> lir::Opnd {
    asm.test(val, val);
    asm.csel_nz(Opnd::Value(Qtrue), Opnd::Value(Qfalse))
}

pub(super) fn gen_box_fixnum(jit: &mut JITState, asm: &mut Assembler, function: &Function, val: lir::Opnd, state: &FrameState) -> lir::Opnd {
    // Load the value, then test for overflow and tag it
    let val = asm.load_mem(val);
    let shifted = asm.lshift(val, Opnd::UImm(1));
    asm.jo(jit, side_exit(jit, function, state, BoxFixnumOverflow));
    asm.or(shifted, Opnd::UImm(RUBY_FIXNUM_FLAG as u64))
}

pub(super) fn gen_test(asm: &mut Assembler, val: lir::Opnd) -> lir::Opnd {
    // Test if any bit (outside of the Qnil bit) is on
    // See RB_TEST(), include/ruby/internal/special_consts.h
    asm.test(val, Opnd::Imm(!Qnil.as_i64()));
    asm.csel_e(0.into(), 1.into())
}
