//! String and regexp lowering.

use std::ffi::{c_long, c_void};

use crate::backend::lir::{self, asm_ccall, asm_comment, Assembler, EC, Opnd, Target};
use crate::cruby::*;
use crate::hir::{FrameState, Function, InsnId};
use super::gc_fastpath;
use super::{JITState, gen_prepare_leaf_call_with_gc, gen_prepare_non_leaf_call, gen_push_opnds};

pub(super) fn gen_intern(asm: &mut Assembler, val: Opnd, state: &FrameState) -> Opnd {
    gen_prepare_leaf_call_with_gc(asm, state);

    asm_ccall!(asm, rb_str_intern, val)
}

const STR_INLINE_STORE_MAX_BYTES: usize = 128;

pub(super) fn gen_string_copy(jit: &mut JITState, asm: &mut Assembler, function: &Function, val_id: InsnId, recv: Opnd, chilled: bool, state: &FrameState) -> Opnd {
    // TODO: split rb_ec_str_resurrect into separate functions
    gen_prepare_leaf_call_with_gc(asm, state);

    let Some(src) = function.type_of(val_id).ruby_object() else {
        return asm_ccall!(asm, rb_ec_str_resurrect, EC, recv, (chilled as i64).into());
    };

    let slow_path = |asm: &mut Assembler| asm_ccall!(asm, rb_ec_str_resurrect, EC, Opnd::Value(src), (chilled as i64).into());

    let mut alloc_size: usize = 0;
    let mut flags: VALUE = VALUE(0);
    let mut len: c_long = 0;
    let mut byte_size: usize = 0;
    let has_fastpath = unsafe {
        rb_zjit_str_resurrect_fastpath(src, chilled, &mut alloc_size, &mut flags, &mut len, &mut byte_size)
    };
    if !has_fastpath {
        return slow_path(asm);
    }

    let full_flags = flags.as_u64();
    let klass = unsafe { rb_cString };

    // Because inline stores are 8 bytes, storing large embedded strings would
    // generate a large number of stores (!125 for a string in the 1024b size
    // pool). Here we choose an arbitrary threshold (128 bytes, or 16 stores),
    // above which we'll emit a C call to memcpy instead of multiple stores.
    if byte_size > STR_INLINE_STORE_MAX_BYTES {
        return gc_fastpath::gc_fastpath_new_obj(jit, asm, function, state, alloc_size, full_flags, klass,
            |asm, obj| {
                asm.store(Opnd::mem(VALUE_BITS, obj, RUBY_OFFSET_RSTRING_LEN), Opnd::Imm(len));
                let src_obj = asm.load(Opnd::Value(src));
                let src_ptr = asm.lea(Opnd::mem(64, src_obj, RUBY_OFFSET_RSTRING_AS_ARY));
                let dst_ptr = asm.lea(Opnd::mem(64, obj, RUBY_OFFSET_RSTRING_AS_ARY));
                asm.ccall(memcpy as *const u8, vec![dst_ptr, src_ptr, Opnd::UImm(byte_size as u64)]);
            },
            slow_path);
    }

    // Pre-process string data into 8 byte chunks and take care of padding
    // outside the loop, so we can keep the complexity out of the fast path
    // loop.
    let padded_size = byte_size.next_multiple_of(8);
    let Some(src_bytes) = (unsafe { src.as_rstring_byte_slice() }) else {
        return slow_path(asm);
    };
    debug_assert_eq!(src_bytes.len(), len as usize);
    let mut string_bytes = vec![0u8; padded_size];
    string_bytes[..src_bytes.len()].copy_from_slice(src_bytes);

    gc_fastpath::gc_fastpath_new_obj(jit, asm, function, state, alloc_size, full_flags, klass,
        |asm, obj| {
            asm.store(Opnd::mem(VALUE_BITS, obj, RUBY_OFFSET_RSTRING_LEN), Opnd::Imm(len));
            for (i, chunk) in string_bytes.chunks_exact(8).enumerate() {
                let word = u64::from_le_bytes(chunk.try_into().unwrap());
                let offset = RUBY_OFFSET_RSTRING_AS_ARY + (i as i32) * 8;
                asm.store(Opnd::mem(64, obj, offset), Opnd::UImm(word));
            }
        },
        slow_path)
}

unsafe extern "C" {
    fn memcpy(dst: *mut c_void, src: *const c_void, n: usize) -> *mut c_void;
}

pub(super) fn gen_string_equal(asm: &mut Assembler, left: Opnd, right: Opnd) -> lir::Opnd {
    asm_ccall!(asm, rb_yarv_str_eql_internal, left, right)
}

pub(super) fn gen_anytostring(asm: &mut Assembler, val: lir::Opnd, state: &FrameState) -> lir::Opnd {
    gen_prepare_leaf_call_with_gc(asm, state);

    asm_ccall!(asm, rb_any_to_s, val)
}

pub(super) fn gen_toregexp(jit: &mut JITState, asm: &mut Assembler, function: &Function, opt: usize, values: Vec<Opnd>, state: &FrameState) -> Opnd {
    gen_prepare_non_leaf_call(jit, asm, function, state);

    let first_opnd_ptr = gen_push_opnds(jit, asm, &values);
    asm_ccall!(asm, rb_reg_new_from_values, values.len().into(), first_opnd_ptr, opt.into())
}

pub(super) fn gen_string_concat(jit: &mut JITState, asm: &mut Assembler, function: &Function, strings: Vec<Opnd>, state: &FrameState) -> Opnd {
    gen_prepare_non_leaf_call(jit, asm, function, state);

    let first_string_ptr = gen_push_opnds(jit, asm, &strings);
    asm_ccall!(asm, rb_str_concat_literals, strings.len().into(), first_string_ptr)
}

fn get_string_ptr(asm: &mut Assembler, string: Opnd) -> Opnd {
    asm_comment!(asm, "get string pointer for embedded or heap");
    let string = asm.load_mem(string);
    let flags = Opnd::mem(VALUE_BITS, string, RUBY_OFFSET_RBASIC_FLAGS);
    asm.test(flags, (RSTRING_NOEMBED as u64).into());
    let heap_ptr = asm.load(Opnd::mem(
        usize::BITS as u8,
        string,
        RUBY_OFFSET_RSTRING_AS_HEAP_PTR,
    ));
    // Load the address of the embedded array
    // (struct RString *)(obj)->as.ary
    let ary = asm.lea(Opnd::mem(VALUE_BITS, string, RUBY_OFFSET_RSTRING_AS_ARY));
    asm.csel_nz(heap_ptr, ary)
}

pub(super) fn gen_string_getbyte(asm: &mut Assembler, string: Opnd, index: Opnd) -> Opnd {
    let string_ptr = get_string_ptr(asm, string);
    // TODO(max): Use SIB indexing here once the backend supports it
    let string_ptr = asm.add(string_ptr, index);
    let byte = asm.load(Opnd::mem(8, string_ptr, 0));
    // Zero-extend the byte to 64 bits
    let byte = byte.with_num_bits(64);
    let byte = asm.and(byte, 0xFF.into());
    // Tag the byte
    let byte = asm.lshift(byte, Opnd::UImm(1));
    asm.or(byte, Opnd::UImm(1))
}

pub(super) fn gen_string_setbyte_fixnum(asm: &mut Assembler, string: Opnd, index: Opnd, value: Opnd) -> Opnd {
    // rb_str_setbyte is not leaf, but we guard types and index ranges in HIR
    asm_ccall!(asm, rb_str_setbyte, string, index, value)
}

pub(super) fn gen_string_append(jit: &mut JITState, asm: &mut Assembler, function: &Function, string: Opnd, val: Opnd, state: &FrameState) -> Opnd {
    gen_prepare_non_leaf_call(jit, asm, function, state);

    // Test if string encodings differ. If different, use rb_str_buf_append. If the same,
    // use rb_jit_str_simple_append, which calls rb_str_cat.
    asm_comment!(asm, "<< on strings");

    // Take receiver's object flags XOR arg's flags. If any
    // string-encoding flags are different between the two,
    // the encodings don't match.
    let string_reg = asm.load_mem(string);
    let val_reg = asm.load_mem(val);
    let flags_xor = asm.xor(
        Opnd::mem(VALUE_BITS, string_reg, RUBY_OFFSET_RBASIC_FLAGS),
        Opnd::mem(VALUE_BITS, val_reg, RUBY_OFFSET_RBASIC_FLAGS)
    );
    asm.test(flags_xor, Opnd::UImm(RUBY_ENCODING_MASK as u64));

    let hir_block_id = asm.current_block().hir_block_id;
    let rpo_idx = asm.current_block().rpo_index;
    let mismatch_block = asm.new_block(hir_block_id, false, rpo_idx);
    let mismatch_edge = Target::Block(Box::new(lir::BranchEdge { target: mismatch_block, args: vec![] }));
    let result_block = asm.new_block(hir_block_id, false, rpo_idx);
    let result_edge = Target::Block(Box::new(lir::BranchEdge { target: result_block, args: vec![] }));

    asm.jnz(jit, mismatch_edge);

    // If encodings match, call the simple append function
    asm_ccall!(asm, rb_jit_str_simple_append, string, val);
    asm.jmp(result_edge.clone());

    // If encodings are different, use a slower encoding-aware concatenate
    asm.set_current_block(mismatch_block);
    let label = jit.get_label(asm, mismatch_block, hir_block_id);
    asm.write_label(label);
    asm_ccall!(asm, rb_str_buf_append, string, val);
    asm.jmp(result_edge);

    // Join block
    asm.set_current_block(result_block);
    let label = jit.get_label(asm, result_block, hir_block_id);
    asm.write_label(label);

    // Either append function returns the receiver
    string
}

pub(super) fn gen_string_append_codepoint(jit: &mut JITState, asm: &mut Assembler, function: &Function, string: Opnd, val: Opnd, state: &FrameState) -> Opnd {
    gen_prepare_non_leaf_call(jit, asm, function, state);
    asm_ccall!(asm, rb_jit_str_concat_codepoint, string, val)
}
