//! Object, collection, and allocation code generation.

use crate::backend::lir::{self, *};
use crate::cruby::*;
use crate::hir::{FrameState, Function, InsnId, RangeType};
use std::ffi::c_long;
use super::gc_fastpath;
use super::{
    JITState, gen_push_opnds,
    gen_prepare_fallback_call, gen_prepare_leaf_call_with_gc, gen_prepare_non_leaf_call,
};

pub(super) fn gen_hash_dup(
    jit: &mut JITState,
    asm: &mut Assembler,
    function: &Function,
    val_id: InsnId,
    val: Opnd,
    state: &FrameState,
) -> lir::Opnd {
    if let Some(src) = function.type_of(val_id).ruby_object() {
        let mut alloc_size: usize = 0;
        let mut flags = VALUE(0);
        let mut ifnone = VALUE(0);
        let mut bound: c_long = 0;
        if unsafe { rb_zjit_hash_dup_can_fastpath(src, &mut alloc_size, &mut flags, &mut ifnone, &mut bound) } {
            let klass = unsafe { rb_cHash };

            let src_ptr = src.as_usize() as *const u8;
            let hint_word = unsafe { (src_ptr.add(RUBY_OFFSET_RHASH_AR_HINT as usize) as *const u64).read() };
            let pairs_base = unsafe { src_ptr.add(RUBY_OFFSET_RHASH_AR_PAIRS as usize) as *const VALUE };

            return gc_fastpath::gc_fastpath_new_obj(jit, asm, function, state, alloc_size, flags.into(), klass,
                |asm, obj| {
                    asm.store(Opnd::mem(VALUE_BITS, obj, RUBY_OFFSET_RHASH_IFNONE), Opnd::Value(ifnone));
                    asm.store(Opnd::mem(VALUE_BITS, obj, RUBY_OFFSET_RHASH_AR_HINT), Opnd::UImm(hint_word));
                    for i in 0..bound {
                        let pair = unsafe { pairs_base.add(2 * (i as usize)) };
                        let (key, value) = unsafe { (pair.read(), pair.add(1).read()) };
                        let offset = RUBY_OFFSET_RHASH_AR_PAIRS + (i as i32) * 2 * SIZEOF_VALUE_I32;
                        asm.store(Opnd::mem(VALUE_BITS, obj, offset), Opnd::Value(key));
                        asm.store(Opnd::mem(VALUE_BITS, obj, offset + SIZEOF_VALUE_I32), Opnd::Value(value));
                    }
                },
                |asm| {
                    gen_prepare_leaf_call_with_gc(asm, state);
                    asm_ccall!(asm, rb_hash_resurrect, val)
                });
        }
    }

    gen_prepare_leaf_call_with_gc(asm, state);
    asm_ccall!(asm, rb_hash_resurrect, val)
}


pub(super) fn gen_hash_aref(jit: &mut JITState, asm: &mut Assembler, function: &Function, hash: Opnd, key: Opnd, state: &FrameState) -> lir::Opnd {
    gen_prepare_non_leaf_call(jit, asm, function, state);
    asm_ccall!(asm, rb_hash_aref, hash, key)
}


pub(super) fn gen_hash_aset(jit: &mut JITState, asm: &mut Assembler, function: &Function, hash: Opnd, key: Opnd, val: Opnd, state: &FrameState) {
    gen_prepare_non_leaf_call(jit, asm, function, state);
    asm_ccall!(asm, rb_hash_aset, hash, key, val);
}


pub(super) fn gen_array_push(asm: &mut Assembler, array: Opnd, val: Opnd, state: &FrameState) {
    gen_prepare_leaf_call_with_gc(asm, state);
    asm_ccall!(asm, rb_ary_push, array, val);
}


pub(super) fn gen_to_new_array(jit: &mut JITState, asm: &mut Assembler, function: &Function, val: Opnd, state: &FrameState) -> lir::Opnd {
    gen_prepare_non_leaf_call(jit, asm, function, state);
    asm_ccall!(asm, rb_vm_splat_array, Opnd::Value(Qtrue), val)
}


pub(super) fn gen_to_array(jit: &mut JITState, asm: &mut Assembler, function: &Function, val: Opnd, state: &FrameState) -> lir::Opnd {
    gen_prepare_non_leaf_call(jit, asm, function, state);
    asm_ccall!(asm, rb_vm_splat_array, Opnd::Value(Qfalse), val)
}


pub(super) fn gen_array_extend(jit: &mut JITState, asm: &mut Assembler, function: &Function, left: Opnd, right: Opnd, state: &FrameState) {
    gen_prepare_non_leaf_call(jit, asm, function, state);
    asm_ccall!(asm, rb_ary_concat, left, right);
}


/// Compile an array duplication instruction
pub(super) fn gen_array_dup(
    jit: &mut JITState,
    asm: &mut Assembler,
    function: &Function,
    val_id: InsnId,
    val: lir::Opnd,
    state: &FrameState,
) -> lir::Opnd {
    // duparray resurrects a frozen literal array baked into the ISEQ, so its elements are known
    // here. When the resurrected copy would be embedded, bump-allocate it inline and store the
    // elements directly; the fresh object is young and white, so those writes need no write
    // barrier (elements may be heap objects).
    if let Some(src) = function.type_of(val_id).ruby_object() {
        let mut alloc_size: usize = 0;
        let mut flags = VALUE(0);
        let mut len: std::os::raw::c_long = 0;
        if unsafe { rb_zjit_array_dup_can_fastpath(src, &mut alloc_size, &mut flags, &mut len) } {
            let klass = unsafe { rb_cArray };
            return gc_fastpath::gc_fastpath_new_obj(jit, asm, function, state, alloc_size, flags.into(), klass, |asm, obj| {
                for i in 0..len {
                    let elem = unsafe { rb_ary_entry(src, i) };
                    let offset = RUBY_OFFSET_RARRAY_AS_ARY + (i as i32) * SIZEOF_VALUE_I32;
                    asm.store(Opnd::mem(VALUE_BITS, obj, offset), Opnd::Value(elem));
                }
            },
            |asm| {
                gen_prepare_leaf_call_with_gc(asm, state);
                asm_ccall!(asm, rb_ary_resurrect, val)
            });
        }
    }

    gen_prepare_leaf_call_with_gc(asm, state);
    asm_ccall!(asm, rb_ary_resurrect, val)
}


/// Compile array access (`array[index]`)
pub(super) fn gen_array_aref(
    asm: &mut Assembler,
    array: Opnd,
    index: Opnd,
) -> lir::Opnd {
    let unboxed_idx = asm.load_mem(index);
    let array = asm.load_mem(array);
    let array_ptr = gen_array_ptr(asm, array);
    let elem_offset = asm.lshift(unboxed_idx, Opnd::UImm(SIZEOF_VALUE.trailing_zeros() as u64));
    let elem_ptr = asm.add(array_ptr, elem_offset);
    asm.load(Opnd::mem(VALUE_BITS, elem_ptr, 0))
}


pub(super) fn gen_array_aset(
    asm: &mut Assembler,
    array: Opnd,
    index: Opnd,
    val: Opnd,
) {
    let unboxed_idx = asm.load_mem(index);
    let array = asm.load_mem(array);
    let array_ptr = gen_array_ptr(asm, array);
    let elem_offset = asm.lshift(unboxed_idx, Opnd::UImm(SIZEOF_VALUE.trailing_zeros() as u64));
    let elem_ptr = asm.add(array_ptr, elem_offset);
    asm.store(Opnd::mem(VALUE_BITS, elem_ptr, 0), val);
}


pub(super) fn gen_array_pop(asm: &mut Assembler, array: Opnd, state: &FrameState) -> lir::Opnd {
    gen_prepare_leaf_call_with_gc(asm, state);
    asm_ccall!(asm, rb_ary_pop, array)
}


pub(super) fn gen_array_length(asm: &mut Assembler, array: Opnd) -> lir::Opnd {
    let array = asm.load_mem(array);
    let flags = Opnd::mem(VALUE_BITS, array, RUBY_OFFSET_RBASIC_FLAGS);
    let embedded_len = asm.and(flags, (RARRAY_EMBED_LEN_MASK as u64).into());
    let embedded_len = asm.rshift(embedded_len, (RARRAY_EMBED_LEN_SHIFT as u64).into());
    // cmov between the embedded length and heap length depending on the embed flag
    asm.test(flags, (RARRAY_EMBED_FLAG as u64).into());
    let heap_len = Opnd::mem(c_long::BITS as u8, array, RUBY_OFFSET_RARRAY_AS_HEAP_LEN);
    asm.csel_nz(embedded_len, heap_len)
}


pub(super) fn gen_array_ptr(asm: &mut Assembler, array: Opnd) -> lir::Opnd {
    let flags = Opnd::mem(VALUE_BITS, array, RUBY_OFFSET_RBASIC_FLAGS);
    asm.test(flags, (RARRAY_EMBED_FLAG as u64).into());
    let heap_ptr = Opnd::mem(usize::BITS as u8, array, RUBY_OFFSET_RARRAY_AS_HEAP_PTR);
    let embedded_ptr = asm.lea(Opnd::mem(VALUE_BITS, array, RUBY_OFFSET_RARRAY_AS_ARY));
    asm.csel_nz(embedded_ptr, heap_ptr)
}


/// Compile ArrayMax - find the maximum element among array elements
pub(super) fn gen_array_max(
    jit: &JITState,
    asm: &mut Assembler,
    function: &Function,
    elements: Vec<Opnd>,
    state: &FrameState,
) -> lir::Opnd {
    gen_prepare_fallback_call(jit, asm, function, state);

    let array_len: u32 = elements.len().try_into().expect("Unable to fit length of elements into u32");

    // After gen_prepare_non_leaf_call, the elements are spilled to the Ruby stack.
    // Get a pointer to the first element on the Ruby stack.
    let stack_bottom = state.stack().len() - elements.len();
    let elements_ptr = asm.lea(Opnd::mem(VALUE_BITS, SP, stack_bottom as i32 * SIZEOF_VALUE_I32));

    unsafe extern "C" {
        fn rb_vm_opt_newarray_max(ec: EcPtr, num: u32, elts: *const VALUE) -> VALUE;
    }

    asm.ccall(
        rb_vm_opt_newarray_max as *const u8,
        vec![EC, array_len.into(), elements_ptr],
    )
}


/// Find the minimum element among array elements
pub(super) fn gen_array_min(
    jit: &JITState,
    asm: &mut Assembler,
    function: &Function,
    elements: Vec<Opnd>,
    state: &FrameState,
) -> lir::Opnd {
    gen_prepare_fallback_call(jit, asm, function, state);

    let array_len: u32 = elements.len().try_into().expect("Unable to fit length of elements into u32");

    // After gen_prepare_non_leaf_call, the elements are spilled to the Ruby stack.
    // Get a pointer to the first element on the Ruby stack.
    let stack_bottom = state.stack().len() - elements.len();
    let elements_ptr = asm.lea(Opnd::mem(VALUE_BITS, SP, stack_bottom as i32 * SIZEOF_VALUE_I32));

    unsafe extern "C" {
        fn rb_vm_opt_newarray_min(ec: EcPtr, num: u32, elts: *const VALUE) -> VALUE;
    }

    asm.ccall(
        rb_vm_opt_newarray_min as *const u8,
        vec![EC, array_len.into(), elements_ptr],
    )
}


pub(super) fn gen_array_include(
    jit: &JITState,
    asm: &mut Assembler,
    function: &Function,
    elements: Vec<Opnd>,
    target: Opnd,
    state: &FrameState,
) -> lir::Opnd {
    gen_prepare_fallback_call(jit, asm, function, state);

    let array_len: c_long = elements.len().try_into().expect("Unable to fit length of elements into c_long");

    // After gen_prepare_non_leaf_call, the elements are spilled to the Ruby stack.
    // The elements are at the bottom of the virtual stack, followed by the target.
    // Get a pointer to the first element on the Ruby stack.
    let stack_bottom = state.stack().len() - elements.len() - 1;
    let elements_ptr = asm.lea(Opnd::mem(64, SP, stack_bottom as i32 * SIZEOF_VALUE_I32));

    unsafe extern "C" {
        fn rb_vm_opt_newarray_include_p(ec: EcPtr, num: c_long, elts: *const VALUE, target: VALUE) -> VALUE;
    }
    asm_ccall!(
        asm,
        rb_vm_opt_newarray_include_p,
        EC, array_len.into(), elements_ptr, target
    )
}


pub(super) fn gen_array_pack_buffer(
    jit: &JITState,
    asm: &mut Assembler,
    function: &Function,
    elements: Vec<Opnd>,
    fmt: Opnd,
    buffer: Option<Opnd>,
    state: &FrameState,
) -> lir::Opnd {
    gen_prepare_fallback_call(jit, asm, function, state);

    let array_len: c_long = elements.len().try_into().expect("Unable to fit length of elements into c_long");

    // After gen_prepare_non_leaf_call, the elements are spilled to the Ruby stack.
    // The elements are at the bottom of the virtual stack, followed by the fmt, and optionally the buffer.
    // Get a pointer to the first element on the Ruby stack.
    let stack_bottom = if buffer.is_some() {
        state.stack().len() - elements.len() - 2
    } else {
        state.stack().len() - elements.len() - 1
    };
    let elements_ptr = asm.lea(Opnd::mem(64, SP, stack_bottom as i32 * SIZEOF_VALUE_I32));

    unsafe extern "C" {
        fn rb_vm_opt_newarray_pack_buffer(ec: EcPtr, num: c_long, elts: *const VALUE, fmt: VALUE, buffer: VALUE) -> VALUE;
    }
    asm_ccall!(
        asm,
        rb_vm_opt_newarray_pack_buffer,
        EC, array_len.into(), elements_ptr, fmt, buffer.unwrap_or_else(|| Qundef.into())
    )
}


pub(super) fn gen_dup_array_include(
    jit: &JITState,
    asm: &mut Assembler,
    function: &Function,
    ary: VALUE,
    target: Opnd,
    state: &FrameState,
) -> lir::Opnd {
    gen_prepare_non_leaf_call(jit, asm, function, state);

    unsafe extern "C" {
        fn rb_vm_opt_duparray_include_p(ec: EcPtr, ary: VALUE, target: VALUE) -> VALUE;
    }
    asm_ccall!(
        asm,
        rb_vm_opt_duparray_include_p,
        EC, ary.into(), target
    )
}


/// Compile a new hash instruction
pub(super) fn gen_new_hash(
    jit: &mut JITState,
    asm: &mut Assembler,
    function: &Function,
    elements: Vec<Opnd>,
    sym_keys: bool,
    state: &FrameState,
) -> lir::Opnd {
    if elements.is_empty() {
        gen_prepare_leaf_call_with_gc(asm, state);

        let mut flags = VALUE(0);
        let alloc_size = unsafe { rb_zjit_hash_new_size(&mut flags, 0) };
        let klass = unsafe { rb_cHash };

        gc_fastpath::gc_fastpath_new_obj(jit, asm, function, state, alloc_size, flags.into(), klass,
            |asm, hash| {
                asm.store(Opnd::mem(VALUE_BITS, hash, RUBY_OFFSET_RHASH_IFNONE), Qnil.into());
            },
            |asm| {
                asm_ccall!(asm, rb_hash_new,)
            })
    // TODO: we should use effects_of for this (we would need to add it).
    } else if sym_keys {
        // Symbols hash and compare without running Ruby and those operations never raise so
        // the bulk insert is leaf.
        gen_prepare_leaf_call_with_gc(asm, state);

        let num_pairs = elements.len() / 2;
        let hash = if num_pairs <= RUBY_RHASH_AR_TABLE_MAX_SIZE as usize {
            let mut flags = VALUE(0);
            let alloc_size = unsafe { rb_zjit_hash_new_size(&mut flags, num_pairs) };
            let klass = unsafe { rb_cHash };

            gc_fastpath::gc_fastpath_new_obj(jit, asm, function, state, alloc_size, flags.into(), klass,
                |asm, hash| {
                    asm.store(Opnd::mem(VALUE_BITS, hash, RUBY_OFFSET_RHASH_IFNONE), Qnil.into());
                },
                |asm| {
                    asm_ccall!(asm, rb_hash_new_capa, num_pairs.into())
                })
        } else {
            asm_ccall!(asm, rb_hash_new_capa, num_pairs.into())
        };

        let argv = gen_push_opnds(jit, asm, &elements);
        asm_ccall!(asm, rb_hash_bulk_insert, elements.len().into(), argv, hash);
        hash
    } else {
        gen_prepare_non_leaf_call(jit, asm, function, state);

        let argv = gen_push_opnds(jit, asm, &elements);
        asm_ccall!(asm, rb_hash_new_with_bulk_insert, elements.len().into(), argv)
    }
}


/// Compile a new range instruction
pub(super) fn gen_new_range(
    jit: &mut JITState,
    asm: &mut Assembler,
    function: &Function,
    low: lir::Opnd,
    high: lir::Opnd,
    flag: RangeType,
    state: &FrameState,
) -> lir::Opnd {
    let hir_block_id = asm.current_block().hir_block_id;
    let rpo_idx = asm.current_block().rpo_index;
    let fast_block = asm.new_block(hir_block_id, false, rpo_idx);
    let slow_block = asm.new_block(hir_block_id, false, rpo_idx);
    let result_block = asm.new_block(hir_block_id, false, rpo_idx);
    let fast_edge = Target::Block(Box::new(lir::BranchEdge { target: fast_block, args: vec![] }));
    let slow_edge = Target::Block(Box::new(lir::BranchEdge { target: slow_block, args: vec![] }));
    let result_edge = |range| Target::Block(Box::new(lir::BranchEdge {
        target: result_block,
        args: vec![range],
    }));

    // rb_range_new skips the call to <=> when either endpoint is nil or both are fixnums.
    asm.cmp(low, Qnil.into());
    asm.je(jit, fast_edge.clone());
    asm.cmp(high, Qnil.into());
    asm.je(jit, fast_edge.clone());
    asm.test(low, Opnd::UImm(RUBY_FIXNUM_FLAG as u64));
    asm.jz(jit, slow_edge.clone());
    asm.test(high, Opnd::UImm(RUBY_FIXNUM_FLAG as u64));
    asm.jz(jit, slow_edge.clone());
    asm.jmp(fast_edge);

    asm.set_current_block(fast_block);
    let label = jit.get_label(asm, fast_block, hir_block_id);
    asm.write_label(label);
    let range = gen_new_range_fixnum(jit, asm, function, low, high, flag, state);
    asm.jmp(result_edge(range));

    asm.set_current_block(slow_block);
    let label = jit.get_label(asm, slow_block, hir_block_id);
    asm.write_label(label);
    // May call `low.<=>(high)`.
    gen_prepare_non_leaf_call(jit, asm, function, state);
    let range = asm_ccall!(asm, rb_range_new, low, high, (flag as i32).into());
    asm.jmp(result_edge(range));

    asm.set_current_block(result_block);
    let label = jit.get_label(asm, result_block, hir_block_id);
    asm.write_label(label);
    let param = asm.new_block_param(VALUE_BITS);
    asm.current_block().add_parameter(param);
    param
}


pub(super) fn gen_new_range_fixnum(
    jit: &mut JITState,
    asm: &mut Assembler,
    function:  &Function,
    low: lir::Opnd,
    high: lir::Opnd,
    flag: RangeType,
    state: &FrameState,
) -> lir::Opnd {
    let mut alloc_size = 0;
    let mut flags = VALUE(0);
    let exclude_end = matches!(flag, RangeType::Exclusive);
    unsafe {
        rb_zjit_range_new_fastpath(exclude_end, &mut alloc_size, &mut flags)
    };

    let klass = unsafe { rb_cRange };
    gc_fastpath::gc_fastpath_new_obj(jit, asm, function, state, alloc_size, flags.into(), klass,
        |asm, range| {
            asm.store(Opnd::mem(VALUE_BITS, range, RUBY_OFFSET_RSTRUCT_FIELDS_OBJ), Opnd::UImm(0));
            asm.store(Opnd::mem(VALUE_BITS, range, RUBY_OFFSET_RSTRUCT_AS_ARY), low);
            asm.store(Opnd::mem(VALUE_BITS, range, RUBY_OFFSET_RSTRUCT_AS_ARY + SIZEOF_VALUE_I32), high);
        },
        |asm| {
            gen_prepare_leaf_call_with_gc(asm, state);

            asm_ccall!(asm, rb_range_new, low, high, (flag as i64).into())
        })
}


pub(super) fn gen_object_alloc(jit: &JITState, asm: &mut Assembler, function: &Function, val: lir::Opnd, state: &FrameState) -> lir::Opnd {
    // Allocating an object from an unknown class is non-leaf; see doc for `ObjectAlloc`.
    gen_prepare_non_leaf_call(jit, asm, function, state);
    asm_ccall!(asm, rb_obj_alloc, val)
}


pub(super) fn gen_object_alloc_class(jit: &mut JITState, asm: &mut Assembler, function: &Function, class: VALUE, state: &FrameState) -> lir::Opnd {
    // Allocating an object for a known class with default allocator is leaf; see doc for
    // `ObjectAllocClass`.
    gen_prepare_leaf_call_with_gc(asm, state);
    if unsafe { rb_zjit_class_has_default_allocator(class) } {
        let mut alloc_size: usize = 0;
        let mut flags = VALUE(0);
        let has_fastpath = unsafe {
            rb_zjit_class_allocate_instance_fastpath(class, &mut alloc_size, &mut flags)
        };
        if has_fastpath {
            gc_fastpath::gc_fastpath_new_obj(jit, asm, function, state, alloc_size, flags.as_u64(), class, |_asm, _obj| {}, |asm| {
                asm_ccall!(asm, rb_class_allocate_instance, class.into())
            })
        } else {
            asm_ccall!(asm, rb_class_allocate_instance, class.into())
        }
    } else {
        assert!(class_has_leaf_allocator(class), "class passed to ObjectAllocClass must have a leaf allocator");
        let alloc_func = unsafe { rb_zjit_class_get_alloc_func(class) };
        assert!(alloc_func.is_some(), "class {} passed to ObjectAllocClass must have an allocator", get_class_name(class));
        asm_comment!(asm, "call allocator for class {}", get_class_name(class));
        asm.count_call_to(&format!("{}::allocator", get_class_name(class)));
        asm.ccall(alloc_func.unwrap() as *const u8, vec![class.into()])
    }
}
