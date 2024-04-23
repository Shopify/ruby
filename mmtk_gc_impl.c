#include "ruby/internal/config.h"

#if USE_MMTK
#include "internal/mmtk_gc_impl.h"

// ================== string.c ==================
// Attach a heap string `str` with a newly allocated imemo:mmtk_strbuf of a given capacity `capa`.
// The first `copy_size` bytes of the new buffer is copied from `src`, and `copy_size` must not
// exceed `capa`.
//
// `src` may point to an element of another heap object, in which case `src_obj` must point to the
// object into which `src` is pointed, and `src_obj` will be pinned during the execution of this
// function.  If `src` does not point into another heap object, `src_obj` may be 0.
void
rb_mmtk_str_new_strbuf_copy_impl(VALUE str, size_t capa, VALUE src_obj, const char *src, size_t copy_size)
{
    RUBY_ASSERT(rb_mmtk_enabled_p());

    // When using MMTk, as.heap.ptr points to the ary field of a rb_mmtk_strbuf_t
    // which is allocated in the heap as an imemo:mmtk_strbuf.
    rb_mmtk_strbuf_t *strbuf = rb_mmtk_new_strbuf(capa); // This may trigger GC, causing objects to be moved.
    char *chars = rb_mmtk_strbuf_to_chars(strbuf);

    // Note that `str` may be an existing string and `src` may point into `str` or its existing
    // buffer.  Do not modify `str` until the new strbuf is fully written.
    if (src != NULL) {
        RUBY_ASSERT(capa >= copy_size);
        memcpy(chars, src, copy_size);
    }

    RSTRING(str)->as.heap.ptr = chars;
    rb_mmtk_str_set_strbuf(str, (VALUE)strbuf);

    // Keep `src_obj` alive and pinned until the function exits.
    RB_GC_GUARD(src_obj);
}

// Attach a heap string with a newly allocated empty imemo:mmtk_strbuf.
static inline void
rb_mmtk_str_new_strbuf(VALUE str, size_t capa)
{
    rb_mmtk_str_new_strbuf_copy_impl(str, capa, 0, NULL, 0);
}

void
rb_mmtk_str_new_strbuf_impl(VALUE str, long len, int termlen)
{
    fprintf(stderr, "allocating string chunk\n");
    // Ask the GC for a chunk of memory (asking the GC for memory)
    rb_mmtk_str_new_strbuf(str, sizeof(char) * len + sizeof(char) * termlen);
}

// How large is the string allocated with rb_str_alloc_heap
static inline size_t
rb_mmtk_str_heap_size(void)
{
    // The main RString plus the stringext.
    return sizeof(struct RString) + sizeof(rb_mmtk_stringext_t);
}

size_t
rb_mmtk_string_size_impl(size_t size)
{
    if (size < rb_mmtk_str_heap_size()) {
        // When using MMTk, we always allocate enough space to hold a heap string.
        // The lowest size class for vanilla Ruby gc is 40 bytes,
        // which is enough to hold a whole `struct RString` for heap strings.
        // But we have one extra field in the trailing rb_mmtk_stringext_t.
        // So we manually ensure the allocated memory region is large enough.
        return rb_mmtk_str_heap_size();
    }
    else {
        return size;
    }
}

// Handle what the ubiquitous SIZED_REALLOC does to `as.heap.ptr`.
void
rb_mmtk_str_sized_realloc_n_impl(VALUE str, size_t new_size, size_t old_size)
{
    RUBY_ASSERT(rb_mmtk_enabled_p());

    RUBY_ASSERT(!STR_EMBED_P(str));
    // lives in string.c and is static inline int. not sure
    // what to do with this. commenting out for now.
    //RUBY_ASSERT(!str_dependent_p(str));

    size_t copy_size = old_size < new_size ? old_size : new_size;

    rb_mmtk_str_new_strbuf_copy_impl(
        str,
        new_size,
        RSTRING_EXT(str)->strbuf,
        RSTRING(str)->as.heap.ptr,
        copy_size);
    RSTRING(str)->as.heap.aux.capa = new_size;
}

VALUE
rb_mmtk_ec_str_alloc_embed_impl(struct rb_execution_context_struct *ec, VALUE klass, size_t capa)
{
    // The optimization about ec is unnecessary for MMTk.  We avoid code duplication.
    return rb_str_alloc_embed(klass, capa);
}

VALUE
rb_mmtk_ec_str_alloc_heap_impl(struct rb_execution_context_struct *ec, VALUE klass)
{
    // The optimization about ec is unnecessary for MMTk.  We avoid code duplication.
    return rb_str_alloc_heap(klass);
}

// ================== array.c ==================

VALUE *
rb_mmtk_ary_heap_alloc_impl(size_t capa)
{
    // rb_mmtk_ary_new_objbuf should be a drop-in replacement.
    // But rb_mmtk_ary_new_objbuf_copy should be used when copying/reallocating/resizing.
    rb_bug("ary_heap_alloc should not be called when using MMTk.");
    return NULL;
}

void
rb_mmtk_ary_heap_free_ptr_impl(VALUE ary, const VALUE *ptr, long size)
{
    // When using MMTk, the underlying buffer is an imemo:mmtk_objbuf which will be GC-ed.
    // We clear its objbuf field just to be safe.
    RUBY_ASSERT(rb_mmtk_enabled_p());
    RB_OBJ_WRITE(ary, &RARRAY_EXT(ary)->objbuf, 0);
    return;
}

size_t
rb_mmtk_ary_alloc_heap_size_impl(void)
{
    return sizeof(struct RArray) + sizeof(rb_mmtk_arrayext_t);
}

size_t
rb_mmtk_ary_alloc_embed_size_impl(long capa)
{
    size_t size = offsetof(struct RArray, as.ary) + (sizeof(VALUE) * capa);
    size_t rb_mmtk_ary_heap_size = sizeof(struct RArray) + sizeof(rb_mmtk_arrayext_t);

    if (size < rb_mmtk_ary_heap_size) {
        // When using MMTk, we always allocate enough space to hold a heap array.
        // The lowest size class for vanilla Ruby gc is 40 bytes,
        // which is enough to hold a whole `struct RArray` for heap arrays.
        // But we have one extra field in the trailing rb_mmtk_arrayext_t.
        // So we manually ensure the allocated memory region is large enough.
        size = rb_mmtk_ary_heap_size;
    }

    return size;
}

void
rb_mmtk_ary_set_objbuf_impl(VALUE ary, VALUE objbuf)
{
    RUBY_ASSERT(rb_mmtk_enabled_p());
    RB_OBJ_WRITE(ary, &RARRAY_EXT(ary)->objbuf, objbuf);
}

// Attach a heap array `ary` with a newly allocated imemo:mmtk_objbuf of the given capacity `capa`.
// The first `copy_len` elements of the new objbuf are copied from `src`, and `copy_len` must not
// exceed `capa`.
//
// `src` may point to an element of another heap object, in which case `src_obj` must point to the
// object into which `src` is pointed, and `src_obj` will be pinned during the execution of this
// function.  If `src` does not point into another heap object, `src_obj` may be 0.
//
// Note: capa is the number of elements in the newly created buffer.
//       copy_len is the number of elements to copy, not the number of bytes.
static inline void
rb_mmtk_ary_new_objbuf_copy_impl(VALUE ary, size_t capa, VALUE src_obj, const VALUE *src, size_t copy_len)
{
    RUBY_ASSERT(rb_mmtk_enabled_p());

    // When using MMTk, as.heap.ptr points to the ary field of a rb_mmtk_objbuf_t
    // which is allocated in the heap as an imemo:mmtk_objbuf.
    rb_mmtk_objbuf_t *objbuf = rb_mmtk_new_objbuf(capa); // This may trigger GC, causing objects to be moved.
    VALUE *elems = rb_mmtk_objbuf_to_elems(objbuf);

    // Note that `ary` may be an existing array and `src` may point into `ary` or its existing
    // buffer.  Do not modify `ary` until the new strbuf is fully written.
    if (src != NULL) {
        RUBY_ASSERT(capa >= copy_len);
        for (size_t i = 0; i < copy_len; i++) {
            // TODO: use array copy write barrier after enabling StickyImmix.
            elems[i] = src[i];
        }
    }

    RARRAY(ary)->as.heap.ptr = elems;
    rb_mmtk_ary_set_objbuf_impl(ary, (VALUE)objbuf);

    // Keep `src_obj` alive and pinned until the function exits.
    RB_GC_GUARD(src_obj);
}

void
rb_mmtk_sized_heap_realloc_impl(VALUE ary, size_t old_capa, size_t new_capa)
{
    size_t copy_len = new_capa < old_capa ? new_capa : old_capa;
    rb_mmtk_ary_new_objbuf_copy_impl(ary, new_capa, RARRAY_EXT(ary)->objbuf, RARRAY(ary)->as.heap.ptr, copy_len);
}

void
rb_mmtk_ary_new_ptr_impl(VALUE ary, size_t capa)
{
    rb_mmtk_ary_new_objbuf_copy_impl(ary, capa, 0, NULL, 0);
}

void
rb_mmtk_ary_resize_capa_new_ptr_impl(VALUE ary, size_t capa, long len)
{
    rb_mmtk_ary_new_objbuf_copy_impl(ary, capa, ary, RARRAY(ary)->as.ary, len);
    FL_UNSET_EMBED(ary);
}

void
rb_mmtk_ary_cancel_sharing_ptr_impl(VALUE ary, long len)
{
    rb_mmtk_ary_new_objbuf_copy_impl(ary, len, RARRAY_EXT(ary)->objbuf, RARRAY(ary)->as.heap.ptr, len);
}

// ================== re.c ==================

void
rb_mmtk_char_offset_realloc_impl(rb_matchext_t *rm, size_t num_regs)
{
    struct rmatch_offset **field = &rm->char_offset;
    struct rmatch_offset *old_field_value = *field;
    rb_mmtk_strbuf_t *old_strbuf = old_field_value == NULL
                                   ? NULL
                                   : rb_mmtk_chars_to_strbuf((char*)old_field_value);
    rb_mmtk_strbuf_t *new_strbuf = rb_mmtk_strbuf_realloc(old_strbuf, num_regs * sizeof(struct rmatch_offset));
    // TODO: Use write barrier.
    *field = (struct rmatch_offset*)rb_mmtk_strbuf_to_chars(new_strbuf);
}
#endif
