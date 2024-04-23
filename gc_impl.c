#include "internal/gc_impl.h"

// ================== string.c ==================
void
rb_gc_str_new_strbuf_copy_impl(VALUE dest, size_t capa, VALUE should_copy, const char *src, size_t copy_size)
{
    char *new_ptr;

    if (should_copy) {
        new_ptr = ALLOC_N(char, capa);

        if (src) {
            memcpy(new_ptr, src, copy_size);
        }

        if (rb_str_freeable_buffer(dest)) {
            xfree((void *)src);
        }
    } else {
        new_ptr = (char *)src;
    }

    RSTRING(dest)->as.heap.ptr = new_ptr;
}

size_t
rb_gc_string_size_impl(size_t capa)
{
    return capa;
}

void
rb_gc_str_new_strbuf_impl(VALUE str, long len, int termlen)
{
    RSTRING(str)->as.heap.ptr = rb_xmalloc_mul_add_mul(sizeof(char), len, sizeof(char), termlen);
}

void
rb_gc_str_sized_realloc_n_impl(VALUE str, size_t new_size, size_t old_size)
{
    SIZED_REALLOC_N(RSTRING(str)->as.heap.ptr, char, new_size, old_size);
}

VALUE
rb_gc_ec_str_alloc_embed_impl(struct rb_execution_context_struct *ec, VALUE klass, size_t capa)
{
    size_t size = rb_str_embed_size(capa);
    RUBY_ASSERT(size > 0);
    RUBY_ASSERT(rb_gc_size_allocatable_p(size));

    NEWOBJ_OF(str, struct RString, klass,
            T_STRING | (RGENGC_WB_PROTECTED_STRING ? FL_WB_PROTECTED : 0), size, ec);

    return (VALUE)str;
}

VALUE
rb_gc_ec_str_alloc_heap_impl(struct rb_execution_context_struct *ec, VALUE klass)
{
    NEWOBJ_OF(str, struct RString, klass,
            T_STRING | STR_NOEMBED | (RGENGC_WB_PROTECTED_STRING ? FL_WB_PROTECTED : 0), sizeof(struct RString), ec);

    return (VALUE)str;
}

// ================== array.c ==================

VALUE *
rb_gc_ary_heap_alloc_impl(size_t capa)
{
    return ALLOC_N(VALUE, capa);
}

void
rb_gc_ary_heap_free_ptr_impl(VALUE ary, const VALUE *ptr, long size)
{
    ruby_sized_xfree((void *)ptr, size);
}

size_t
rb_gc_ary_alloc_heap_size_impl(void)
{
    return sizeof(struct RString);
}

size_t
rb_gc_ary_alloc_embed_size_impl(long capa)
{
    return offsetof(struct RArray, as.ary) + (sizeof(VALUE) * capa);
}

void
rb_gc_sized_heap_realloc_impl(VALUE ary, size_t old_capa, size_t new_capa)
{
    SIZED_REALLOC_N(RARRAY(ary)->as.heap.ptr, VALUE, new_capa, old_capa);
}

void
rb_gc_ary_new_ptr_impl(VALUE ary, size_t capa)
{
    // TODO: copied from ARY_SET_PTR macro
    VALUE * capa_ptr = rb_gc_ary_heap_alloc(capa);
    RUBY_ASSERT(!ARY_EMBED_P(ary));
    RUBY_ASSERT(!OBJ_FROZEN(ary));
    RARRAY(ary)->as.heap.ptr = capa_ptr;
}

void rb_gc_ary_resize_capa_new_ptr_impl(VALUE ary, size_t capa, long len)
{
    // TODO: this is really similar to the above function
    // but needs a memcopy nad FL unset embed...
    VALUE * capa_ptr = rb_gc_ary_heap_alloc(capa);

    // TODO: copied from ARY_EMBED_PTR macro
    RUBY_ASSERT(ARY_EMBED_P(ary));
    MEMCPY(capa_ptr, RARRAY(ary)->as.ary, VALUE, len);

    FL_UNSET_EMBED(ary);

    RUBY_ASSERT(!ARY_EMBED_P(ary));
    RUBY_ASSERT(!OBJ_FROZEN(ary));
    RARRAY(ary)->as.heap.ptr = capa_ptr;
}

void
rb_gc_ary_cancel_sharing_ptr_impl(VALUE ary, long len)
{
    // TODO: this is really similar to the above function
    // but doesn't use an FL_UNSET EMBED
    // also it uses len only, not capa and len
    VALUE * capa_ptr = rb_gc_ary_heap_alloc(len);

    // TODO: copied from ARY_EMBED_PTR macro
    RUBY_ASSERT(ARY_EMBED_P(ary));
    MEMCPY(capa_ptr, RARRAY(ary)->as.ary, VALUE, len);

    RUBY_ASSERT(!ARY_EMBED_P(ary));
    RUBY_ASSERT(!OBJ_FROZEN(ary));
    RARRAY(ary)->as.heap.ptr = capa_ptr;
}

void
rb_gc_ary_make_shared_ptr_impl(VALUE ary, VALUE shared, size_t capa, long len)
{
    VALUE *ptr = rb_gc_ary_heap_alloc(capa);

    RUBY_ASSERT(!ARY_EMBED_P(shared));
    RUBY_ASSERT(!OBJ_FROZEN(shared));
    RARRAY(shared)->as.heap.ptr = ptr;

    ary_memcpy(shared, 0, len, RARRAY_CONST_PTR(ary));

    FL_UNSET_EMBED(ary);

    RUBY_ASSERT(!ARY_EMBED_P(ary));
    RUBY_ASSERT(!OBJ_FROZEN(ary));
    RARRAY(ary)->as.heap.ptr = ptr;
}

void
rb_gc_ary_replace_ptr_impl(VALUE copy, VALUE orig, long len)
{
    VALUE *ptr = rb_gc_ary_heap_alloc(len);

    FL_UNSET_EMBED(copy);
    ARY_SET_PTR(copy, ptr);
    ARY_SET_LEN(copy, len);
    ARY_SET_CAPA(copy, len);

    // No allocation and exception expected that could leave `copy` in a
    // bad state from the edits above.
    ary_memcpy(copy, 0, len, RARRAY_CONST_PTR(orig));
}

// ================== re.c ==================

void
rb_gc_char_offset_realloc_impl(rb_matchext_t *rm, size_t num_regs)
{
    REALLOC_N(rm->char_offset, struct rmatch_offset, num_regs);
}

