#include "internal/gc_impl.h"

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
