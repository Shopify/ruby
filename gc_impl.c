#include "internal/gc_impl.h"

void
rb_gc_str_new_strbuf_copy_impl(VALUE dest, size_t capa, void * should_copy, const char *src, size_t copy_size)
{
    char *new_ptr;

    if (should_copy) {
        new_ptr = ALLOC_N(char, capa);

        if (src) {
            memcpy(new_ptr, src, copy_size);
        }

        if (rb_str_freeable_buffer(dest)) {
            xfree(src);
        }
    } else {
        new_ptr = src;
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
    RSTRING(str)->as.heap.ptr =
        rb_xmalloc_mul_add_mul(sizeof(char), len, sizeof(char), termlen);

}
