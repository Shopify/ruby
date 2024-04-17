#include "ruby/internal/config.h"

#if USE_MMTK
#include "internal/mmtk_gc_impl.h"

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

void rb_gc_str_new_strbuf_impl(VALUE str, long len, int termlen);

void
rb_mmtk_str_new_strbuf_impl(VALUE str, long len, int termlen)
{
    fprintf(stderr, "allocating string chunk\n");
    // Ask the GC for a chunk of memory (asking the GC for memory)
    rb_mmtk_str_new_strbuf(str, sizeof(char) * len + sizeof(char) * termlen);
}

// How large is the string allocated with str_alloc_heap
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
#endif
