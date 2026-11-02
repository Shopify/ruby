#ifndef INTERNAL_ARRAY_H                                 /*-*-C-*-vi:se ft=c:*/
#define INTERNAL_ARRAY_H
/**
 * @author     Ruby developers <ruby-core@ruby-lang.org>
 * @copyright  This  file  is   a  part  of  the   programming  language  Ruby.
 *             Permission  is hereby  granted,  to  either redistribute  and/or
 *             modify this file, provided that  the conditions mentioned in the
 *             file COPYING are met.  Consult the file for details.
 * @brief      Internal header for Array.
 */
#include "ruby/internal/config.h"
#include <stddef.h>                 /* for size_t */
#include "internal/static_assert.h" /* for STATIC_ASSERT */
#include "ruby/internal/stdbool.h"         /* for bool */
#include "ruby/ruby.h"              /* for RARRAY_LEN */

#ifndef ARRAY_DEBUG
# define ARRAY_DEBUG (0+RUBY_DEBUG)
#endif

#define RARRAY_SHARED_FLAG      ELTS_SHARED
#define RARRAY_SHARED_ROOT_FLAG FL_USER12
#define RARRAY_PTR_IN_USE_FLAG  FL_USER14
#define RARRAY_FAKEARY          FL_USER19

/* The element storage as it is, in the slot or on the heap, at whatever
 * width the array uses.  For code that moves bytes around (at
 * ary_stride_bytes per element); readers go through RARRAY_AREF or
 * RARRAY_CONST_PTR, which decode or widen. */
static inline const void *
rarray_storage_ptr(VALUE ary)
{
    if (FL_TEST_RAW(ary, RARRAY_EMBED_FLAG)) {
        return (const void *)RARRAY(ary)->as.ary;
    }
    return (const void *)RARRAY(ary)->as.heap.ptr;
}

/* The packed integers of a narrow array.  Asserts the array is narrow: on a
 * wide array these bytes are VALUEs, not integers. */
static inline const void *
rarray_packed_ptr(VALUE ary)
{
    RUBY_ASSERT(rb_array_stride(ary) != RARRAY_STRIDE_VALUE);
    return rarray_storage_ptr(ary);
}

/* Element i of a packed buffer as a C integer, given its width and sign.
 * The single place that knows the encoding.  int64_t, not long: long is
 * 32-bit on LLP64 platforms. */
static inline int64_t
rarray_packed_load(const void *p, int stride, int uns, rb_len_t i)
{
    if (uns) {
        switch (stride) {
          case RARRAY_STRIDE_W8:  return ((const uint8_t  *)p)[i];
          case RARRAY_STRIDE_W16: return ((const uint16_t *)p)[i];
          default:                return ((const uint32_t *)p)[i];
        }
    }
    switch (stride) {
      case RARRAY_STRIDE_W8:  return ((const int8_t  *)p)[i];
      case RARRAY_STRIDE_W16: return ((const int16_t *)p)[i];
      default:                return ((const int32_t *)p)[i];
    }
}

/* Load element i of a narrowed array as a C integer. */
static inline int64_t
rarray_narrow_load_int(VALUE ary, rb_len_t i)
{
    return rarray_packed_load(rarray_packed_ptr(ary), rb_array_stride(ary), rb_array_stride_unsigned_p(ary), i);
}

/* Decode element i of a narrowed array into a ::VALUE. */
static inline VALUE
rarray_narrow_aref(VALUE ary, rb_len_t i)
{
    return LL2NUM(rarray_narrow_load_int(ary, i));
}

/* Core's RARRAY_AREF replaces the public macro.  It adds a bounds assertion
 * and decodes narrowed elements in place instead of widening the array, so
 * read-only paths leave a narrowed array narrow.  Never allocates. */
#undef RARRAY_AREF
RBIMPL_ATTR_ARTIFICIAL()
static inline VALUE
RARRAY_AREF(VALUE ary, rb_len_t i)
{
    RBIMPL_ASSERT_OR_ASSUME(i >= 0);
    RBIMPL_ASSERT_OR_ASSUME(i < RARRAY_LEN(ary));

    /* One test for the common case: a heap array of VALUEs.  Checking the
     * embed and stride bits separately costs a second branch per element,
     * which is measurable in tight read loops over wide arrays. */
    const VALUE flags = RBASIC(ary)->flags;
    if (RB_LIKELY((flags & (RARRAY_EMBED_FLAG | RARRAY_STRIDE_MASK)) == 0)) {
        return RARRAY(ary)->as.heap.ptr[i];
    }
    if (flags & RARRAY_STRIDE_MASK) {
        return rarray_narrow_aref(ary, i);
    }
    return RARRAY(ary)->as.ary[i];
}

/* array.c */
VALUE rb_ary_hash_values(rb_len_t len, const VALUE *elements);
VALUE rb_ary_last(int, const VALUE *, VALUE);
void rb_ary_set_len(VALUE, rb_len_t);
void rb_ary_delete_same(VALUE, VALUE);
VALUE rb_ary_hidden_new_fill(rb_len_t capa);
VALUE rb_ary_at(VALUE, VALUE);
size_t rb_ary_memsize(VALUE);
VALUE rb_to_array_type(VALUE obj);
VALUE rb_to_array(VALUE obj);
void rb_ary_cancel_sharing(VALUE ary);
/* Settle the representation of an array about to be frozen: pack an
 * all-Fixnum heap array to the narrowest width that holds every value, and
 * keep an embedded array wide.  Returns the resulting stride. */
int rb_ary_narrow(VALUE ary);
VALUE rb_ary_wide_or_dup(VALUE ary);
size_t rb_ary_size_as_embedded(VALUE ary);
void rb_ary_make_embedded(VALUE ary);
bool rb_ary_embeddable_p(VALUE ary);
bool rb_ary_embedded_shared_root_p(VALUE ary);
VALUE rb_ary_diff(VALUE ary1, VALUE ary2);
VALUE rb_ary_compact_bang(VALUE ary);
VALUE rb_ary_modify_expand(VALUE ary, rb_len_t expand);
RUBY_EXTERN VALUE rb_cArray_empty_frozen;

static inline VALUE rb_ary_entry_internal(VALUE ary, rb_len_t offset);
static inline bool ARY_PTR_USING_P(VALUE ary);

VALUE rb_ary_tmp_new_from_values(VALUE, rb_len_t, const VALUE *);
VALUE rb_check_to_array(VALUE ary);
VALUE rb_ary_behead(VALUE, rb_len_t);
VALUE rb_ary_aref1(VALUE ary, VALUE i);

struct rb_execution_context_struct;
VALUE rb_ec_ary_new_from_values(struct rb_execution_context_struct *ec, rb_len_t n, const VALUE *elts);

// YJIT needs this function to never allocate and never raise
static inline VALUE
rb_ary_entry_internal(VALUE ary, rb_len_t offset)
{
    rb_len_t len = RARRAY_LEN(ary);
    if (len == 0) return Qnil;
    if (offset < 0) {
        offset += len;
        if (offset < 0) return Qnil;
    }
    else if (len <= offset) {
        return Qnil;
    }
    return RARRAY_AREF(ary, offset); /* narrow-aware; does not allocate */
}

static inline bool
ARY_PTR_USING_P(VALUE ary)
{
    return FL_TEST_RAW(ary, RARRAY_PTR_IN_USE_FLAG);
}

RBIMPL_ATTR_MAYBE_UNUSED()
static inline int
ary_should_not_be_shared_and_embedded(VALUE ary)
{
    return !FL_ALL_RAW(ary, RARRAY_SHARED_FLAG|RARRAY_EMBED_FLAG);
}

static inline bool
ARY_SHARED_P(VALUE ary)
{
    assert(RB_TYPE_P(ary, T_ARRAY));
    assert(ary_should_not_be_shared_and_embedded(ary));
    return FL_TEST_RAW(ary, RARRAY_SHARED_FLAG);
}

static inline bool
ARY_EMBED_P(VALUE ary)
{
    assert(RB_TYPE_P(ary, T_ARRAY));
    assert(ary_should_not_be_shared_and_embedded(ary));
    return FL_TEST_RAW(ary, RARRAY_EMBED_FLAG);
}

static inline VALUE
ARY_SHARED_ROOT(VALUE ary)
{
    assert(ARY_SHARED_P(ary));
    return RARRAY(ary)->as.heap.aux.shared_root;
}

static inline bool
ARY_SHARED_ROOT_P(VALUE ary)
{
    assert(RB_TYPE_P(ary, T_ARRAY));
    return FL_TEST_RAW(ary, RARRAY_SHARED_ROOT_FLAG);
}

static inline rb_len_t
ARY_SHARED_ROOT_REFCNT(VALUE ary)
{
    assert(ARY_SHARED_ROOT_P(ary));
    return RARRAY(ary)->as.heap.aux.capa;
}

#undef rb_ary_new_from_args
#if RBIMPL_HAS_WARNING("-Wgnu-zero-variadic-macro-arguments")
# /* Skip it; clang -pedantic doesn't like the following */
#elif defined(__GNUC__) && defined(HAVE_VA_ARGS_MACRO)
#define rb_ary_new_from_args(n, ...) \
    __extension__ ({ \
        const VALUE args_to_new_ary[] = {__VA_ARGS__}; \
        if (__builtin_constant_p(n)) { \
            STATIC_ASSERT(rb_ary_new_from_args, numberof(args_to_new_ary) == (n)); \
        } \
        rb_ary_new_from_values(numberof(args_to_new_ary), args_to_new_ary); \
    })
#endif


#endif /* INTERNAL_ARRAY_H */
