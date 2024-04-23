#ifndef INTERNAL_GC_IMPL_H
#define INTERNAL_GC_IMPL_H
#include <stdio.h>
#include "ruby/internal/config.h"
#include "ruby/internal/value.h"
#include "ruby/internal/memory.h"
#include "internal/string.h"
#include "internal/gc.h"
#include "ruby/internal/core/rmatch.h"

// ================== string.c ==================
void rb_gc_str_new_strbuf_copy_impl(VALUE dest, size_t capa, VALUE should_copy, const char *src, size_t copy_size);
size_t rb_gc_string_size_impl(size_t capa);
void rb_gc_str_new_strbuf_impl(VALUE str, long len, int termlen);
void rb_gc_str_sized_realloc_n_impl(VALUE str, size_t new_size, size_t old_size);
VALUE rb_gc_ec_str_alloc_embed_impl(struct rb_execution_context_struct *ec, VALUE klass, size_t capa);
VALUE rb_gc_ec_str_alloc_heap_impl(struct rb_execution_context_struct *ec, VALUE klass);
// ================== array.c ==================
VALUE * rb_gc_ary_heap_alloc_impl(size_t capa);
void rb_gc_ary_heap_free_ptr_impl(VALUE ary, const VALUE *ptr, long size);
size_t rb_gc_ary_alloc_heap_size_impl(void);
size_t rb_gc_ary_alloc_embed_size_impl(long capa);
void rb_gc_sized_heap_realloc_impl(VALUE ary, size_t old_capa, size_t new_capa);
void rb_gc_ary_new_ptr_impl(VALUE ary, size_t capa);
void rb_gc_ary_resize_capa_new_ptr_impl(VALUE ary, size_t capa, long len);
void rb_gc_ary_cancel_sharing_ptr_impl(VALUE ary, long len);
// ================== re.c ==================
void rb_gc_char_offset_realloc_impl(rb_matchext_t *rm, size_t num_regs);
#endif
