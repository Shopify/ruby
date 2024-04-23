#ifndef MMTK_GC_H
#define MMTK_GC_H
#include "internal/cmdlineopt.h"
#include "internal/mmtk.h"
#include "internal/mmtk_support.h"
#include "internal/string.h"

// ================== string.c ==================
void rb_mmtk_str_new_strbuf_copy_impl(VALUE str, size_t capa, VALUE src_obj, const char *src, size_t copy_size);
void rb_gc_str_new_strbuf_impl(VALUE str, long len, int termlen);
void rb_mmtk_str_new_strbuf_impl(VALUE str, long len, int termlen);
size_t rb_mmtk_string_size_impl(size_t size);
void rb_mmtk_str_sized_realloc_n_impl(VALUE str, size_t new_size, size_t old_size);
VALUE rb_mmtk_ec_str_alloc_embed_impl(struct rb_execution_context_struct *ec, VALUE klass, size_t capa);
VALUE rb_mmtk_ec_str_alloc_heap_impl(struct rb_execution_context_struct *ec, VALUE klass);
// ================== array.c ==================
VALUE * rb_mmtk_ary_heap_alloc_impl(size_t capa);
void rb_mmtk_ary_heap_free_ptr_impl(VALUE ary, const VALUE *ptr, long size);
size_t rb_mmtk_ary_alloc_heap_size_impl(void);
size_t rb_mmtk_ary_alloc_embed_size_impl(long capa);
void rb_mmtk_sized_heap_realloc_impl(VALUE ary, size_t old_capa, size_t new_capa);
void rb_mmtk_ary_new_ptr_impl(VALUE ary, size_t capa);
void rb_mmtk_ary_resize_capa_new_ptr_impl(VALUE ary, size_t capa, long len);
// ================== re.c ==================
void rb_mmtk_char_offset_realloc_impl(rb_matchext_t *rm, size_t num_regs);
#endif
