#ifndef MMTK_GC_H
#define MMTK_GC_H
#include "internal/cmdlineopt.h"
#include "internal/mmtk.h"
#include "internal/mmtk_support.h"
#include "internal/string.h"

void rb_mmtk_str_new_strbuf_copy_impl(VALUE str, size_t capa, VALUE src_obj, const char *src, size_t copy_size);
void rb_gc_str_new_strbuf_impl(VALUE str, long len, int termlen);
void rb_mmtk_str_new_strbuf_impl(VALUE str, long len, int termlen);
size_t rb_mmtk_string_size_impl(size_t size);
void rb_mmtk_str_sized_realloc_n_impl(VALUE str, size_t new_size, size_t old_size);
VALUE rb_mmtk_ec_str_alloc_embed_impl(struct rb_execution_context_struct *ec, VALUE klass, size_t capa);

#endif
