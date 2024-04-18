#ifndef INTERNAL_GC_IMPL_H
#define INTERNAL_GC_IMPL_H
#include <stdio.h>
#include "ruby/internal/config.h"
#include "ruby/internal/value.h"
#include "ruby/internal/memory.h"
#include "internal/string.h"
#include "internal/gc.h"

void rb_gc_str_new_strbuf_copy_impl(VALUE dest, size_t capa, VALUE should_copy, const char *src, size_t copy_size);
size_t rb_gc_string_size_impl(size_t capa);
void rb_gc_str_new_strbuf_impl(VALUE str, long len, int termlen);
void rb_gc_str_sized_realloc_n_impl(VALUE str, size_t new_size, size_t old_size);
VALUE rb_gc_ec_str_alloc_embed_impl(struct rb_execution_context_struct *ec, VALUE klass, size_t capa);
VALUE rb_gc_ec_str_alloc_heap_impl(struct rb_execution_context_struct *ec, VALUE klass);
#endif
