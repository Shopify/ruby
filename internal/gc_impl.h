#ifndef INTERNAL_GC_IMPL_H
#define INTERNAL_GC_IMPL_H
#include <stdio.h>
#include "ruby/internal/config.h"
#include "ruby/internal/value.h"
#include "ruby/internal/memory.h"
#include "internal/string.h"
#include "internal/gc.h"

void rb_gc_str_new_strbuf_copy_impl(VALUE dest, size_t capa, void * should_copy, const char *src, size_t copy_size);
size_t rb_gc_string_size_impl(size_t capa);
void rb_gc_str_new_strbuf_impl(VALUE str, long len, int termlen);
#endif
