#ifndef RUBY_JIT_NATIVE_H
#define RUBY_JIT_NATIVE_H 1

/**
 * Native call hints for JIT optimization of FFI-style calls.
 *
 * C extensions that bind native functions (e.g. the FFI gem, Fiddle) can
 * register hints telling the JIT compiler how to call through to the actual
 * native function directly, bypassing the extension's generic dispatch layer
 * and libffi.
 *
 * Usage from a C extension:
 *
 *   void *func = dlsym(handle, "my_func");
 *   // my_func(int, const char*) -> int
 *   int arg_types[] = { RB_JIT_NATIVE_INT, RB_JIT_NATIVE_STRING };
 *   rb_jit_hint_native_call(klass, rb_intern("my_func"), func,
 *                           2, arg_types, RB_JIT_NATIVE_INT);
 */

#include "ruby/ruby.h"

RUBY_SYMBOL_EXPORT_BEGIN

enum rb_jit_native_type {
    RB_JIT_NATIVE_VOID = 0,
    RB_JIT_NATIVE_BOOL,
    RB_JIT_NATIVE_INT,       /* C int */
    RB_JIT_NATIVE_UINT,      /* C unsigned int */
    RB_JIT_NATIVE_LONG,      /* C long */
    RB_JIT_NATIVE_ULONG,     /* C unsigned long */
    RB_JIT_NATIVE_INT64,     /* int64_t / long long */
    RB_JIT_NATIVE_UINT64,    /* uint64_t / unsigned long long */
    RB_JIT_NATIVE_FLOAT,     /* C float */
    RB_JIT_NATIVE_DOUBLE,    /* C double */
    RB_JIT_NATIVE_POINTER,   /* void * */
    RB_JIT_NATIVE_STRING,    /* const char * (null-terminated) */
};

#define RB_JIT_NATIVE_MAX_ARGS 6

typedef struct rb_jit_native_call_hint {
    void *func_ptr;
    int argc;
    int arg_types[RB_JIT_NATIVE_MAX_ARGS]; /* enum rb_jit_native_type */
    int ret_type;                          /* enum rb_jit_native_type */
} rb_jit_native_call_hint_t;

/**
 * Register a native call hint for a method.
 *
 * Tells the JIT that +mid+ on +klass+ is a thin wrapper around +func_ptr+
 * with the given signature.  When the JIT compiles a call to this method it
 * may emit a direct native call instead of going through the extension's
 * dispatch function and libffi.
 *
 * @param klass     The class or module the method is defined on.
 *                  For singleton methods use rb_singleton_class(obj).
 * @param mid       The method ID (rb_intern("name")).
 * @param func_ptr  The native function pointer (e.g. from dlsym).
 * @param argc      Number of arguments (0..RB_JIT_NATIVE_MAX_ARGS).
 * @param arg_types Array of argc rb_jit_native_type values.
 * @param ret_type  Return type (rb_jit_native_type).
 */
void rb_jit_hint_native_call(VALUE klass, ID mid, void *func_ptr,
                              int argc, const int *arg_types, int ret_type);

/**
 * Look up a previously registered native call hint.
 *
 * @param klass  The class/module to search.
 * @param mid    The method ID.
 * @return       Pointer to the hint, or NULL if none registered.
 *               The returned pointer is valid until the hint is removed.
 */
const rb_jit_native_call_hint_t *
rb_jit_find_native_call_hint(VALUE klass, ID mid);

RUBY_SYMBOL_EXPORT_END

#endif /* RUBY_JIT_NATIVE_H */
