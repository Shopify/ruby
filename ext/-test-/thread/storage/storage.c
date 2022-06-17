#include "ruby/ruby.h"
#include "ruby/thread.h"

static rb_thread_storage_key_t thread_store_key;

typedef struct thread_counter {
    unsigned int count;
} thread_counter_t;

thread_counter_t *thread_local_store(void) {
    thread_counter_t *local_store = rb_thread_storage_get(thread_store_key);

    if (!local_store) {
        local_store = ZALLOC(thread_counter_t);
        rb_thread_storage_set(thread_store_key, local_store);
    }
    return local_store;
}

static VALUE
thread_counter_get(VALUE klass)
{
    return UINT2NUM(thread_local_store()->count);
}

static VALUE
thread_counter_set(VALUE klass, VALUE value)
{
    thread_local_store()->count = NUM2UINT(value);
    return value;
}

void
Init_storage(void)
{
    rb_thread_storage_create_key(xfree); // Test creating the first key
    thread_store_key = rb_thread_storage_create_key(xfree);

    VALUE mBug = rb_define_module("Bug");
    VALUE klass = rb_define_module_under(mBug, "ThreadStorage");
    rb_define_singleton_method(klass, "counter", thread_counter_get, 0);
    rb_define_singleton_method(klass, "counter=", thread_counter_set, 1);
}
