#include "ruby/ruby.h"
#include "ruby/atomic.h"
#include "ruby/thread.h"
#include "ruby/thread_native.h"

static rb_atomic_t acquire_enter_count = 0;
static rb_atomic_t acquire_exit_count = 0;
static rb_atomic_t release_count = 0;

void
ex_callback(rb_event_flag_t event, gvl_hook_event_args_t args)
{
    switch(event) {
      case RUBY_INTERNAL_EVENT_GVL_ACQUIRE_ENTER:
        RUBY_ATOMIC_INC(acquire_enter_count);
        break;
      case RUBY_INTERNAL_EVENT_GVL_ACQUIRE_EXIT:
        RUBY_ATOMIC_INC(acquire_exit_count);
        break;
      case RUBY_INTERNAL_EVENT_GVL_RELEASE:
        RUBY_ATOMIC_INC(release_count);
        break;
    }
}

static gvl_hook_t * single_hook = NULL;

static VALUE
thread_counters(VALUE thread)
{
    VALUE array = rb_ary_new2(3);
    rb_ary_push(array, UINT2NUM(acquire_enter_count));
    rb_ary_push(array, UINT2NUM(acquire_exit_count));
    rb_ary_push(array, UINT2NUM(release_count));
    return array;
}

static VALUE
thread_reset_counters(VALUE thread)
{
    RUBY_ATOMIC_SET(acquire_enter_count, 0);
    RUBY_ATOMIC_SET(acquire_exit_count, 0);
    RUBY_ATOMIC_SET(release_count, 0);
    return Qtrue;
}

static VALUE
thread_register_gvl_callback(VALUE thread)
{
    single_hook = rb_gvl_event_new(
        *ex_callback,
        RUBY_INTERNAL_EVENT_GVL_ACQUIRE_ENTER | RUBY_INTERNAL_EVENT_GVL_ACQUIRE_EXIT | RUBY_INTERNAL_EVENT_GVL_RELEASE
    );

    return Qnil;
}

static VALUE
thread_unregister_gvl_callback(VALUE thread)
{
    if (single_hook) {
        rb_gvl_event_delete(single_hook);
        single_hook = NULL;
    }

    return Qnil;
}

static VALUE
thread_register_and_unregister_gvl_callback(VALUE thread)
{
    gvl_hook_t * hooks[5];
    for (int i = 0; i < 5; i++) {
        hooks[i] = rb_gvl_event_new(*ex_callback, RUBY_INTERNAL_EVENT_GVL_ACQUIRE_ENTER);
    }

    if (!rb_gvl_event_delete(hooks[4])) return Qfalse;
    if (!rb_gvl_event_delete(hooks[0])) return Qfalse;
    if (!rb_gvl_event_delete(hooks[3])) return Qfalse;
    if (!rb_gvl_event_delete(hooks[2])) return Qfalse;
    if (!rb_gvl_event_delete(hooks[1])) return Qfalse;
    return Qtrue;
}

void
Init_instrumentation(void)
{
    VALUE mBug = rb_define_module("Bug");
    VALUE klass = rb_define_module_under(mBug, "GVLInstrumentation");
    rb_define_singleton_method(klass, "counters", thread_counters, 0);
    rb_define_singleton_method(klass, "reset_counters", thread_reset_counters, 0);
    rb_define_singleton_method(klass, "register_callback", thread_register_gvl_callback, 0);
    rb_define_singleton_method(klass, "unregister_callback", thread_unregister_gvl_callback, 0);
    rb_define_singleton_method(klass, "register_and_unregister_callbacks", thread_register_and_unregister_gvl_callback, 0);
}
