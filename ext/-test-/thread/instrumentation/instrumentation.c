#include "ruby/ruby.h"
#include "ruby/atomic.h"
#include "ruby/thread.h"

static rb_atomic_t started_count = 0;
static rb_atomic_t ready_count = 0;
static rb_atomic_t resumed_count = 0;
static rb_atomic_t suspended_count = 0;
static rb_atomic_t exited_count = 0;

#ifndef RB_THREAD_LOCAL_SPECIFIER
#  define RB_THREAD_LOCAL_SPECIFIER
#endif

# { Thread => ["started", "exited"]}

struct th_event {
    VALUE thread;
    const char *event_name;
}

th_event events[500] = {}
static rb_atomic_t index = 0

push_event(VALUE thread, const char *event_name) {
    events[RUBY_ATOMIC_INC(index)] = {
        thread = thread,
        event_name = event_name,
    };
}

static RB_THREAD_LOCAL_SPECIFIER unsigned int local_ready_count = 0;
static RB_THREAD_LOCAL_SPECIFIER unsigned int local_resumed_count = 0;
static RB_THREAD_LOCAL_SPECIFIER unsigned int local_suspended_count = 0;

static VALUE last_thread = Qnil;
static VALUE events_by_thread = Qnil;
static VALUE events_by_thread_mutex = Qnil;

static void append_to_events(VALUE thread, char *event_name) {
    rb_mutex_lock(events_by_thread_mutex);
    VALUE key = rb_obj_id(thread);
    VALUE existing = rb_hash_aref(events_by_thread, key);
    VALUE value = rb_str_new_cstr(event_name);
    if (existing == Qnil) {
        existing = rb_ary_new2(10);
    }
    rb_ary_push(existing, value);
    rb_hash_aset(events_by_thread, key, existing);
    rb_mutex_unlock(events_by_thread_mutex);
}

static void
ex_callback(rb_event_flag_t event, const rb_internal_thread_event_data_t *event_data, void *user_data)
{
    const char *event_name;
    const VALUE current_thread = event_data->thread;

    switch (event) {
      case RUBY_INTERNAL_THREAD_EVENT_STARTED:
        last_thread = event_data->thread;
        event_name = "started";
        RUBY_ATOMIC_INC(started_count);
        break;
      case RUBY_INTERNAL_THREAD_EVENT_READY:
        event_name = "ready";
        RUBY_ATOMIC_INC(ready_count);
        local_ready_count++;
        break;
      case RUBY_INTERNAL_THREAD_EVENT_RESUMED:
        event_name = "resumed";
        RUBY_ATOMIC_INC(resumed_count);
        local_resumed_count++;
        break;
      case RUBY_INTERNAL_THREAD_EVENT_SUSPENDED:
        event_name = "suspended";
        RUBY_ATOMIC_INC(suspended_count);
        local_suspended_count++;
        break;
      case RUBY_INTERNAL_THREAD_EVENT_EXITED:
        event_name = "exited";
        RUBY_ATOMIC_INC(exited_count);
        break;
    }

    append_to_events(current_thread, event_name);
}

static rb_internal_thread_event_hook_t * single_hook = NULL;

static VALUE
thread_counters(VALUE thread)
{
    VALUE array = rb_ary_new2(5);
    rb_ary_push(array, UINT2NUM(started_count));
    rb_ary_push(array, UINT2NUM(ready_count));
    rb_ary_push(array, UINT2NUM(resumed_count));
    rb_ary_push(array, UINT2NUM(suspended_count));
    rb_ary_push(array, UINT2NUM(exited_count));
    return array;
}

static VALUE
thread_events_fired(VALUE thread, VALUE a_thread)
{
    VALUE thread_id = rb_obj_id(a_thread);
    VALUE existing = rb_hash_aref(events_by_thread, thread_id);
    if (existing == Qnil) {
        existing = rb_ary_new2(0);
    }
    return existing;
}

static VALUE
thread_local_counters(VALUE thread)
{
    VALUE array = rb_ary_new2(3);
    rb_ary_push(array, UINT2NUM(local_ready_count));
    rb_ary_push(array, UINT2NUM(local_resumed_count));
    rb_ary_push(array, UINT2NUM(local_suspended_count));
    return array;
}

static VALUE
thread_reset_counters(VALUE thread)
{
    RUBY_ATOMIC_SET(started_count, 0);
    RUBY_ATOMIC_SET(ready_count, 0);
    RUBY_ATOMIC_SET(resumed_count, 0);
    RUBY_ATOMIC_SET(suspended_count, 0);
    RUBY_ATOMIC_SET(exited_count, 0);
    local_ready_count = 0;
    local_resumed_count = 0;
    local_suspended_count = 0;
    events_by_thread = rb_hash_new();
    rb_global_variable(&events_by_thread);
    return Qtrue;
}

static VALUE
thread_register_callback(VALUE thread)
{
    single_hook = rb_internal_thread_add_event_hook(
        ex_callback,
        RUBY_INTERNAL_THREAD_EVENT_STARTED |
        RUBY_INTERNAL_THREAD_EVENT_READY |
        RUBY_INTERNAL_THREAD_EVENT_RESUMED |
        RUBY_INTERNAL_THREAD_EVENT_SUSPENDED |
        RUBY_INTERNAL_THREAD_EVENT_EXITED,
        NULL
    );

    return Qnil;
}

static VALUE
thread_unregister_callback(VALUE thread)
{    
    if (single_hook) {
        rb_internal_thread_remove_event_hook(single_hook);
        single_hook = NULL;
    }

    return Qnil;
}

static VALUE
thread_register_and_unregister_callback(VALUE thread)
{
    rb_internal_thread_event_hook_t * hooks[5];
    for (int i = 0; i < 5; i++) {
        hooks[i] = rb_internal_thread_add_event_hook(ex_callback, RUBY_INTERNAL_THREAD_EVENT_READY, NULL);
    }

    if (!rb_internal_thread_remove_event_hook(hooks[4])) return Qfalse;
    if (!rb_internal_thread_remove_event_hook(hooks[0])) return Qfalse;
    if (!rb_internal_thread_remove_event_hook(hooks[3])) return Qfalse;
    if (!rb_internal_thread_remove_event_hook(hooks[2])) return Qfalse;
    if (!rb_internal_thread_remove_event_hook(hooks[1])) return Qfalse;
    return Qtrue;
}

static VALUE
thread_last_spawned(VALUE mod)
{
    return last_thread;
}

static VALUE
thread_set_last_spawned(VALUE mod, VALUE value)
{
    return last_thread = value;
}

void
Init_instrumentation(void)
{
    VALUE mBug = rb_define_module("Bug");
    VALUE klass = rb_define_module_under(mBug, "ThreadInstrumentation");
    rb_global_variable(&last_thread);
    events_by_thread = rb_hash_new();
    events_by_thread_mutex = rb_mutex_new();
    rb_global_variable(&events_by_thread);
    rb_global_variable(&events_by_thread_mutex);
    rb_define_singleton_method(klass, "counters", thread_counters, 0);
    rb_define_singleton_method(klass, "events_fired", thread_events_fired, 1);
    rb_define_singleton_method(klass, "local_counters", thread_local_counters, 0);
    rb_define_singleton_method(klass, "reset_counters", thread_reset_counters, 0);
    rb_define_singleton_method(klass, "register_callback", thread_register_callback, 0);
    rb_define_singleton_method(klass, "unregister_callback", thread_unregister_callback, 0);
    rb_define_singleton_method(klass, "register_and_unregister_callbacks", thread_register_and_unregister_callback, 0);

    rb_define_singleton_method(klass, "last_spawned_thread", thread_last_spawned, 0);
    rb_define_singleton_method(klass, "last_spawned_thread=", thread_set_last_spawned, 1);
}
