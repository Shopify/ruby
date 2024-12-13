#include <sys/mman.h>
#include <unistd.h>

#include "ruby/atomic.h"
#include "ruby/debug.h"
#include "internal/gc.h"
#include "ccan/list/list.h"
#include "darray.h"
#include "internal/sanitizers.h"

/*===== FORWARD DECLARATIONS FROM gc.c */

unsigned int rb_gc_vm_lock(void);
void         rb_gc_vm_unlock(unsigned int lev);
unsigned int rb_gc_cr_lock(void);
void         rb_gc_cr_unlock(unsigned int lev);
size_t       rb_size_mul_or_raise(size_t x, size_t y, VALUE exc);
void         rb_gc_run_obj_finalizer(VALUE objid, long count, VALUE (*callback)(long i, void *data), void *data);
void         rb_gc_set_pending_interrupt(void);
void         rb_gc_unset_pending_interrupt(void);
bool         rb_gc_obj_free(void *objspace, VALUE obj);
const char * rb_obj_info(VALUE obj);
bool         rb_gc_shutdown_call_finalizer_p(VALUE obj);

VALUE        rb_gc_impl_object_id(void *objspace_ptr, VALUE obj);

#ifdef HAVE_MALLOC_USABLE_SIZE
# include <malloc.h>
# define malloc_size(ptr) malloc_usable_size(ptr)
#else
# include <malloc/malloc.h>
#endif

#define GC_ASSERT RUBY_ASSERT

#define MAX(a, b) ((a) > (b) ? (a) : (b))

#ifndef HEAP_ALIGN_LOG
/* default tiny heap size: 64KiB */
#define HEAP_ALIGN_LOG 16
#endif

enum {
    HEAP_ALIGN = (1UL << HEAP_ALIGN_LOG),
    HEAP_ALIGN_MASK = (~(~0UL << HEAP_ALIGN_LOG)),
    HEAP_SIZE = HEAP_ALIGN,
};

// TODO Reduce the struct to its strict minimum
typedef struct rb_objspace {
    size_t allocated;
    size_t used;
    size_t increment;
    VALUE free_object;
    void * root;
} rb_objspace_t;

static inline void *
calloc1(size_t n)
{
    return calloc(1, n);
}

static void *
gc_aligned_malloc(size_t alignment, size_t size)
{
    /* alignment must be a power of 2 */
    GC_ASSERT(((alignment - 1) & alignment) == 0);
    GC_ASSERT(alignment % sizeof(void*) == 0);

    void *res;

#if defined(HAVE_POSIX_MEMALIGN)
    if (posix_memalign(&res, alignment, size) != 0) {
        return NULL;
    }
#elif defined(HAVE_MEMALIGN)
    res = memalign(alignment, size);
#else
    char* aligned;
    res = malloc(alignment + size + sizeof(void*));
    aligned = (char*)res + alignment + sizeof(void*);
    aligned -= ((VALUE)aligned & (alignment - 1));
    ((void**)aligned)[-1] = res;
    res = (void*)aligned;
#endif

    GC_ASSERT((uintptr_t)res % alignment == 0);

    return res;
}

static void *
gc_aligned_realloc(void * root, size_t alignment, size_t new_size)
{
    /* alignment must be a power of 2 */
    GC_ASSERT(((alignment - 1) & alignment) == 0);
    GC_ASSERT(alignment % sizeof(void*) == 0);

    void *res;

    res = realloc(root, new_size);

    GC_ASSERT((uintptr_t)res % alignment == 0);
    GC_ASSERT(res == root);

    return res;
}

const char *
rb_gc_impl_active_gc_name(void)
{
    return "noop";
}

// Bootup
void *
rb_gc_impl_objspace_alloc(void)
{
    rb_objspace_t *objspace = calloc1(sizeof(rb_objspace_t));
    return objspace;
}

void
rb_gc_impl_objspace_init(void *objspace_ptr)
{
    rb_objspace_t *objspace = objspace_ptr;

    objspace->increment = HEAP_SIZE;
    objspace->root = gc_aligned_malloc(HEAP_ALIGN, objspace->increment);
    objspace->free_object = (VALUE)objspace->root;
    objspace->allocated = HEAP_SIZE;
}

void rb_gc_impl_objspace_free(void *objspace_ptr)                         { /* nop */ }
void * rb_gc_impl_ractor_cache_alloc(void *objspace)                      { return NULL; }
void rb_gc_impl_ractor_cache_free(void *objspace_ptr, void *cache)        { /* nop */ }
void rb_gc_impl_set_params(void *objspace_ptr)                            { /* nop */ }

void
rb_gc_impl_init(void)
{
    VALUE gc_constants = rb_hash_new();
    rb_hash_aset(gc_constants, ID2SYM(rb_intern("HEAP_SIZE")), SIZET2NUM(HEAP_SIZE));
    OBJ_FREEZE(gc_constants);
    /* Internal constants in the garbage collector. */
    rb_define_const(rb_mGC, "INTERNAL_CONSTANTS", gc_constants);

    /* internal methods */
    rb_define_singleton_method(rb_mGC, "verify_internal_consistency", rb_f_notimplement, 0);

    VALUE rb_mProfiler = rb_define_module_under(rb_mGC, "Profiler");
    rb_define_singleton_method(rb_mProfiler, "enabled?", rb_f_notimplement, 0);
    rb_define_singleton_method(rb_mProfiler, "enable", rb_f_notimplement, 0);
    rb_define_singleton_method(rb_mProfiler, "raw_data", rb_f_notimplement, 0);
    rb_define_singleton_method(rb_mProfiler, "disable", rb_f_notimplement, 0);
    rb_define_singleton_method(rb_mProfiler, "clear", rb_f_notimplement, 0);
    rb_define_singleton_method(rb_mProfiler, "result", rb_f_notimplement, 0);
    rb_define_singleton_method(rb_mProfiler, "report", rb_f_notimplement, -1);
    rb_define_singleton_method(rb_mProfiler, "total_time", rb_f_notimplement, 0);

    {
        VALUE opts;
        rb_define_const(rb_mGC, "OPTS", opts = rb_ary_new());
        OBJ_FREEZE(opts);
    }
}

static size_t heap_sizes[1] = {
    0
};

size_t *
rb_gc_impl_heap_sizes(void *objspace_ptr)
{
    return heap_sizes;
}

size_t available_allocated_unused_memory(void *objspace_ptr) {
    rb_objspace_t *objspace = objspace_ptr;

    return objspace->allocated - objspace->used;
}

// Shutdown
void rb_gc_impl_shutdown_free_objects(void *objspace_ptr) {
    rb_objspace_t *objspace = objspace_ptr;

    ruby_sized_xfree((void *)objspace->root, objspace->allocated);
}

// GC
void rb_gc_impl_start(void *o, bool f, bool im, bool is, bool c)          { /* noop */ }
bool rb_gc_impl_during_gc_p(void *objspace)                               { return FALSE; }
void rb_gc_impl_prepare_heap(void *objspace_ptr)                          { /* nop */ }
bool rb_gc_impl_gc_enabled_p(void *objspace)                              { return FALSE; }
void rb_gc_impl_gc_enable(void *objspace_ptr)                             { /* nop */ }
void rb_gc_impl_gc_disable(void *objspace_ptr, bool finish_current_gc)    { /* nop */ }
void rb_gc_impl_stress_set(void *objspace_ptr, VALUE flag)                { /* nop */ }
VALUE rb_gc_impl_stress_get(void *objspace)                               { return Qfalse; }
VALUE rb_gc_impl_config_get(void *objspace_ptr)                           { return Qnil; }
void rb_gc_impl_config_set(void *objspace_ptr, VALUE hash)                { /* nop */ }

// Object allocation
size_t round_up_to_alignment(size_t v1, size_t alignment) {
    return (v1 + alignment) & ~alignment;
}

void grow_heap_by(rb_objspace_t *objspace, size_t additional_size) {
    size_t grow_factor = MAX(objspace->increment, additional_size);
    size_t new_size = objspace->allocated + grow_factor;
    size_t aligned_new_size = round_up_to_alignment(new_size, HEAP_ALIGN);

    objspace->root = gc_aligned_realloc(objspace->root, HEAP_ALIGN, aligned_new_size);
    objspace->allocated += aligned_new_size;
}

static inline VALUE
newobj_fill(VALUE obj, VALUE v1, VALUE v2, VALUE v3)
{
    VALUE *p = (VALUE *)obj;
    p[2] = v1;
    p[3] = v2;
    p[4] = v3;
    return obj;
}

VALUE
rb_gc_impl_new_obj(void *objspace_ptr, void *cache_ptr, VALUE klass, VALUE flags, VALUE v1, VALUE v2, VALUE v3, bool wb_protected, size_t alloc_size)
{
    rb_objspace_t *objspace = objspace_ptr;

    if (alloc_size > available_allocated_unused_memory(objspace)) {
        grow_heap_by(objspace, alloc_size);
    }

    VALUE obj = objspace->free_object;
    RBASIC(obj)->flags = flags;
    *((VALUE *)&RBASIC(obj)->klass) = klass;

    objspace->free_object += alloc_size;
    objspace->used += alloc_size;

    return newobj_fill(obj, v1, v2, v3);
}

size_t rb_gc_impl_obj_slot_size(VALUE obj)                               { return 0; }
size_t rb_gc_impl_heap_id_for_size(void *objspace_ptr, size_t size)      { return 0; }
bool
rb_gc_impl_size_allocatable_p(size_t size)
{
    return 0;
}

// Malloc
void *rb_gc_impl_malloc(void *objspace_ptr, size_t size) {
    return malloc(size);
}

void *rb_gc_impl_calloc(void *objspace_ptr, size_t size) {
    return calloc(1, size);
}

void *rb_gc_impl_realloc(void *objspace_ptr, void *ptr, size_t new_size, size_t old_size) {
    return realloc(ptr, new_size);
}

void rb_gc_impl_free(void *objspace_ptr, void *ptr, size_t old_size) {
    free(ptr);
}

void rb_gc_impl_adjust_memory_usage(void *objspace_ptr, ssize_t diff) {
    // No-op
    // TOOD Should we account that memory in our objspace?
}

// Marking
void rb_gc_impl_mark_and_move(void *objspace_ptr, VALUE *ptr)             { /* nop */ }
void rb_gc_impl_mark(void *objspace_ptr, VALUE obj)                       { /* nop */ }
void rb_gc_impl_mark_and_pin(void *objspace_ptr, VALUE obj)               { /* nop */ }
void rb_gc_impl_mark_maybe(void *objspace_ptr, VALUE obj)                 { /* nop */ }
void rb_gc_impl_mark_weak(void *objspace_ptr, VALUE *ptr)                 { /* nop */ }
void rb_gc_impl_remove_weak(void *objspace_ptr, VALUE parent, VALUE *ptr) { /* nop */ }
// Compaction
bool rb_gc_impl_object_moved_p(void *objspace, VALUE obj)                 { return FALSE; }
VALUE rb_gc_impl_location(void *objspace_ptr, VALUE value)                { return value; }
// Write barriers
void rb_gc_impl_writebarrier(void *objspace_ptr, VALUE a, VALUE b)        { /* nop */ }
void rb_gc_impl_writebarrier_unprotect(void *objspace_ptr, VALUE obj)     { /* nop */ }
void rb_gc_impl_writebarrier_remember(void *objspace_ptr, VALUE obj)      { /* nop */ }
// Heap walking
void rb_gc_impl_each_objects(void *objspace_ptr, int (*callback)(void *, void *, size_t, void *), void *data)     { /* noop */ }
void rb_gc_impl_each_object(void *objspace_ptr, void (*func)(VALUE obj, void *data), void *data)                  { /* noop */ }
// Finalizers
void rb_gc_impl_make_zombie(void *objspace_ptr, VALUE obj, void (*dfree)(void *), void *data)                     { /* noop */ }

VALUE rb_gc_impl_define_finalizer(void *objspace_ptr, VALUE obj, VALUE block)                                     { return Qnil; }
void rb_gc_impl_undefine_finalizer(void *objspace_ptr, VALUE obj)                                                 { /* noop */ }
void rb_gc_impl_copy_finalizer(void *objspace_ptr, VALUE dest, VALUE obj)                                         { /* noop */ }
void rb_gc_impl_shutdown_call_finalizer(void *objspace_ptr)                                                       { /* noop */ }
// Object ID
VALUE rb_gc_impl_object_id(void *objspace_ptr, VALUE obj) {
    return obj;
}

VALUE rb_gc_impl_object_id_to_ref(void *objspace_ptr, VALUE object_id) {
    return object_id;
}

// Statistics
void rb_gc_impl_set_measure_total_time(void *objspace_ptr, VALUE flag)    { /* noop */ }
bool rb_gc_impl_get_measure_total_time(void *objspace)                    { return FALSE; }
unsigned long long rb_gc_impl_get_total_time(void *objspace_ptr)          { return 0; }
size_t rb_gc_impl_gc_count(void *objspace_ptr)                            { return 0; }
VALUE rb_gc_impl_latest_gc_info(void *objspace_ptr, VALUE key)            { return Qnil; }
VALUE rb_gc_impl_stat(void *objspace_ptr, VALUE hash_or_sym)              { return Qnil; }
VALUE rb_gc_impl_stat_heap(void *objspace_ptr, VALUE heap_name, VALUE hash_or_sym)    { return Qnil; }
// Miscellaneous
size_t rb_gc_impl_obj_flags(void *objspace_ptr, VALUE obj, ID* flags, size_t max)     { return 0; }
bool rb_gc_impl_pointer_to_heap_p(void *objspace_ptr, const void *ptr)           { return FALSE; }
bool rb_gc_impl_garbage_object_p(void *objspace_ptr, VALUE obj)           { return FALSE; }
void rb_gc_impl_set_event_hook(void *objspace_ptr, const rb_event_flag_t event)   { /* noop */ }
void rb_gc_impl_copy_attributes(void *objspace_ptr, VALUE dest, VALUE obj)        { /* noop */ }