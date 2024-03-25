/**********************************************************************

  gc.c -

  $Author$
  created at: Tue Oct  5 09:44:46 JST 1993

  Copyright (C) 1993-2007 Yukihiro Matsumoto
  Copyright (C) 2000  Network Applied Communication Laboratory, Inc.
  Copyright (C) 2000  Information-technology Promotion Agency, Japan

**********************************************************************/

#define rb_data_object_alloc rb_data_object_alloc
#define rb_data_typed_object_alloc rb_data_typed_object_alloc

#include "ruby/internal/config.h"
#ifdef _WIN32
# include "ruby/ruby.h"
#endif

#include <signal.h>

#define sighandler_t ruby_sighandler_t

#ifndef _WIN32
#include <unistd.h>
#endif

#if defined(__wasm__) && !defined(__EMSCRIPTEN__)
# include "wasm/setjmp.h"
# include "wasm/machine.h"
#else
# include <setjmp.h>
#endif
#include <stdarg.h>
#include <stdio.h>

/* MALLOC_HEADERS_BEGIN */
#ifndef HAVE_MALLOC_USABLE_SIZE
# ifdef _WIN32
#  define HAVE_MALLOC_USABLE_SIZE
#  define malloc_usable_size(a) _msize(a)
# elif defined HAVE_MALLOC_SIZE
#  define HAVE_MALLOC_USABLE_SIZE
#  define malloc_usable_size(a) malloc_size(a)
# endif
#endif

# define GC_ASSERT

#ifdef HAVE_MALLOC_USABLE_SIZE
# ifdef RUBY_ALTERNATIVE_MALLOC_HEADER
/* Alternative malloc header is included in ruby/missing.h */
# elif defined(HAVE_MALLOC_H)
#  include <malloc.h>
# elif defined(HAVE_MALLOC_NP_H)
#  include <malloc_np.h>
# elif defined(HAVE_MALLOC_MALLOC_H)
#  include <malloc/malloc.h>
# endif
#endif

#ifdef HAVE_MALLOC_TRIM
# include <malloc.h>

# ifdef __EMSCRIPTEN__
/* malloc_trim is defined in emscripten/emmalloc.h on emscripten. */
#  include <emscripten/emmalloc.h>
# endif
#endif

#if !defined(PAGE_SIZE) && defined(HAVE_SYS_USER_H)
/* LIST_HEAD conflicts with sys/queue.h on macOS */
# include <sys/user.h>
#endif
/* MALLOC_HEADERS_END */

#ifdef HAVE_SYS_TIME_H
# include <sys/time.h>
#endif

#ifdef HAVE_SYS_RESOURCE_H
# include <sys/resource.h>
#endif

#if defined _WIN32 || defined __CYGWIN__
# include <windows.h>
#elif defined(HAVE_POSIX_MEMALIGN)
#elif defined(HAVE_MEMALIGN)
# include <malloc.h>
#endif

#include <sys/types.h>

#ifdef __EMSCRIPTEN__
#include <emscripten.h>
#endif

#ifdef HAVE_MACH_TASK_EXCEPTION_PORTS
# include <mach/task.h>
# include <mach/mach_init.h>
# include <mach/mach_port.h>
#endif
#undef LIST_HEAD /* ccan/list conflicts with BSD-origin sys/queue.h. */

#include "constant.h"
#include "darray.h"
#include "debug_counter.h"
#include "eval_intern.h"
#include "id_table.h"
#include "internal.h"
#include "internal/class.h"
#include "internal/compile.h"
#include "internal/complex.h"
#include "internal/cont.h"
#include "internal/error.h"
#include "internal/eval.h"
#include "internal/gc.h"
#include "internal/hash.h"
#include "internal/imemo.h"
#include "internal/io.h"
#include "internal/numeric.h"
#include "internal/object.h"
#include "internal/proc.h"
#include "internal/rational.h"
#include "internal/sanitizers.h"
#include "internal/struct.h"
#include "internal/symbol.h"
#include "internal/thread.h"
#include "internal/variable.h"
#include "internal/warnings.h"
#include "rjit.h"
#include "probes.h"
#include "regint.h"
#include "ruby/debug.h"
#include "ruby/io.h"
#include "ruby/re.h"
#include "ruby/st.h"
#include "ruby/thread.h"
#include "ruby/util.h"
#include "ruby_assert.h"
#include "ruby_atomic.h"
#include "symbol.h"
#include "vm_core.h"
#include "vm_sync.h"
#include "vm_callinfo.h"
#include "ractor_core.h"

#include "builtin.h"
#include "shape.h"

unsigned int
rb_gc_vm_lock(void)
{
    unsigned int lev;
    RB_VM_LOCK_ENTER_CR_LEV(GET_RACTOR(), &lev);
    return lev;
}

void
rb_gc_vm_unlock(unsigned int lev)
{
    RB_VM_LOCK_LEAVE_CR_LEV(GET_RACTOR(), &lev);
}

unsigned int
rb_gc_vm_lock_no_barrier(void)
{
    unsigned int lev;
    RB_VM_LOCK_ENTER_LEV_NB(&lev);
    return lev;
}

void
rb_gc_vm_unlock_no_barrier(unsigned int lev)
{
    RB_VM_LOCK_LEAVE_LEV(&lev);
}

void
rb_gc_vm_barrier(void)
{
    rb_vm_barrier();
}

void
rb_gc_event_hook(VALUE obj, rb_event_flag_t event)
{
    if (LIKELY(!(ruby_vm_event_flags & event))) return;

    rb_execution_context_t *ec = GET_EC();
    if (!ec->cfp) return;

    EXEC_EVENT_HOOK(ec, event, ec->cfp->self, 0, 0, 0, obj);
}

void *
rb_gc_get_objspace(void)
{
    return GET_VM()->objspace;
}

typedef int each_obj_callback(void *, void *, size_t, void *);

/* Headers from gc_impl.c */
void rb_gc_impl_make_zombie(void *objspace_ptr, VALUE obj, void (*dfree)(void *), void *data);
VALUE rb_gc_impl_undefine_finalizer(void *objspace_ptr, VALUE obj);
VALUE rb_gc_impl_define_finalizer(void *objspace_ptr, VALUE obj, VALUE block);
void rb_gc_impl_shutdown_call_finalizer(void *objspace_ptr);
VALUE rb_gc_impl_object_id_to_ref(void *objspace_ptr, VALUE object_id);
VALUE rb_gc_impl_object_id(void *objspace_ptr, VALUE obj);
void rb_gc_impl_mark_and_move(void *objspace_ptr, VALUE *ptr);
void rb_gc_impl_mark(void *objspace_ptr, VALUE obj);
void rb_gc_impl_mark_and_pin(void *objspace_ptr, VALUE obj);
void rb_gc_impl_mark_maybe(void *objspace_ptr, VALUE obj);
void rb_gc_impl_mark_weak(void *objspace_ptr, VALUE *ptr);
void rb_gc_impl_remove_weak(void *objspace_ptr, VALUE parent_obj, VALUE *ptr);
void rb_gc_impl_objspace_free(void *objspace_ptr);
void rb_gc_impl_writebarrier(void *objspace_ptr, VALUE a, VALUE b);
void rb_gc_impl_writebarrier_unprotect(void *objspace_ptr, VALUE obj);
void rb_gc_impl_writebarrier_remember(void *objspace_ptr, VALUE obj);
size_t rb_gc_impl_obj_flags(VALUE obj, ID* flags, size_t max);
void *rb_gc_impl_ractor_cache_alloc(void *objspace_ptr);
bool rb_gc_impl_is_pointer_to_heap(void *objspace_ptr, const void *ptr);
void rb_gc_impl_prepare_heap(void *objspace_ptr);
void rb_gc_impl_each_objects(void *objspace_ptr, each_obj_callback *callback, void *data);
bool rb_gc_impl_object_moved_p(void *objspace_ptr, VALUE obj);
VALUE rb_gc_impl_location(void *objspace_ptr, VALUE value);
VALUE rb_gc_impl_compact_stats(void *objspace_ptr);
void rb_gc_impl_start(void *objspace_ptr, bool full_mark, bool immediate_mark, bool immediate_sweep, bool compact);
bool rb_gc_impl_during_gc_p(void *objspace_ptr);
size_t rb_gc_impl_gc_count(void *objspace_ptr);
VALUE rb_gc_impl_latest_gc_info(void *objspace_ptr, VALUE key);
size_t rb_gc_impl_stat(void *objspace_ptr, VALUE hash_or_sym);
size_t rb_gc_impl_stat_heap(void *objspace_ptr, int size_pool_idx, VALUE hash_or_sym);
VALUE rb_gc_impl_stress_get(void *objspace_ptr);
void rb_gc_impl_stress_set(void *objspace_ptr, VALUE flag);
bool rb_gc_impl_gc_enabled_p(void *objspace_ptr);
void rb_gc_impl_gc_enable(void *objspace_ptr);
void rb_gc_impl_gc_disable(void *objspace_ptr, bool finish_current_gc);
bool rb_gc_impl_auto_compact_enabled_p(void *objspace_ptr);
void rb_gc_impl_auto_compact_enable(void *objspace_ptr, VALUE val);
void rb_gc_impl_auto_compact_disable(void *objspace_ptr);
bool rb_gc_impl_during_gc_p(void *objspace_ptr);
void *rb_gc_impl_malloc(void *objspace_ptr, size_t size);
void *rb_gc_impl_calloc(void *objspace_ptr, size_t size);
void *rb_gc_impl_realloc(void *objspace_ptr, void *ptr, size_t new_size, size_t old_size);
void rb_gc_impl_free(void *objspace_ptr, void *ptr, size_t old_size);
void rb_gc_impl_adjust_memory_usage(void *objspace_ptr, ssize_t diff);
const char *rb_gc_impl_obj_info(void *objspace_ptr, VALUE obj);
const char *rb_gc_impl_full_obj_info(void *objspace_ptr, VALUE obj, char *buffer, size_t buffer_size);
void rb_gc_impl_verify_internal_consistency(void *objspace_ptr);
VALUE rb_gc_impl_new_obj(void *objspace_ptr, void *cache_ptr, VALUE klass, VALUE flags, VALUE v1, VALUE v2, VALUE v3, bool wb_protected, size_t alloc_size);
size_t rb_gc_impl_obj_slot_size(VALUE obj);
void rb_gc_impl_obj_free_object_id(void *objspace_ptr, VALUE obj);

void rb_vm_update_references(void *ptr);

void
rb_objspace_free(void *objspace)
{
    rb_gc_impl_objspace_free(objspace);
}

typedef struct RVALUE {
    union {
        struct {
            VALUE flags;		/* always 0 for freed obj */
            struct RVALUE *next;
        } free;
        struct RBasic  basic;
        struct RObject object;
        struct RClass  klass;
        struct RFloat  flonum;
        struct RString string;
        struct RArray  array;
        struct RRegexp regexp;
        struct RHash   hash;
        struct RData   data;
        struct RTypedData   typeddata;
        struct RStruct rstruct;
        struct RBignum bignum;
        struct RFile   file;
        struct RMatch  match;
        struct RRational rational;
        struct RComplex complex;
        struct RSymbol symbol;
        union {
            rb_cref_t cref;
            struct vm_svar svar;
            struct vm_throw_data throw_data;
            struct vm_ifunc ifunc;
            struct MEMO memo;
            struct rb_method_entry_struct ment;
            const rb_iseq_t iseq;
            rb_env_t env;
            struct rb_imemo_tmpbuf_struct alloc;
            rb_ast_t ast;
        } imemo;
        struct {
            struct RBasic basic;
            VALUE v1;
            VALUE v2;
            VALUE v3;
        } values;
    } as;

    /* Start of RVALUE_OVERHEAD.
     * Do not directly read these members from the RVALUE as they're located
     * at the end of the slot (which may differ in size depending on the size
     * pool). */
#if RACTOR_CHECK_MODE
    uint32_t _ractor_belonging_id;
#endif
#if GC_DEBUG
    const char *file;
    int line;
#endif
} RVALUE;

#if RACTOR_CHECK_MODE
# define RVALUE_OVERHEAD (sizeof(RVALUE) - offsetof(RVALUE, _ractor_belonging_id))
#elif GC_DEBUG
# define RVALUE_OVERHEAD (sizeof(RVALUE) - offsetof(RVALUE, file))
#else
# define RVALUE_OVERHEAD 0
#endif

STATIC_ASSERT(sizeof_rvalue, sizeof(RVALUE) == (SIZEOF_VALUE * 5) + RVALUE_OVERHEAD);
STATIC_ASSERT(alignof_rvalue, RUBY_ALIGNOF(RVALUE) == SIZEOF_VALUE);

#define rb_setjmp(env) RUBY_SETJMP(env)
#define rb_jmp_buf rb_jmpbuf_t
#undef rb_data_object_wrap

#if !defined(MAP_ANONYMOUS) && defined(MAP_ANON)
#define MAP_ANONYMOUS MAP_ANON
#endif

#define unless_objspace(objspace) \
    void *objspace; \
    rb_vm_t *unless_objspace_vm = GET_VM(); \
    if (unless_objspace_vm) objspace = unless_objspace_vm->objspace; \
    else /* return; or objspace will be warned uninitialized */

#define RMOVED(obj) ((struct RMoved *)(obj))

#define TYPED_UPDATE_IF_MOVED(_objspace, _type, _thing) do { \
    if (rb_gc_impl_object_moved_p((_objspace), (VALUE)(_thing))) {    \
        *(_type *)&(_thing) = (_type)rb_gc_impl_location(_objspace, (VALUE)_thing); \
    } \
} while (0)

#define UPDATE_IF_MOVED(_objspace, _thing) TYPED_UPDATE_IF_MOVED(_objspace, VALUE, _thing)

static size_t malloc_offset = 0;
#if defined(HAVE_MALLOC_USABLE_SIZE)
static size_t
gc_compute_malloc_offset(void)
{
    // Different allocators use different metadata storage strategies which result in different
    // ideal sizes.
    // For instance malloc(64) will waste 8B with glibc, but waste 0B with jemalloc.
    // But malloc(56) will waste 0B with glibc, but waste 8B with jemalloc.
    // So we try allocating 64, 56 and 48 bytes and select the first offset that doesn't
    // waste memory.
    // This was tested on Linux with glibc 2.35 and jemalloc 5, and for both it result in
    // no wasted memory.
    size_t offset = 0;
    for (offset = 0; offset <= 16; offset += 8) {
        size_t allocated = (64 - offset);
        void *test_ptr = malloc(allocated);
        size_t wasted = malloc_usable_size(test_ptr) - allocated;
        free(test_ptr);

        if (wasted == 0) {
            return offset;
        }
    }
    return 0;
}
#else
static size_t
gc_compute_malloc_offset(void)
{
    // If we don't have malloc_usable_size, we use powers of 2.
    return 0;
}
#endif

size_t
rb_malloc_grow_capa(size_t current, size_t type_size)
{
    size_t current_capacity = current;
    if (current_capacity < 4) {
        current_capacity = 4;
    }
    current_capacity *= type_size;

    // We double the current capacity.
    size_t new_capacity = (current_capacity * 2);

    // And round up to the next power of 2 if it's not already one.
    if (rb_popcount64(new_capacity) != 1) {
        new_capacity = (size_t)(1 << (64 - nlz_int64(new_capacity)));
    }

    new_capacity -= malloc_offset;
    new_capacity /= type_size;
    if (current > new_capacity) {
        rb_bug("rb_malloc_grow_capa: current_capacity=%zu, new_capacity=%zu, malloc_offset=%zu", current, new_capacity, malloc_offset);
    }
    RUBY_ASSERT(new_capacity > current);
    return new_capacity;
}

static inline struct rbimpl_size_mul_overflow_tag
size_add_overflow(size_t x, size_t y)
{
    size_t z;
    bool p;
#if 0

#elif __has_builtin(__builtin_add_overflow)
    p = __builtin_add_overflow(x, y, &z);

#elif defined(DSIZE_T)
    RB_GNUC_EXTENSION DSIZE_T dx = x;
    RB_GNUC_EXTENSION DSIZE_T dy = y;
    RB_GNUC_EXTENSION DSIZE_T dz = dx + dy;
    p = dz > SIZE_MAX;
    z = (size_t)dz;

#else
    z = x + y;
    p = z < y;

#endif
    return (struct rbimpl_size_mul_overflow_tag) { p, z, };
}

static inline struct rbimpl_size_mul_overflow_tag
size_mul_add_overflow(size_t x, size_t y, size_t z) /* x * y + z */
{
    struct rbimpl_size_mul_overflow_tag t = rbimpl_size_mul_overflow(x, y);
    struct rbimpl_size_mul_overflow_tag u = size_add_overflow(t.right, z);
    return (struct rbimpl_size_mul_overflow_tag) { t.left || u.left, u.right };
}

static inline struct rbimpl_size_mul_overflow_tag
size_mul_add_mul_overflow(size_t x, size_t y, size_t z, size_t w) /* x * y + z * w */
{
    struct rbimpl_size_mul_overflow_tag t = rbimpl_size_mul_overflow(x, y);
    struct rbimpl_size_mul_overflow_tag u = rbimpl_size_mul_overflow(z, w);
    struct rbimpl_size_mul_overflow_tag v = size_add_overflow(t.right, u.right);
    return (struct rbimpl_size_mul_overflow_tag) { t.left || u.left || v.left, v.right };
}

PRINTF_ARGS(NORETURN(static void gc_raise(VALUE, const char*, ...)), 2, 3);

static inline size_t
size_mul_or_raise(size_t x, size_t y, VALUE exc)
{
    struct rbimpl_size_mul_overflow_tag t = rbimpl_size_mul_overflow(x, y);
    if (LIKELY(!t.left)) {
        return t.right;
    }
    else if (rb_during_gc()) {
        rb_memerror();          /* or...? */
    }
    else {
        gc_raise(
            exc,
            "integer overflow: %"PRIuSIZE
            " * %"PRIuSIZE
            " > %"PRIuSIZE,
            x, y, (size_t)SIZE_MAX);
    }
}

size_t
rb_size_mul_or_raise(size_t x, size_t y, VALUE exc)
{
    return size_mul_or_raise(x, y, exc);
}

static inline size_t
size_mul_add_or_raise(size_t x, size_t y, size_t z, VALUE exc)
{
    struct rbimpl_size_mul_overflow_tag t = size_mul_add_overflow(x, y, z);
    if (LIKELY(!t.left)) {
        return t.right;
    }
    else if (rb_during_gc()) {
        rb_memerror();          /* or...? */
    }
    else {
        gc_raise(
            exc,
            "integer overflow: %"PRIuSIZE
            " * %"PRIuSIZE
            " + %"PRIuSIZE
            " > %"PRIuSIZE,
            x, y, z, (size_t)SIZE_MAX);
    }
}

size_t
rb_size_mul_add_or_raise(size_t x, size_t y, size_t z, VALUE exc)
{
    return size_mul_add_or_raise(x, y, z, exc);
}

static inline size_t
size_mul_add_mul_or_raise(size_t x, size_t y, size_t z, size_t w, VALUE exc)
{
    struct rbimpl_size_mul_overflow_tag t = size_mul_add_mul_overflow(x, y, z, w);
    if (LIKELY(!t.left)) {
        return t.right;
    }
    else if (rb_during_gc()) {
        rb_memerror();          /* or...? */
    }
    else {
        gc_raise(
            exc,
            "integer overflow: %"PRIdSIZE
            " * %"PRIdSIZE
            " + %"PRIdSIZE
            " * %"PRIdSIZE
            " > %"PRIdSIZE,
            x, y, z, w, (size_t)SIZE_MAX);
    }
}

#if defined(HAVE_RB_GC_GUARDED_PTR_VAL) && HAVE_RB_GC_GUARDED_PTR_VAL
/* trick the compiler into thinking a external signal handler uses this */
volatile VALUE rb_gc_guarded_val;
volatile VALUE *
rb_gc_guarded_ptr_val(volatile VALUE *ptr, VALUE val)
{
    rb_gc_guarded_val = val;

    return ptr;
}
#endif

size_t
rb_gc_obj_slot_size(VALUE obj)
{
    return rb_gc_impl_obj_slot_size(obj);
}

static inline VALUE
newobj_of(rb_ractor_t *cr, VALUE klass, VALUE flags, VALUE v1, VALUE v2, VALUE v3, bool wb_protected, size_t size)
{
    VALUE obj = rb_gc_impl_new_obj(rb_gc_get_objspace(), GET_RACTOR()->newobj_cache, klass, flags, 0, 0, 0, FALSE, size);

    if (UNLIKELY(ruby_vm_event_flags & RUBY_INTERNAL_EVENT_NEWOBJ)) {
        unsigned int lev;
        RB_VM_LOCK_ENTER_CR_LEV(GET_RACTOR(), &lev);
        {
            memset((char *)obj + sizeof(struct RBasic), 0, rb_gc_obj_slot_size(obj) - sizeof(struct RBasic));

            rb_gc_event_hook(obj, RUBY_INTERNAL_EVENT_NEWOBJ);
        }
        RB_VM_LOCK_LEAVE_CR_LEV(GET_RACTOR(), &lev);
    }

    return obj;
}

VALUE
rb_wb_unprotected_newobj_of(VALUE klass, VALUE flags, size_t size)
{
    GC_ASSERT((flags & FL_WB_PROTECTED) == 0);
    return newobj_of(GET_RACTOR(), klass, flags, 0, 0, 0, FALSE, size);
}

VALUE
rb_wb_protected_newobj_of(rb_execution_context_t *ec, VALUE klass, VALUE flags, size_t size)
{
    GC_ASSERT((flags & FL_WB_PROTECTED) == 0);
    return newobj_of(rb_ec_ractor_ptr(ec), klass, flags, 0, 0, 0, TRUE, size);
}

#define UNEXPECTED_NODE(func) \
    rb_bug(#func"(): GC does not handle T_NODE 0x%x(%p) 0x%"PRIxVALUE, \
           BUILTIN_TYPE(obj), (void*)(obj), RBASIC(obj)->flags)

static inline void
rb_data_object_check(VALUE klass)
{
    if (klass != rb_cObject && (rb_get_alloc_func(klass) == rb_class_allocate_instance)) {
        rb_undef_alloc_func(klass);
        rb_warn("undefining the allocator of T_DATA class %"PRIsVALUE, klass);
    }
}

VALUE
rb_data_object_wrap(VALUE klass, void *datap, RUBY_DATA_FUNC dmark, RUBY_DATA_FUNC dfree)
{
    RUBY_ASSERT_ALWAYS(dfree != (RUBY_DATA_FUNC)1);
    if (klass) rb_data_object_check(klass);
    return newobj_of(GET_RACTOR(), klass, T_DATA, (VALUE)dmark, (VALUE)dfree, (VALUE)datap, !dmark, sizeof(struct RTypedData));
}

VALUE
rb_data_object_zalloc(VALUE klass, size_t size, RUBY_DATA_FUNC dmark, RUBY_DATA_FUNC dfree)
{
    VALUE obj = rb_data_object_wrap(klass, 0, dmark, dfree);
    DATA_PTR(obj) = xcalloc(1, size);
    return obj;
}

static VALUE
typed_data_alloc(VALUE klass, VALUE typed_flag, void *datap, const rb_data_type_t *type, size_t size)
{
    RBIMPL_NONNULL_ARG(type);
    if (klass) rb_data_object_check(klass);
    bool wb_protected = (type->flags & RUBY_FL_WB_PROTECTED) || !type->function.dmark;
    return newobj_of(GET_RACTOR(), klass, T_DATA, (VALUE)type, 1 | typed_flag, (VALUE)datap, wb_protected, size);
}

VALUE
rb_data_typed_object_wrap(VALUE klass, void *datap, const rb_data_type_t *type)
{
    if (UNLIKELY(type->flags & RUBY_TYPED_EMBEDDABLE)) {
        rb_raise(rb_eTypeError, "Cannot wrap an embeddable TypedData");
    }

    return typed_data_alloc(klass, 0, datap, type, sizeof(struct RTypedData));
}

VALUE
rb_data_typed_object_zalloc(VALUE klass, size_t size, const rb_data_type_t *type)
{
    if (type->flags & RUBY_TYPED_EMBEDDABLE) {
        if (!(type->flags & RUBY_TYPED_FREE_IMMEDIATELY)) {
            rb_raise(rb_eTypeError, "Embeddable TypedData must be freed immediately");
        }

        size_t embed_size = offsetof(struct RTypedData, data) + size;
        if (rb_gc_size_allocatable_p(embed_size)) {
            VALUE obj = typed_data_alloc(klass, TYPED_DATA_EMBEDDED, 0, type, embed_size);
            memset((char *)obj + offsetof(struct RTypedData, data), 0, size);
            return obj;
        }
    }

    VALUE obj = typed_data_alloc(klass, 0, NULL, type, sizeof(struct RTypedData));
    DATA_PTR(obj) = xcalloc(1, size);
    return obj;
}

static size_t
rb_objspace_data_type_memsize(VALUE obj)
{
    size_t size = 0;
    if (RTYPEDDATA_P(obj)) {
        const rb_data_type_t *type = RTYPEDDATA_TYPE(obj);
        const void *ptr = RTYPEDDATA_GET_DATA(obj);

        if (RTYPEDDATA_TYPE(obj)->flags & RUBY_TYPED_EMBEDDABLE && !RTYPEDDATA_EMBEDDED_P(obj)) {
#ifdef HAVE_MALLOC_USABLE_SIZE
            size += malloc_usable_size((void *)ptr);
#endif
        }

        if (ptr && type->function.dsize) {
            size += type->function.dsize(ptr);
        }
    }

    return size;
}

const char *
rb_objspace_data_type_name(VALUE obj)
{
    if (RTYPEDDATA_P(obj)) {
        return RTYPEDDATA_TYPE(obj)->wrap_struct_name;
    }
    else {
        return 0;
    }
}

static enum rb_id_table_iterator_result
cvar_table_free_i(VALUE value, void *ctx)
{
    xfree((void *)value);
    return ID_TABLE_CONTINUE;
}

static inline void
make_io_zombie(void *objspace, VALUE obj)
{
    rb_io_t *fptr = RFILE(obj)->fptr;
    rb_gc_impl_make_zombie(objspace, obj, rb_io_fptr_finalize_internal, fptr);
}

static bool
rb_data_free(void *objspace, VALUE obj)
{
    void *data = RTYPEDDATA_P(obj) ? RTYPEDDATA_GET_DATA(obj) : DATA_PTR(obj);
    if (data) {
        int free_immediately = false;
        void (*dfree)(void *);

        if (RTYPEDDATA_P(obj)) {
            free_immediately = (RTYPEDDATA(obj)->type->flags & RUBY_TYPED_FREE_IMMEDIATELY) != 0;
            dfree = RTYPEDDATA(obj)->type->function.dfree;
        }
        else {
            dfree = RDATA(obj)->dfree;
        }

        if (dfree) {
            if (dfree == RUBY_DEFAULT_FREE) {
                if (!RTYPEDDATA_EMBEDDED_P(obj)) {
                    xfree(data);
                    RB_DEBUG_COUNTER_INC(obj_data_xfree);
                }
            }
            else if (free_immediately) {
                (*dfree)(data);
                if (RTYPEDDATA_TYPE(obj)->flags & RUBY_TYPED_EMBEDDABLE && !RTYPEDDATA_EMBEDDED_P(obj)) {
                    xfree(data);
                }

                RB_DEBUG_COUNTER_INC(obj_data_imm_free);
            }
            else {
                rb_gc_impl_make_zombie(rb_gc_get_objspace(), obj, dfree, data);
                RB_DEBUG_COUNTER_INC(obj_data_zombie);
                return FALSE;
            }
        }
        else {
            RB_DEBUG_COUNTER_INC(obj_data_empty);
        }
    }

    return true;
}

static int
obj_free(void *objspace, VALUE obj)
{
    RB_DEBUG_COUNTER_INC(obj_free);

    rb_gc_event_hook(obj, RUBY_INTERNAL_EVENT_FREEOBJ);

    switch (BUILTIN_TYPE(obj)) {
      case T_NIL:
      case T_FIXNUM:
      case T_TRUE:
      case T_FALSE:
        rb_bug("obj_free() called for broken object");
        break;
      default:
        break;
    }

    if (FL_TEST(obj, FL_EXIVAR)) {
        rb_free_generic_ivar((VALUE)obj);
        FL_UNSET(obj, FL_EXIVAR);
    }

    switch (BUILTIN_TYPE(obj)) {
      case T_OBJECT:
        if (rb_shape_obj_too_complex(obj)) {
            RB_DEBUG_COUNTER_INC(obj_obj_too_complex);
            st_free_table(ROBJECT_IV_HASH(obj));
        }
        else if (RBASIC(obj)->flags & ROBJECT_EMBED) {
            RB_DEBUG_COUNTER_INC(obj_obj_embed);
        }
        else {
            xfree(ROBJECT(obj)->as.heap.ivptr);
            RB_DEBUG_COUNTER_INC(obj_obj_ptr);
        }
        break;
      case T_MODULE:
      case T_CLASS:
        rb_id_table_free(RCLASS_M_TBL(obj));
        rb_cc_table_free(obj);
        if (rb_shape_obj_too_complex(obj)) {
            st_free_table((st_table *)RCLASS_IVPTR(obj));
        }
        else {
            xfree(RCLASS_IVPTR(obj));
        }

        if (RCLASS_CONST_TBL(obj)) {
            rb_free_const_table(RCLASS_CONST_TBL(obj));
        }
        if (RCLASS_CVC_TBL(obj)) {
            rb_id_table_foreach_values(RCLASS_CVC_TBL(obj), cvar_table_free_i, NULL);
            rb_id_table_free(RCLASS_CVC_TBL(obj));
        }
        rb_class_remove_subclass_head(obj);
        rb_class_remove_from_module_subclasses(obj);
        rb_class_remove_from_super_subclasses(obj);
        if (FL_TEST_RAW(obj, RCLASS_SUPERCLASSES_INCLUDE_SELF)) {
            xfree(RCLASS_SUPERCLASSES(obj));
        }

        (void)RB_DEBUG_COUNTER_INC_IF(obj_module_ptr, BUILTIN_TYPE(obj) == T_MODULE);
        (void)RB_DEBUG_COUNTER_INC_IF(obj_class_ptr, BUILTIN_TYPE(obj) == T_CLASS);
        break;
      case T_STRING:
        rb_str_free(obj);
        break;
      case T_ARRAY:
        rb_ary_free(obj);
        break;
      case T_HASH:
#if USE_DEBUG_COUNTER
        switch (RHASH_SIZE(obj)) {
          case 0:
            RB_DEBUG_COUNTER_INC(obj_hash_empty);
            break;
          case 1:
            RB_DEBUG_COUNTER_INC(obj_hash_1);
            break;
          case 2:
            RB_DEBUG_COUNTER_INC(obj_hash_2);
            break;
          case 3:
            RB_DEBUG_COUNTER_INC(obj_hash_3);
            break;
          case 4:
            RB_DEBUG_COUNTER_INC(obj_hash_4);
            break;
          case 5:
          case 6:
          case 7:
          case 8:
            RB_DEBUG_COUNTER_INC(obj_hash_5_8);
            break;
          default:
            GC_ASSERT(RHASH_SIZE(obj) > 8);
            RB_DEBUG_COUNTER_INC(obj_hash_g8);
        }

        if (RHASH_AR_TABLE_P(obj)) {
            if (RHASH_AR_TABLE(obj) == NULL) {
                RB_DEBUG_COUNTER_INC(obj_hash_null);
            }
            else {
                RB_DEBUG_COUNTER_INC(obj_hash_ar);
            }
        }
        else {
            RB_DEBUG_COUNTER_INC(obj_hash_st);
        }
#endif

        rb_hash_free(obj);
        break;
      case T_REGEXP:
        if (RREGEXP(obj)->ptr) {
            onig_free(RREGEXP(obj)->ptr);
            RB_DEBUG_COUNTER_INC(obj_regexp_ptr);
        }
        break;
      case T_DATA:
        if (!rb_data_free(objspace, obj)) return false;
        break;
      case T_MATCH:
        {
            rb_matchext_t *rm = RMATCH_EXT(obj);
#if USE_DEBUG_COUNTER
            if (rm->regs.num_regs >= 8) {
                RB_DEBUG_COUNTER_INC(obj_match_ge8);
            }
            else if (rm->regs.num_regs >= 4) {
                RB_DEBUG_COUNTER_INC(obj_match_ge4);
            }
            else if (rm->regs.num_regs >= 1) {
                RB_DEBUG_COUNTER_INC(obj_match_under4);
            }
#endif
            onig_region_free(&rm->regs, 0);
            xfree(rm->char_offset);

            RB_DEBUG_COUNTER_INC(obj_match_ptr);
        }
        break;
      case T_FILE:
        if (RFILE(obj)->fptr) {
            make_io_zombie(objspace, obj);
            RB_DEBUG_COUNTER_INC(obj_file_ptr);
            return FALSE;
        }
        break;
      case T_RATIONAL:
        RB_DEBUG_COUNTER_INC(obj_rational);
        break;
      case T_COMPLEX:
        RB_DEBUG_COUNTER_INC(obj_complex);
        break;
      case T_MOVED:
        break;
      case T_ICLASS:
        /* Basically , T_ICLASS shares table with the module */
        if (RICLASS_OWNS_M_TBL_P(obj)) {
            /* Method table is not shared for origin iclasses of classes */
            rb_id_table_free(RCLASS_M_TBL(obj));
        }
        if (RCLASS_CALLABLE_M_TBL(obj) != NULL) {
            rb_id_table_free(RCLASS_CALLABLE_M_TBL(obj));
        }
        rb_class_remove_subclass_head(obj);
        rb_cc_table_free(obj);
        rb_class_remove_from_module_subclasses(obj);
        rb_class_remove_from_super_subclasses(obj);

        RB_DEBUG_COUNTER_INC(obj_iclass_ptr);
        break;

      case T_FLOAT:
        RB_DEBUG_COUNTER_INC(obj_float);
        break;

      case T_BIGNUM:
        if (!BIGNUM_EMBED_P(obj) && BIGNUM_DIGITS(obj)) {
            xfree(BIGNUM_DIGITS(obj));
            RB_DEBUG_COUNTER_INC(obj_bignum_ptr);
        }
        else {
            RB_DEBUG_COUNTER_INC(obj_bignum_embed);
        }
        break;

      case T_NODE:
        UNEXPECTED_NODE(obj_free);
        break;

      case T_STRUCT:
        if ((RBASIC(obj)->flags & RSTRUCT_EMBED_LEN_MASK) ||
            RSTRUCT(obj)->as.heap.ptr == NULL) {
            RB_DEBUG_COUNTER_INC(obj_struct_embed);
        }
        else {
            xfree((void *)RSTRUCT(obj)->as.heap.ptr);
            RB_DEBUG_COUNTER_INC(obj_struct_ptr);
        }
        break;

      case T_SYMBOL:
        {
            rb_gc_free_dsymbol(obj);
            RB_DEBUG_COUNTER_INC(obj_symbol);
        }
        break;

      case T_IMEMO:
        rb_imemo_free((VALUE)obj);
        break;

      default:
        rb_bug("gc_sweep(): unknown data type 0x%x(%p) 0x%"PRIxVALUE,
               BUILTIN_TYPE(obj), (void*)obj, RBASIC(obj)->flags);
    }

    if (FL_TEST(obj, FL_FINALIZE)) {
        rb_gc_impl_make_zombie(rb_gc_get_objspace(), obj, 0, 0);
        return FALSE;
    }
    else {
        RBASIC(obj)->flags = 0;
        return TRUE;
    }
}



static int
internal_object_p(VALUE obj)
{
    RVALUE *p = (RVALUE *)obj;
    void *ptr = asan_unpoison_object_temporary(obj);
    bool used_p = p->as.basic.flags;

    if (used_p) {
        switch (BUILTIN_TYPE(obj)) {
          case T_NODE:
            UNEXPECTED_NODE(internal_object_p);
            break;
          case T_NONE:
          case T_MOVED:
          case T_IMEMO:
          case T_ICLASS:
          case T_ZOMBIE:
            break;
          case T_CLASS:
            if (!p->as.basic.klass) break;
            if (RCLASS_SINGLETON_P(obj)) {
                return rb_singleton_class_internal_p(obj);
            }
            return 0;
          default:
            if (!p->as.basic.klass) break;
            return 0;
        }
    }
    if (ptr || ! used_p) {
        asan_poison_object(obj);
    }
    return 1;
}

int
rb_objspace_internal_object_p(VALUE obj)
{
    return internal_object_p(obj);
}

static int
os_obj_of_i(void *vstart, void *vend, size_t stride, void *data)
{
    struct os_each_struct *oes = (struct os_each_struct *)data;

    VALUE v = (VALUE)vstart;
    for (; v != (VALUE)vend; v += stride) {
        if (!internal_object_p(v)) {
            if (!oes->of || rb_obj_is_kind_of(v, oes->of)) {
                if (!rb_multi_ractor_p() || rb_ractor_shareable_p(v)) {
                    rb_yield(v);
                    oes->num++;
                }
            }
        }
    }

    return 0;
}

static VALUE
os_obj_of(VALUE of)
{
    struct os_each_struct oes;

    oes.num = 0;
    oes.of = of;
    rb_objspace_each_objects(rb_gc_get_objspace(), os_obj_of_i, &oes);
    return SIZET2NUM(oes.num);
}

/*
 *  call-seq:
 *     ObjectSpace.each_object([module]) {|obj| ... } -> integer
 *     ObjectSpace.each_object([module])              -> an_enumerator
 *
 *  Calls the block once for each living, nonimmediate object in this
 *  Ruby process. If <i>module</i> is specified, calls the block
 *  for only those classes or modules that match (or are a subclass of)
 *  <i>module</i>. Returns the number of objects found. Immediate
 *  objects (<code>Fixnum</code>s, <code>Symbol</code>s
 *  <code>true</code>, <code>false</code>, and <code>nil</code>) are
 *  never returned. In the example below, #each_object returns both
 *  the numbers we defined and several constants defined in the Math
 *  module.
 *
 *  If no block is given, an enumerator is returned instead.
 *
 *     a = 102.7
 *     b = 95       # Won't be returned
 *     c = 12345678987654321
 *     count = ObjectSpace.each_object(Numeric) {|x| p x }
 *     puts "Total count: #{count}"
 *
 *  <em>produces:</em>
 *
 *     12345678987654321
 *     102.7
 *     2.71828182845905
 *     3.14159265358979
 *     2.22044604925031e-16
 *     1.7976931348623157e+308
 *     2.2250738585072e-308
 *     Total count: 7
 *
 */

static VALUE
os_each_obj(int argc, VALUE *argv, VALUE os)
{
    VALUE of;

    of = (!rb_check_arity(argc, 0, 1) ? 0 : argv[0]);
    RETURN_ENUMERATOR(os, 1, &of);
    return os_obj_of(of);
}

/*
 *  call-seq:
 *     ObjectSpace.undefine_finalizer(obj)
 *
 *  Removes all finalizers for <i>obj</i>.
 *
 */

static VALUE
undefine_final(VALUE os, VALUE obj)
{
    return rb_gc_impl_undefine_finalizer(rb_gc_get_objspace(), obj);
}

static void
should_be_callable(VALUE block)
{
    if (!rb_obj_respond_to(block, idCall, TRUE)) {
        rb_raise(rb_eArgError, "wrong type argument %"PRIsVALUE" (should be callable)",
                 rb_obj_class(block));
    }
}

static void
should_be_finalizable(VALUE obj)
{
    if (!FL_ABLE(obj)) {
        rb_raise(rb_eArgError, "cannot define finalizer for %s",
                 rb_obj_classname(obj));
    }
    rb_check_frozen(obj);
}

/*
 *  call-seq:
 *     ObjectSpace.define_finalizer(obj, aProc=proc())
 *
 *  Adds <i>aProc</i> as a finalizer, to be called after <i>obj</i>
 *  was destroyed. The object ID of the <i>obj</i> will be passed
 *  as an argument to <i>aProc</i>. If <i>aProc</i> is a lambda or
 *  method, make sure it can be called with a single argument.
 *
 *  The return value is an array <code>[0, aProc]</code>.
 *
 *  The two recommended patterns are to either create the finaliser proc
 *  in a non-instance method where it can safely capture the needed state,
 *  or to use a custom callable object that stores the needed state
 *  explicitly as instance variables.
 *
 *      class Foo
 *        def initialize(data_needed_for_finalization)
 *          ObjectSpace.define_finalizer(self, self.class.create_finalizer(data_needed_for_finalization))
 *        end
 *
 *        def self.create_finalizer(data_needed_for_finalization)
 *          proc {
 *            puts "finalizing #{data_needed_for_finalization}"
 *          }
 *        end
 *      end
 *
 *      class Bar
 *       class Remover
 *          def initialize(data_needed_for_finalization)
 *            @data_needed_for_finalization = data_needed_for_finalization
 *          end
 *
 *          def call(id)
 *            puts "finalizing #{@data_needed_for_finalization}"
 *          end
 *        end
 *
 *        def initialize(data_needed_for_finalization)
 *          ObjectSpace.define_finalizer(self, Remover.new(data_needed_for_finalization))
 *        end
 *      end
 *
 *  Note that if your finalizer references the object to be
 *  finalized it will never be run on GC, although it will still be
 *  run at exit. You will get a warning if you capture the object
 *  to be finalized as the receiver of the finalizer.
 *
 *      class CapturesSelf
 *        def initialize(name)
 *          ObjectSpace.define_finalizer(self, proc {
 *            # this finalizer will only be run on exit
 *            puts "finalizing #{name}"
 *          })
 *        end
 *      end
 *
 *  Also note that finalization can be unpredictable and is never guaranteed
 *  to be run except on exit.
 */

static VALUE
define_final(int argc, VALUE *argv, VALUE os)
{
    VALUE obj, block;

    rb_scan_args(argc, argv, "11", &obj, &block);
    should_be_finalizable(obj);
    if (argc == 1) {
        block = rb_block_proc();
    }
    else {
        should_be_callable(block);
    }

    if (rb_callable_receiver(block) == obj) {
        rb_warn("finalizer references object to be finalized");
    }

    return rb_gc_impl_define_finalizer(rb_gc_get_objspace(), obj, block);
}

VALUE
rb_define_finalizer(VALUE obj, VALUE block)
{
    should_be_finalizable(obj);
    should_be_callable(block);
    return rb_gc_impl_define_finalizer(rb_gc_get_objspace(), obj, block);
}

static void
warn_exception_in_finalizer(rb_execution_context_t *ec, VALUE final)
{
    if (!UNDEF_P(final) && !NIL_P(ruby_verbose)) {
        VALUE errinfo = ec->errinfo;
        rb_warn("Exception in finalizer %+"PRIsVALUE, final);
        rb_ec_error_print(ec, errinfo);
    }
}

void
rb_objspace_call_finalizer(void)
{
    rb_gc_impl_shutdown_call_finalizer(rb_gc_get_objspace());
}

static inline int
is_markable_object(VALUE obj)
{
    return !RB_SPECIAL_CONST_P(obj);
}

int
rb_objspace_markable_object_p(VALUE obj)
{
    return is_markable_object(obj) && is_live_object(rb_gc_get_objspace(), obj);
}

int
rb_objspace_garbage_object_p(VALUE obj)
{
    rb_objspace_t *objspace = &rb_objspace;
    return is_garbage_object(objspace, obj);
}

bool
rb_gc_is_ptr_to_obj(const void *ptr)
{
    rb_objspace_t *objspace = &rb_objspace;
    return is_pointer_to_heap(objspace, ptr);
}

/* :nodoc: */
static VALUE
os_id2ref(VALUE os, VALUE objid)
{
    return rb_gc_impl_object_id_to_ref(rb_gc_get_objspace(), objid);
}

static VALUE
rb_find_object_id(void *objspace, VALUE obj, VALUE (*get_heap_object_id)(void *, VALUE))
{
    if (STATIC_SYM_P(obj)) {
        return (SYM2ID(obj) * sizeof(RVALUE) + (4 << 2)) | FIXNUM_FLAG;
    }
    else if (FLONUM_P(obj)) {
#if SIZEOF_LONG == SIZEOF_VOIDP
        return LONG2NUM((SIGNED_VALUE)obj);
#else
        return LL2NUM((SIGNED_VALUE)obj);
#endif
    }
    else if (SPECIAL_CONST_P(obj)) {
        return LONG2NUM((SIGNED_VALUE)obj);
    }

    return get_heap_object_id(obj);
}

static VALUE
nonspecial_obj_id(void *_objspace, VALUE obj)
{
#if SIZEOF_LONG == SIZEOF_VOIDP
    return (VALUE)((SIGNED_VALUE)(obj)|FIXNUM_FLAG);
#elif SIZEOF_LONG_LONG == SIZEOF_VOIDP
    return LL2NUM((SIGNED_VALUE)(obj) / 2);
#else
# error not supported
#endif
}

VALUE
rb_memory_id(VALUE obj)
{
    return rb_find_object_id(NULL, obj, nonspecial_obj_id);
}

/*
 *  Document-method: __id__
 *  Document-method: object_id
 *
 *  call-seq:
 *     obj.__id__       -> integer
 *     obj.object_id    -> integer
 *
 *  Returns an integer identifier for +obj+.
 *
 *  The same number will be returned on all calls to +object_id+ for a given
 *  object, and no two active objects will share an id.
 *
 *  Note: that some objects of builtin classes are reused for optimization.
 *  This is the case for immediate values and frozen string literals.
 *
 *  BasicObject implements +__id__+, Kernel implements +object_id+.
 *
 *  Immediate values are not passed by reference but are passed by value:
 *  +nil+, +true+, +false+, Fixnums, Symbols, and some Floats.
 *
 *      Object.new.object_id  == Object.new.object_id  # => false
 *      (21 * 2).object_id    == (21 * 2).object_id    # => true
 *      "hello".object_id     == "hello".object_id     # => false
 *      "hi".freeze.object_id == "hi".freeze.object_id # => true
 */

VALUE
rb_obj_id(VALUE obj)
{
    /*
     *                32-bit VALUE space
     *          MSB ------------------------ LSB
     *  false   00000000000000000000000000000000
     *  true    00000000000000000000000000000010
     *  nil     00000000000000000000000000000100
     *  undef   00000000000000000000000000000110
     *  symbol  ssssssssssssssssssssssss00001110
     *  object  oooooooooooooooooooooooooooooo00        = 0 (mod sizeof(RVALUE))
     *  fixnum  fffffffffffffffffffffffffffffff1
     *
     *                    object_id space
     *                                       LSB
     *  false   00000000000000000000000000000000
     *  true    00000000000000000000000000000010
     *  nil     00000000000000000000000000000100
     *  undef   00000000000000000000000000000110
     *  symbol   000SSSSSSSSSSSSSSSSSSSSSSSSSSS0        S...S % A = 4 (S...S = s...s * A + 4)
     *  object   oooooooooooooooooooooooooooooo0        o...o % A = 0
     *  fixnum  fffffffffffffffffffffffffffffff1        bignum if required
     *
     *  where A = sizeof(RVALUE)/4
     *
     *  sizeof(RVALUE) is
     *  20 if 32-bit, double is 4-byte aligned
     *  24 if 32-bit, double is 8-byte aligned
     *  40 if 64-bit
     */

    return rb_find_object_id(rb_gc_get_objspace(), obj, rb_gc_impl_object_id);
}

static enum rb_id_table_iterator_result
cc_table_memsize_i(VALUE ccs_ptr, void *data_ptr)
{
    size_t *total_size = data_ptr;
    struct rb_class_cc_entries *ccs = (struct rb_class_cc_entries *)ccs_ptr;
    *total_size += sizeof(*ccs);
    *total_size += sizeof(ccs->entries[0]) * ccs->capa;
    return ID_TABLE_CONTINUE;
}

static size_t
cc_table_memsize(struct rb_id_table *cc_table)
{
    size_t total = rb_id_table_memsize(cc_table);
    rb_id_table_foreach_values(cc_table, cc_table_memsize_i, &total);
    return total;
}

static size_t
obj_memsize_of(VALUE obj, int use_all_types)
{
    size_t size = 0;

    if (SPECIAL_CONST_P(obj)) {
        return 0;
    }

    if (FL_TEST(obj, FL_EXIVAR)) {
        size += rb_generic_ivar_memsize(obj);
    }

    switch (BUILTIN_TYPE(obj)) {
      case T_OBJECT:
        if (rb_shape_obj_too_complex(obj)) {
            size += rb_st_memsize(ROBJECT_IV_HASH(obj));
        }
        else if (!(RBASIC(obj)->flags & ROBJECT_EMBED)) {
            size += ROBJECT_IV_CAPACITY(obj) * sizeof(VALUE);
        }
        break;
      case T_MODULE:
      case T_CLASS:
        if (RCLASS_M_TBL(obj)) {
            size += rb_id_table_memsize(RCLASS_M_TBL(obj));
        }
        // class IV sizes are allocated as powers of two
        size += SIZEOF_VALUE << bit_length(RCLASS_IV_COUNT(obj));
        if (RCLASS_CVC_TBL(obj)) {
            size += rb_id_table_memsize(RCLASS_CVC_TBL(obj));
        }
        if (RCLASS_EXT(obj)->const_tbl) {
            size += rb_id_table_memsize(RCLASS_EXT(obj)->const_tbl);
        }
        if (RCLASS_CC_TBL(obj)) {
            size += cc_table_memsize(RCLASS_CC_TBL(obj));
        }
        if (FL_TEST_RAW(obj, RCLASS_SUPERCLASSES_INCLUDE_SELF)) {
            size += (RCLASS_SUPERCLASS_DEPTH(obj) + 1) * sizeof(VALUE);
        }
        break;
      case T_ICLASS:
        if (RICLASS_OWNS_M_TBL_P(obj)) {
            if (RCLASS_M_TBL(obj)) {
                size += rb_id_table_memsize(RCLASS_M_TBL(obj));
            }
        }
        if (RCLASS_CC_TBL(obj)) {
            size += cc_table_memsize(RCLASS_CC_TBL(obj));
        }
        break;
      case T_STRING:
        size += rb_str_memsize(obj);
        break;
      case T_ARRAY:
        size += rb_ary_memsize(obj);
        break;
      case T_HASH:
        if (RHASH_ST_TABLE_P(obj)) {
            VM_ASSERT(RHASH_ST_TABLE(obj) != NULL);
            /* st_table is in the slot */
            size += st_memsize(RHASH_ST_TABLE(obj)) - sizeof(st_table);
        }
        break;
      case T_REGEXP:
        if (RREGEXP_PTR(obj)) {
            size += onig_memsize(RREGEXP_PTR(obj));
        }
        break;
      case T_DATA:
        if (use_all_types) size += rb_objspace_data_type_memsize(obj);
        break;
      case T_MATCH:
        {
            rb_matchext_t *rm = RMATCH_EXT(obj);
            size += onig_region_memsize(&rm->regs);
            size += sizeof(struct rmatch_offset) * rm->char_offset_num_allocated;
        }
        break;
      case T_FILE:
        if (RFILE(obj)->fptr) {
            size += rb_io_memsize(RFILE(obj)->fptr);
        }
        break;
      case T_RATIONAL:
      case T_COMPLEX:
        break;
      case T_IMEMO:
        size += rb_imemo_memsize(obj);
        break;

      case T_FLOAT:
      case T_SYMBOL:
        break;

      case T_BIGNUM:
        if (!(RBASIC(obj)->flags & BIGNUM_EMBED_FLAG) && BIGNUM_DIGITS(obj)) {
            size += BIGNUM_LEN(obj) * sizeof(BDIGIT);
        }
        break;

      case T_NODE:
        UNEXPECTED_NODE(obj_memsize_of);
        break;

      case T_STRUCT:
        if ((RBASIC(obj)->flags & RSTRUCT_EMBED_LEN_MASK) == 0 &&
            RSTRUCT(obj)->as.heap.ptr) {
            size += sizeof(VALUE) * RSTRUCT_LEN(obj);
        }
        break;

      case T_ZOMBIE:
      case T_MOVED:
        break;

      default:
        rb_bug("objspace/memsize_of(): unknown data type 0x%x(%p)",
               BUILTIN_TYPE(obj), (void*)obj);
    }

    return size + rb_gc_obj_slot_size(obj);
}

size_t
rb_obj_memsize_of(VALUE obj)
{
    return obj_memsize_of(obj, TRUE);
}

static int
set_zero(st_data_t key, st_data_t val, st_data_t arg)
{
    VALUE k = (VALUE)key;
    VALUE hash = (VALUE)arg;
    rb_hash_aset(hash, k, INT2FIX(0));
    return ST_CONTINUE;
}

static VALUE
type_sym(size_t type)
{
    switch (type) {
#define COUNT_TYPE(t) case (t): return ID2SYM(rb_intern(#t)); break;
        COUNT_TYPE(T_NONE);
        COUNT_TYPE(T_OBJECT);
        COUNT_TYPE(T_CLASS);
        COUNT_TYPE(T_MODULE);
        COUNT_TYPE(T_FLOAT);
        COUNT_TYPE(T_STRING);
        COUNT_TYPE(T_REGEXP);
        COUNT_TYPE(T_ARRAY);
        COUNT_TYPE(T_HASH);
        COUNT_TYPE(T_STRUCT);
        COUNT_TYPE(T_BIGNUM);
        COUNT_TYPE(T_FILE);
        COUNT_TYPE(T_DATA);
        COUNT_TYPE(T_MATCH);
        COUNT_TYPE(T_COMPLEX);
        COUNT_TYPE(T_RATIONAL);
        COUNT_TYPE(T_NIL);
        COUNT_TYPE(T_TRUE);
        COUNT_TYPE(T_FALSE);
        COUNT_TYPE(T_SYMBOL);
        COUNT_TYPE(T_FIXNUM);
        COUNT_TYPE(T_IMEMO);
        COUNT_TYPE(T_UNDEF);
        COUNT_TYPE(T_NODE);
        COUNT_TYPE(T_ICLASS);
        COUNT_TYPE(T_ZOMBIE);
        COUNT_TYPE(T_MOVED);
#undef COUNT_TYPE
        default:              return SIZET2NUM(type); break;
    }
}

struct count_objects_data {
    size_t counts[T_MASK+1];
    size_t freed;
    size_t total;
};

static void
count_objects_i(VALUE obj, void *d)
{
    struct count_objects_data *data = (struct count_objects_data *)d;

    if (RBASIC(obj)->flags) {
        data->counts[BUILTIN_TYPE(obj)]++;
    }
    else {
        data->freed++;
    }

    data->total++;
}

/*
 *  call-seq:
 *     ObjectSpace.count_objects([result_hash]) -> hash
 *
 *  Counts all objects grouped by type.
 *
 *  It returns a hash, such as:
 *	{
 *	  :TOTAL=>10000,
 *	  :FREE=>3011,
 *	  :T_OBJECT=>6,
 *	  :T_CLASS=>404,
 *	  # ...
 *	}
 *
 *  The contents of the returned hash are implementation specific.
 *  It may be changed in future.
 *
 *  The keys starting with +:T_+ means live objects.
 *  For example, +:T_ARRAY+ is the number of arrays.
 *  +:FREE+ means object slots which is not used now.
 *  +:TOTAL+ means sum of above.
 *
 *  If the optional argument +result_hash+ is given,
 *  it is overwritten and returned. This is intended to avoid probe effect.
 *
 *    h = {}
 *    ObjectSpace.count_objects(h)
 *    puts h
 *    # => { :TOTAL=>10000, :T_CLASS=>158280, :T_MODULE=>20672, :T_STRING=>527249 }
 *
 *  This method is only expected to work on C Ruby.
 *
 */

static VALUE
count_objects(int argc, VALUE *argv, VALUE os)
{
    struct count_objects_data data = { 0 };
    VALUE hash = Qnil;

    if (rb_check_arity(argc, 0, 1) == 1) {
        hash = argv[0];
        if (!RB_TYPE_P(hash, T_HASH))
            rb_raise(rb_eTypeError, "non-hash given");
    }

    rb_gc_impl_each_object(rb_gc_get_objspace(), count_objects_i, &data);

    if (NIL_P(hash)) {
        hash = rb_hash_new();
    }
    else if (!RHASH_EMPTY_P(hash)) {
        rb_hash_stlike_foreach(hash, set_zero, hash);
    }
    rb_hash_aset(hash, ID2SYM(rb_intern("TOTAL")), SIZET2NUM(data.total));
    rb_hash_aset(hash, ID2SYM(rb_intern("FREE")), SIZET2NUM(data.freed));

    for (size_t i = 0; i <= T_MASK; i++) {
        VALUE type = type_sym(i);
        if (data.counts[i])
            rb_hash_aset(hash, type, SIZET2NUM(data.counts[i]));
    }

    return hash;
}

#define SET_STACK_END SET_MACHINE_STACK_END(&ec->machine.stack_end)

#define STACK_START (ec->machine.stack_start)
#define STACK_END (ec->machine.stack_end)
#define STACK_LEVEL_MAX (ec->machine.stack_maxsize/sizeof(VALUE))

#if STACK_GROW_DIRECTION < 0
# define STACK_LENGTH  (size_t)(STACK_START - STACK_END)
#elif STACK_GROW_DIRECTION > 0
# define STACK_LENGTH  (size_t)(STACK_END - STACK_START + 1)
#else
# define STACK_LENGTH  ((STACK_END < STACK_START) ? (size_t)(STACK_START - STACK_END) \
                        : (size_t)(STACK_END - STACK_START + 1))
#endif
#if !STACK_GROW_DIRECTION
int ruby_stack_grow_direction;
int
ruby_get_stack_grow_direction(volatile VALUE *addr)
{
    VALUE *end;
    SET_MACHINE_STACK_END(&end);

    if (end > addr) return ruby_stack_grow_direction = 1;
    return ruby_stack_grow_direction = -1;
}
#endif

size_t
ruby_stack_length(VALUE **p)
{
    rb_execution_context_t *ec = GET_EC();
    SET_STACK_END;
    if (p) *p = STACK_UPPER(STACK_END, STACK_START, STACK_END);
    return STACK_LENGTH;
}

#define PREVENT_STACK_OVERFLOW 1
#ifndef PREVENT_STACK_OVERFLOW
#if !(defined(POSIX_SIGNAL) && defined(SIGSEGV) && defined(HAVE_SIGALTSTACK))
# define PREVENT_STACK_OVERFLOW 1
#else
# define PREVENT_STACK_OVERFLOW 0
#endif
#endif
#if PREVENT_STACK_OVERFLOW && !defined(__EMSCRIPTEN__)
static int
stack_check(rb_execution_context_t *ec, int water_mark)
{
    SET_STACK_END;

    size_t length = STACK_LENGTH;
    size_t maximum_length = STACK_LEVEL_MAX - water_mark;

    return length > maximum_length;
}
#else
#define stack_check(ec, water_mark) FALSE
#endif

#define STACKFRAME_FOR_CALL_CFUNC 2048

int
rb_ec_stack_check(rb_execution_context_t *ec)
{
    return stack_check(ec, STACKFRAME_FOR_CALL_CFUNC);
}

int
ruby_stack_check(void)
{
    return stack_check(GET_EC(), STACKFRAME_FOR_CALL_CFUNC);
}

ATTRIBUTE_NO_ADDRESS_SAFETY_ANALYSIS(static void each_location(void *objspace, register const VALUE *x, register long n, void (*cb)(void *objspace, VALUE)));
static void
each_location(void *objspace, register const VALUE *x, register long n, void (*cb)(void *objspace, VALUE))
{
    VALUE v;
    while (n--) {
        v = *x;
        cb(objspace, v);
        x++;
    }
}

static void
gc_mark_locations(void *objspace, const VALUE *start, const VALUE *end, void (*cb)(VALUE))
{
    long n;

    if (end <= start) return;
    n = end - start;
    each_location(objspace, start, n, cb);
}

void
rb_gc_mark_locations(const VALUE *start, const VALUE *end)
{
    gc_mark_locations(rb_gc_get_objspace(), start, end, rb_gc_impl_mark_maybe);
}

void
rb_gc_mark_values(long n, const VALUE *values)
{
    for (long i = 0; i < n; i++) {
        rb_gc_impl_mark(rb_gc_get_objspace(), values[i]);
    }
}

void
rb_gc_mark_vm_stack_values(long n, const VALUE *values)
{
    for (long i = 0; i < n; i++) {
        rb_gc_impl_mark_and_pin(rb_gc_get_objspace(), values[i]);
    }
}

static int
mark_key(st_data_t key, st_data_t value, st_data_t data)
{
    void *objspace = (void *)data;

    rb_gc_impl_mark_and_pin(objspace, (VALUE)key);

    return ST_CONTINUE;
}

void
rb_mark_set(st_table *tbl)
{
    if (!tbl) return;

    st_foreach(tbl, mark_key, (st_data_t)rb_gc_get_objspace());
}

static int
mark_keyvalue(st_data_t key, st_data_t value, st_data_t data)
{
    void *objspace = (void *)data;

    rb_gc_impl_mark(objspace, (VALUE)key);
    rb_gc_impl_mark(objspace, (VALUE)value);

    return ST_CONTINUE;
}

static int
pin_key_pin_value(st_data_t key, st_data_t value, st_data_t data)
{
    void *objspace = (void *)data;

    rb_gc_impl_mark_and_pin(objspace, (VALUE)key);
    rb_gc_impl_mark_and_pin(objspace, (VALUE)value);

    return ST_CONTINUE;
}

static int
pin_key_mark_value(st_data_t key, st_data_t value, st_data_t data)
{
    void *objspace = (void *)data;

    rb_gc_impl_mark_and_pin(objspace, (VALUE)key);
    rb_gc_impl_mark(objspace, (VALUE)value);

    return ST_CONTINUE;
}

static void
mark_hash(void *objspace, VALUE hash)
{
    if (rb_hash_compare_by_id_p(hash)) {
        rb_hash_stlike_foreach(hash, pin_key_mark_value, (st_data_t)objspace);
    }
    else {
        rb_hash_stlike_foreach(hash, mark_keyvalue, (st_data_t)objspace);
    }

    rb_gc_impl_mark(objspace, RHASH(hash)->ifnone);
}

void
rb_mark_hash(st_table *tbl)
{
    if (!tbl) return;

    st_foreach(tbl, pin_key_pin_value, (st_data_t)rb_gc_get_objspace());
}

static enum rb_id_table_iterator_result
mark_method_entry_i(VALUE me, void *objspace)
{
    rb_gc_impl_mark(objspace, me);

    return ID_TABLE_CONTINUE;
}

static void
mark_m_tbl(void *objspace, struct rb_id_table *tbl)
{
    if (tbl) {
        rb_id_table_foreach_values(tbl, mark_method_entry_i, objspace);
    }
}

#if STACK_GROW_DIRECTION < 0
#define GET_STACK_BOUNDS(start, end, appendix) ((start) = STACK_END, (end) = STACK_START)
#elif STACK_GROW_DIRECTION > 0
#define GET_STACK_BOUNDS(start, end, appendix) ((start) = STACK_START, (end) = STACK_END+(appendix))
#else
#define GET_STACK_BOUNDS(start, end, appendix) \
    ((STACK_END < STACK_START) ? \
     ((start) = STACK_END, (end) = STACK_START) : ((start) = STACK_START, (end) = STACK_END+(appendix)))
#endif

static void
each_stack_location(void *objspace, const rb_execution_context_t *ec,
                     const VALUE *stack_start, const VALUE *stack_end, void (*cb)(void *objspace, VALUE obj))
{

    gc_mark_locations(objspace, stack_start, stack_end, cb);

#if defined(__mc68000__)
    gc_mark_locations(objspace,
                      (VALUE*)((char*)stack_start + 2),
                      (VALUE*)((char*)stack_end - 2), cb);
#endif
}

static void
gc_mark_machine_stack_location_maybe(void *objspace, VALUE obj)
{
    rb_gc_impl_mark_maybe(objspace, obj);

#ifdef RUBY_ASAN_ENABLED
    rb_execution_context_t *ec = objspace->marking_machine_context_ec;
    void *fake_frame_start;
    void *fake_frame_end;
    bool is_fake_frame = asan_get_fake_stack_extents(
        ec->thread_ptr->asan_fake_stack_handle, obj,
        ec->machine.stack_start, ec->machine.stack_end,
        &fake_frame_start, &fake_frame_end
    );
    if (is_fake_frame) {
        each_stack_location(objspace, ec, fake_frame_start, fake_frame_end, gc_mark_maybe);
    }
#endif
}

#if defined(__wasm__)


static VALUE *rb_stack_range_tmp[2];

static void
rb_mark_locations(void *begin, void *end)
{
    rb_stack_range_tmp[0] = begin;
    rb_stack_range_tmp[1] = end;
}

# if defined(__EMSCRIPTEN__)

static void
mark_current_machine_context(rb_objspace_t *objspace, rb_execution_context_t *ec)
{
    emscripten_scan_stack(rb_mark_locations);
    each_stack_location(objspace, ec, rb_stack_range_tmp[0], rb_stack_range_tmp[1], gc_mark_maybe);

    emscripten_scan_registers(rb_mark_locations);
    each_stack_location(objspace, ec, rb_stack_range_tmp[0], rb_stack_range_tmp[1], gc_mark_maybe);
}
# else // use Asyncify version

static void
mark_current_machine_context(rb_objspace_t *objspace, rb_execution_context_t *ec)
{
    VALUE *stack_start, *stack_end;
    SET_STACK_END;
    GET_STACK_BOUNDS(stack_start, stack_end, 1);
    each_stack_location(objspace, ec, stack_start, stack_end, gc_mark_maybe);

    rb_wasm_scan_locals(rb_mark_locations);
    each_stack_location(objspace, ec, rb_stack_range_tmp[0], rb_stack_range_tmp[1], gc_mark_maybe);
}

# endif

#else // !defined(__wasm__)

static void
mark_current_machine_context(void *objspace, rb_execution_context_t *ec)
{
    union {
        rb_jmp_buf j;
        VALUE v[sizeof(rb_jmp_buf) / (sizeof(VALUE))];
    } save_regs_gc_mark;
    VALUE *stack_start, *stack_end;

    FLUSH_REGISTER_WINDOWS;
    memset(&save_regs_gc_mark, 0, sizeof(save_regs_gc_mark));
    /* This assumes that all registers are saved into the jmp_buf (and stack) */
    rb_setjmp(save_regs_gc_mark.j);

    /* SET_STACK_END must be called in this function because
     * the stack frame of this function may contain
     * callee save registers and they should be marked. */
    SET_STACK_END;
    GET_STACK_BOUNDS(stack_start, stack_end, 1);

#ifdef RUBY_ASAN_ENABLED
    objspace->marking_machine_context_ec = ec;
#endif

    each_location(objspace, save_regs_gc_mark.v, numberof(save_regs_gc_mark.v), gc_mark_machine_stack_location_maybe);
    each_stack_location(objspace, ec, stack_start, stack_end, gc_mark_machine_stack_location_maybe);

#ifdef RUBY_ASAN_ENABLED
    objspace->marking_machine_context_ec = NULL;
#endif
}
#endif

void
rb_gc_mark_machine_stack(const rb_execution_context_t *ec)
{
    VALUE *stack_start, *stack_end;
    GET_STACK_BOUNDS(stack_start, stack_end, 0);
    RUBY_DEBUG_LOG("ec->th:%u stack_start:%p stack_end:%p", rb_ec_thread_ptr(ec)->serial, stack_start, stack_end);

    rb_gc_mark_locations(stack_start, stack_end);
}

static int
rb_mark_tbl_i(st_data_t key, st_data_t value, st_data_t data)
{
    void *objspace = (void *)data;

    rb_gc_impl_mark_and_pin(objspace, (VALUE)value);

    return ST_CONTINUE;
}

void
rb_mark_tbl(st_table *tbl)
{
    if (!tbl || tbl->num_entries == 0) return;

    st_foreach(tbl, rb_mark_tbl_i, (st_data_t)rb_gc_get_objspace());
}

static int
gc_mark_tbl_no_pin_i(st_data_t key, st_data_t value, st_data_t data)
{
    void *objspace = (void *)data;

    rb_gc_impl_mark(objspace, (VALUE)value);

    return ST_CONTINUE;
}

static void
gc_mark_tbl_no_pin(void *objspace, st_table *tbl)
{
    if (!tbl || tbl->num_entries == 0) return;

    st_foreach(tbl, gc_mark_tbl_no_pin_i, (st_data_t)objspace);
}

void
rb_mark_tbl_no_pin(st_table *tbl)
{
    gc_mark_tbl_no_pin(rb_gc_get_objspace(), tbl);
}

void
rb_gc_mark_maybe(VALUE obj)
{
    rb_gc_impl_mark_maybe(rb_gc_get_objspace(), obj);
}

static enum rb_id_table_iterator_result
mark_cvc_tbl_i(VALUE cvc_entry, void *objspace)
{
    struct rb_cvar_class_tbl_entry *entry;

    entry = (struct rb_cvar_class_tbl_entry *)cvc_entry;

    RUBY_ASSERT(entry->cref == 0 || (BUILTIN_TYPE((VALUE)entry->cref) == T_IMEMO && IMEMO_TYPE_P(entry->cref, imemo_cref)));
    rb_gc_impl_mark(objspace, (VALUE)entry->cref);

    return ID_TABLE_CONTINUE;
}

static void
mark_cvc_tbl(void *objspace, VALUE klass)
{
    struct rb_id_table *tbl = RCLASS_CVC_TBL(klass);
    if (tbl) {
        rb_id_table_foreach_values(tbl, mark_cvc_tbl_i, objspace);
    }
}

static void reachable_objects_from_callback(VALUE obj);

void
rb_gc_mark_movable(VALUE obj)
{
    rb_gc_impl_mark(rb_gc_get_objspace(), obj);
}

void
rb_gc_mark(VALUE obj)
{
    rb_gc_impl_mark_and_pin(rb_gc_get_objspace(), obj);
}

void
rb_gc_mark_and_move(VALUE *ptr)
{
    rb_gc_impl_mark_and_move(rb_gc_get_objspace(), ptr);
}

void
rb_gc_mark_weak(VALUE *ptr)
{
    rb_gc_impl_mark_weak(rb_gc_get_objspace(), ptr);
}

void
rb_gc_remove_weak(VALUE parent_obj, VALUE *ptr)
{
    rb_gc_impl_remove_weak(rb_gc_get_objspace(), parent_obj, ptr);
}

static bool
gc_declarative_marking_p(const rb_data_type_t *type)
{
    return (type->flags & RUBY_TYPED_DECL_MARKING) != 0;
}

static enum rb_id_table_iterator_result
mark_const_table_i(VALUE value, void *objspace)
{
    const rb_const_entry_t *ce = (const rb_const_entry_t *)value;

    rb_gc_impl_mark(objspace, ce->value);
    rb_gc_impl_mark(objspace, ce->file);

    return ID_TABLE_CONTINUE;
}

void
rb_gc_mark_children(void *objspace, VALUE obj)
{
    if (FL_TEST(obj, FL_EXIVAR)) {
        rb_mark_generic_ivar(obj);
    }

    switch (BUILTIN_TYPE(obj)) {
      case T_FLOAT:
      case T_BIGNUM:
      case T_SYMBOL:
        /* Not immediates, but does not have references and singleton class.
         *
         * RSYMBOL(obj)->fstr intentionally not marked. See log for 96815f1e
         * ("symbol.c: remove rb_gc_mark_symbols()") */
        return;

      case T_NIL:
      case T_FIXNUM:
        rb_bug("rb_gc_mark() called for broken object");
        break;

      case T_NODE:
        UNEXPECTED_NODE(rb_gc_mark);
        break;

      case T_IMEMO:
        rb_imemo_mark_and_move(obj, false);
        return;

      default:
        break;
    }

    rb_gc_impl_mark(objspace, RBASIC(obj)->klass);

    switch (BUILTIN_TYPE(obj)) {
      case T_CLASS:
        if (FL_TEST(obj, FL_SINGLETON)) {
            rb_gc_impl_mark(objspace, RCLASS_ATTACHED_OBJECT(obj));
        }
        // Continue to the shared T_CLASS/T_MODULE
      case T_MODULE:
        if (RCLASS_SUPER(obj)) {
            rb_gc_impl_mark(objspace, RCLASS_SUPER(obj));
        }

        mark_m_tbl(objspace, RCLASS_M_TBL(obj));
        mark_cvc_tbl(objspace, obj);
        rb_cc_table_mark(obj);
        if (rb_shape_obj_too_complex(obj)) {
            gc_mark_tbl_no_pin(objspace, (st_table *)RCLASS_IVPTR(obj));
        }
        else {
            for (attr_index_t i = 0; i < RCLASS_IV_COUNT(obj); i++) {
                rb_gc_impl_mark(objspace, RCLASS_IVPTR(obj)[i]);
            }
        }

        if (RCLASS_CONST_TBL(obj)) {
            rb_id_table_foreach_values(RCLASS_CONST_TBL(obj), mark_const_table_i, objspace);
        }

        rb_gc_impl_mark(objspace, RCLASS_EXT(obj)->classpath);
        break;

      case T_ICLASS:
        if (RICLASS_OWNS_M_TBL_P(obj)) {
            mark_m_tbl(objspace, RCLASS_M_TBL(obj));
        }
        if (RCLASS_SUPER(obj)) {
            rb_gc_impl_mark(objspace, RCLASS_SUPER(obj));
        }

        if (RCLASS_INCLUDER(obj)) {
            rb_gc_impl_mark(objspace, RCLASS_INCLUDER(obj));
        }
        mark_m_tbl(objspace, RCLASS_CALLABLE_M_TBL(obj));
        rb_cc_table_mark(obj);
        break;

      case T_ARRAY:
        if (ARY_SHARED_P(obj)) {
            VALUE root = ARY_SHARED_ROOT(obj);
            rb_gc_impl_mark(objspace, root);
        }
        else {
            long i, len = RARRAY_LEN(obj);
            const VALUE *ptr = RARRAY_CONST_PTR(obj);
            for (long i = 0; i < len; i++) {
                rb_gc_impl_mark(objspace, ptr[i]);
            }
        }
        break;

      case T_HASH:
        mark_hash(objspace, obj);
        break;

      case T_STRING:
        if (STR_SHARED_P(obj)) {
            if (STR_EMBED_P(RSTRING(obj)->as.heap.aux.shared)) {
                /* Embedded shared strings cannot be moved because this string
                 * points into the slot of the shared string. There may be code
                 * using the RSTRING_PTR on the stack, which would pin this
                 * string but not pin the shared string, causing it to move. */
                rb_gc_impl_mark_and_pin(objspace, RSTRING(obj)->as.heap.aux.shared);
            }
            else {
                rb_gc_impl_mark(objspace, RSTRING(obj)->as.heap.aux.shared);
            }
        }
        break;

      case T_DATA: {
        void *const ptr = RTYPEDDATA_P(obj) ? RTYPEDDATA_GET_DATA(obj) : DATA_PTR(obj);

        if (ptr) {
            if (RTYPEDDATA_P(obj) && gc_declarative_marking_p(RTYPEDDATA(obj)->type)) {
                size_t *offset_list = (size_t *)RTYPEDDATA(obj)->type->function.dmark;

                for (size_t offset = *offset_list; offset != RUBY_REF_END; offset = *offset_list++) {
                    rb_gc_impl_mark(objspace, *(VALUE *)((char *)ptr + offset));
                }
            }
            else {
                RUBY_DATA_FUNC mark_func = RTYPEDDATA_P(obj) ?
                    RTYPEDDATA(obj)->type->function.dmark :
                    RDATA(obj)->dmark;
                if (mark_func) (*mark_func)(ptr);
            }
        }

        break;
      }

      case T_OBJECT: {
        rb_shape_t *shape = rb_shape_get_shape_by_id(ROBJECT_SHAPE_ID(obj));

        if (rb_shape_obj_too_complex(obj)) {
            mark_tbl_no_pin(objspace, ROBJECT_IV_HASH(obj));
        }
        else {
            const VALUE * const ptr = ROBJECT_IVPTR(obj);

            uint32_t len = ROBJECT_IV_COUNT(obj);
            for (uint32_t i = 0; i < len; i++) {
                rb_gc_impl_mark(objspace, ptr[i]);
            }
        }

        if (shape) {
            VALUE klass = RBASIC_CLASS(obj);

            // Increment max_iv_count if applicable, used to determine size pool allocation
            attr_index_t num_of_ivs = shape->next_iv_index;
            if (RCLASS_EXT(klass)->max_iv_count < num_of_ivs) {
                RCLASS_EXT(klass)->max_iv_count = num_of_ivs;
            }
        }

        break;
      }

      case T_FILE:
        if (RFILE(obj)->fptr) {
            rb_gc_impl_mark(objspace, RFILE(obj)->fptr->self);
            rb_gc_impl_mark(objspace, RFILE(obj)->fptr->pathv);
            rb_gc_impl_mark(objspace, RFILE(obj)->fptr->tied_io_for_writing);
            rb_gc_impl_mark(objspace, RFILE(obj)->fptr->writeconv_asciicompat);
            rb_gc_impl_mark(objspace, RFILE(obj)->fptr->writeconv_pre_ecopts);
            rb_gc_impl_mark(objspace, RFILE(obj)->fptr->encs.ecopts);
            rb_gc_impl_mark(objspace, RFILE(obj)->fptr->write_lock);
            rb_gc_impl_mark(objspace, RFILE(obj)->fptr->timeout);
        }
        break;

      case T_REGEXP:
        rb_gc_impl_mark(objspace, RREGEXP(obj)->src);
        break;

      case T_MATCH:
        rb_gc_impl_mark(objspace, RMATCH(obj)->regexp);
        if (RMATCH(obj)->str) {
            rb_gc_impl_mark(objspace, RMATCH(obj)->str);
        }
        break;

      case T_RATIONAL:
        rb_gc_impl_mark(objspace, RRATIONAL(obj)->num);
        rb_gc_impl_mark(objspace, RRATIONAL(obj)->den);
        break;

      case T_COMPLEX:
        rb_gc_impl_mark(objspace, RCOMPLEX(obj)->real);
        rb_gc_impl_mark(objspace, RCOMPLEX(obj)->imag);
        break;

      case T_STRUCT: {
        const long len = RSTRUCT_LEN(obj);
        const VALUE * const ptr = RSTRUCT_CONST_PTR(obj);

        for (long i = 0; i < len; i++) {
            rb_gc_impl_mark(objspace, ptr[i]);
        }

        break;
      }

      default:
#if GC_DEBUG
        rb_gcdebug_print_obj_condition((VALUE)obj);
#endif
        if (BUILTIN_TYPE(obj) == T_MOVED)   rb_bug("rb_gc_mark(): %p is T_MOVED", (void *)obj);
        if (BUILTIN_TYPE(obj) == T_NONE)   rb_bug("rb_gc_mark(): %p is T_NONE", (void *)obj);
        if (BUILTIN_TYPE(obj) == T_ZOMBIE) rb_bug("rb_gc_mark(): %p is T_ZOMBIE", (void *)obj);
        rb_bug("rb_gc_mark(): unknown data type 0x%x(%p) %s",
               BUILTIN_TYPE(obj), (void *)obj,
               is_pointer_to_heap(objspace, (void *)obj) ? "corrupted object" : "non object");
    }
}

static int
pin_value(st_data_t key, st_data_t value, st_data_t data)
{
    void *objspace = (void *)data;

    rb_gc_impl_mark_and_pin(objspace, (VALUE)value);

    return ST_CONTINUE;
}

void
rb_gc_mark_roots(void *objspace)
{
    rb_execution_context_t *ec = GET_EC();
    rb_vm_t *vm = rb_ec_vm_ptr(ec);

#if PRINT_ROOT_TICKS
    tick_t start_tick = tick();
    int tick_count = 0;
    const char *prev_category = 0;

    if (mark_ticks_categories[0] == 0) {
        atexit(show_mark_ticks);
    }
#endif

    // objspace->rgengc.parent_object = Qfalse;

#if PRINT_ROOT_TICKS
#define MARK_CHECKPOINT_PRINT_TICK(category) do { \
    if (prev_category) { \
        tick_t t = tick(); \
        mark_ticks[tick_count] = t - start_tick; \
        mark_ticks_categories[tick_count] = prev_category; \
        tick_count++; \
    } \
    prev_category = category; \
    start_tick = tick(); \
} while (0)
#else /* PRINT_ROOT_TICKS */
#define MARK_CHECKPOINT_PRINT_TICK(category)
#endif

#define MARK_CHECKPOINT(category) do { \
    if (categoryp) *categoryp = category; \
    MARK_CHECKPOINT_PRINT_TICK(category); \
} while (0)

    // MARK_CHECKPOINT("vm");
    SET_STACK_END;
    rb_vm_mark(vm);
    if (vm->self) rb_gc_impl_mark(objspace, vm->self);

    // MARK_CHECKPOINT("finalizers");
    if (finalizer_table != NULL) {
        st_foreach(finalizer_table, pin_value, (st_data_t)objspace);
    }

    // MARK_CHECKPOINT("machine_context");
    mark_current_machine_context(objspace, ec);

    // MARK_CHECKPOINT("end_proc");
    rb_mark_end_proc();

    // MARK_CHECKPOINT("global_tbl");
    rb_gc_mark_global_tbl();

    // MARK_CHECKPOINT("object_id");
    rb_gc_mark(objspace->next_object_id);
    mark_tbl_no_pin(objspace, objspace->obj_to_id_tbl); /* Only mark ids */

    if (stress_to_class) rb_gc_mark(stress_to_class);

    // MARK_CHECKPOINT("finish");
#undef MARK_CHECKPOINT
}

size_t
rb_gc_obj_optimal_size(VALUE obj)
{
    switch (BUILTIN_TYPE(obj)) {
      case T_ARRAY:
        return rb_ary_size_as_embedded(obj);

      case T_OBJECT:
        if (rb_shape_obj_too_complex(obj)) {
            return sizeof(struct RObject);
        }
        else {
            return rb_obj_embedded_size(ROBJECT_IV_CAPACITY(obj));
        }

      case T_STRING:
        return rb_str_size_as_embedded(obj);

      case T_HASH:
        return sizeof(struct RHash) + (RHASH_ST_TABLE_P(obj) ? sizeof(st_table) : sizeof(ar_table));

      default:
        return 0;
    }
}

void
rb_gc_writebarrier(VALUE a, VALUE b)
{
    rb_gc_impl_writebarrier(rb_gc_get_objspace(), a, b);
}

void
rb_gc_writebarrier_unprotect(VALUE obj)
{
    rb_gc_impl_writebarrier_unprotect(rb_gc_get_objspace(), obj);
}

/*
 * remember `obj' if needed.
 */
void
rb_gc_writebarrier_remember(VALUE obj)
{
    rb_gc_impl_writebarrier_remember(rb_gc_get_objspace(), obj);
}

void
rb_gc_copy_attributes(VALUE dest, VALUE obj)
{
    if (RVALUE_WB_UNPROTECTED(obj)) {
        rb_gc_writebarrier_unprotect(dest);
    }
    rb_gc_copy_finalizer(dest, obj);
}

// TODO: rearchitect this function to work for a generic GC
size_t
rb_obj_gc_flags(VALUE obj, ID* flags, size_t max)
{
    return rb_gc_impl_obj_flags(obj, flags, max);
}

/* GC */

void *
rb_gc_ractor_cache_alloc(void)
{
    return rb_gc_impl_ractor_cache_alloc(rb_gc_get_objspace());
}

void
rb_gc_ractor_cache_free(void *cache)
{
    rb_gc_impl_ractor_cache_free(rb_gc_get_objspace(), cache);
}

void
rb_gc_force_recycle(VALUE obj)
{
    /* no-op */
}

void
rb_gc_register_mark_object(VALUE obj)
{
    if (!rb_gc_impl_is_pointer_to_heap(rb_gc_get_objspace(), (void *)obj))
        return;

    rb_vm_register_global_object(obj);
}

void
rb_gc_register_address(VALUE *addr)
{
    rb_vm_t *vm = GET_VM();

    VALUE obj = *addr;

    struct global_object_list *tmp = ALLOC(struct global_object_list);
    tmp->next = vm->global_object_list;
    tmp->varptr = addr;
    vm->global_object_list = tmp;

    /*
     * Because some C extensions have assignment-then-register bugs,
     * we guard `obj` here so that it would not get swept defensively.
     */
    RB_GC_GUARD(obj);
    if (0 && !SPECIAL_CONST_P(obj)) {
        rb_warn("Object is assigned to registering address already: %"PRIsVALUE,
                rb_obj_class(obj));
        rb_print_backtrace(stderr);
    }
}

void
rb_gc_unregister_address(VALUE *addr)
{
    rb_vm_t *vm = GET_VM();
    struct global_object_list *tmp = vm->global_object_list;

    if (tmp->varptr == addr) {
        vm->global_object_list = tmp->next;
        xfree(tmp);
        return;
    }
    while (tmp->next) {
        if (tmp->next->varptr == addr) {
            struct global_object_list *t = tmp->next;

            tmp->next = tmp->next->next;
            xfree(t);
            break;
        }
        tmp = tmp->next;
    }
}

void
rb_global_variable(VALUE *var)
{
    rb_gc_register_address(var);
}

static VALUE
gc_start_internal(rb_execution_context_t *ec, VALUE self, VALUE full_mark, VALUE immediate_mark, VALUE immediate_sweep, VALUE compact)
{
    rb_gc_impl_start(rb_gc_get_objspace(), RTEST(full_mark), RTEST(immediate_mark), RTEST(immediate_sweep), RTEST(compact));

    return Qnil;
}

enum {
    gc_stress_no_major,
    gc_stress_no_immediate_sweep,
    gc_stress_full_mark_after_malloc,
    gc_stress_max
};

#define gc_stress_full_mark_after_malloc_p() \
    (FIXNUM_P(ruby_gc_stress_mode) && (FIX2LONG(ruby_gc_stress_mode) & (1<<gc_stress_full_mark_after_malloc)))

/*
 * rb_objspace_each_objects() is special C API to walk through
 * Ruby object space.  This C API is too difficult to use it.
 * To be frank, you should not use it. Or you need to read the
 * source code of this function and understand what this function does.
 *
 * 'callback' will be called several times (the number of heap page,
 * at current implementation) with:
 *   vstart: a pointer to the first living object of the heap_page.
 *   vend: a pointer to next to the valid heap_page area.
 *   stride: a distance to next VALUE.
 *
 * If callback() returns non-zero, the iteration will be stopped.
 *
 * This is a sample callback code to iterate liveness objects:
 *
 *   static int
 *   sample_callback(void *vstart, void *vend, int stride, void *data)
 *   {
 *       VALUE v = (VALUE)vstart;
 *       for (; v != (VALUE)vend; v += stride) {
 *           if (!rb_objspace_internal_object_p(v)) { // liveness check
 *               // do something with live object 'v'
 *           }
 *       }
 *       return 0; // continue to iteration
 *   }
 *
 * Note: 'vstart' is not a top of heap_page.  This point the first
 *       living object to grasp at least one object to avoid GC issue.
 *       This means that you can not walk through all Ruby object page
 *       including freed object page.
 *
 * Note: On this implementation, 'stride' is the same as sizeof(RVALUE).
 *       However, there are possibilities to pass variable values with
 *       'stride' with some reasons.  You must use stride instead of
 *       use some constant value in the iteration.
 */
void
rb_objspace_each_objects(void *objspace_ptr, each_obj_callback *callback, void *data)
{
    rb_gc_impl_each_objects(objspace_ptr, callback, data);
}

static void
gc_ref_update_array(void *objspace, VALUE v)
{
    if (ARY_SHARED_P(v)) {
        VALUE old_root = RARRAY(v)->as.heap.aux.shared_root;

        UPDATE_IF_MOVED(objspace, RARRAY(v)->as.heap.aux.shared_root);

        VALUE new_root = RARRAY(v)->as.heap.aux.shared_root;
        // If the root is embedded and its location has changed
        if (ARY_EMBED_P(new_root) && new_root != old_root) {
            size_t offset = (size_t)(RARRAY(v)->as.heap.ptr - RARRAY(old_root)->as.ary);
            GC_ASSERT(RARRAY(v)->as.heap.ptr >= RARRAY(old_root)->as.ary);
            RARRAY(v)->as.heap.ptr = RARRAY(new_root)->as.ary + offset;
        }
    }
    else {
        long len = RARRAY_LEN(v);

        if (len > 0) {
            VALUE *ptr = (VALUE *)RARRAY_CONST_PTR(v);
            for (long i = 0; i < len; i++) {
                UPDATE_IF_MOVED(objspace, ptr[i]);
            }
        }

        if (rb_gc_obj_slot_size(v) >= rb_ary_size_as_embedded(v)) {
            if (rb_ary_embeddable_p(v)) {
                rb_ary_make_embedded(v);
            }
        }
    }
}

static void gc_ref_update_table_values_only(void *objspace, st_table *tbl);

static void
gc_ref_update_object(void *objspace, VALUE v)
{
    VALUE *ptr = ROBJECT_IVPTR(v);

    if (rb_shape_obj_too_complex(v)) {
        gc_ref_update_table_values_only(objspace, ROBJECT_IV_HASH(v));
        return;
    }

    size_t slot_size = rb_gc_obj_slot_size(v);
    size_t embed_size = rb_obj_embedded_size(ROBJECT_IV_CAPACITY(v));
    if (slot_size >= embed_size && !RB_FL_TEST_RAW(v, ROBJECT_EMBED)) {
        // Object can be re-embedded
        memcpy(ROBJECT(v)->as.ary, ptr, sizeof(VALUE) * ROBJECT_IV_COUNT(v));
        RB_FL_SET_RAW(v, ROBJECT_EMBED);
        xfree(ptr);
        ptr = ROBJECT(v)->as.ary;
    }

    for (uint32_t i = 0; i < ROBJECT_IV_COUNT(v); i++) {
        UPDATE_IF_MOVED(objspace, ptr[i]);
    }
}

static int
hash_replace_ref(st_data_t *key, st_data_t *value, st_data_t argp, int existing)
{
    void *objspace = (void *)argp;

    if (gc_object_moved_p(objspace, (VALUE)*key)) {
        *key = rb_gc_impl_location(objspace, (VALUE)*key);
    }

    if (gc_object_moved_p(objspace, (VALUE)*value)) {
        *value = rb_gc_impl_location(objspace, (VALUE)*value);
    }

    return ST_CONTINUE;
}

static int
hash_foreach_replace(st_data_t key, st_data_t value, st_data_t argp, int error)
{
    void *objspace;

    objspace = (void *)argp;

    if (gc_object_moved_p(objspace, (VALUE)key)) {
        return ST_REPLACE;
    }

    if (gc_object_moved_p(objspace, (VALUE)value)) {
        return ST_REPLACE;
    }
    return ST_CONTINUE;
}

static int
hash_replace_ref_value(st_data_t *key, st_data_t *value, st_data_t argp, int existing)
{
    void *objspace = (void *)argp;

    if (gc_object_moved_p(objspace, (VALUE)*value)) {
        *value = rb_gc_impl_location(objspace, (VALUE)*value);
    }

    return ST_CONTINUE;
}

static int
hash_foreach_replace_value(st_data_t key, st_data_t value, st_data_t argp, int error)
{
    void *objspace;

    objspace = (void *)argp;

    if (gc_object_moved_p(objspace, (VALUE)value)) {
        return ST_REPLACE;
    }
    return ST_CONTINUE;
}

static void
gc_ref_update_table_values_only(void *objspace, st_table *tbl)
{
    if (!tbl || tbl->num_entries == 0) return;

    if (st_foreach_with_replace(tbl, hash_foreach_replace_value, hash_replace_ref_value, (st_data_t)objspace)) {
        rb_raise(rb_eRuntimeError, "hash modified during iteration");
    }
}

void
rb_gc_ref_update_table_values_only(st_table *tbl)
{
    gc_ref_update_table_values_only(rb_gc_get_objspace(), tbl);
}

static void
gc_update_table_refs(void *objspace, st_table *tbl)
{
    if (!tbl || tbl->num_entries == 0) return;

    if (st_foreach_with_replace(tbl, hash_foreach_replace, hash_replace_ref, (st_data_t)objspace)) {
        rb_raise(rb_eRuntimeError, "hash modified during iteration");
    }
}

/* Update MOVED references in a VALUE=>VALUE st_table */
void
rb_gc_update_tbl_refs(st_table *ptr)
{
    gc_update_table_refs(rb_gc_get_objspace(), ptr);
}

static void
gc_ref_update_hash(void *objspace, VALUE v)
{
    rb_hash_stlike_foreach_with_replace(v, hash_foreach_replace, hash_replace_ref, (st_data_t)objspace);
}

static void
gc_update_values(void *objspace, long n, VALUE *values)
{
    for (long i = 0; i < n; i++) {
        UPDATE_IF_MOVED(objspace, values[i]);
    }
}

void
rb_gc_update_values(long n, VALUE *values)
{
    gc_update_values(rb_gc_get_objspace(), n, values);
}

static enum rb_id_table_iterator_result
check_id_table_move(VALUE value, void *data)
{
    void *objspace = (void *)data;

    if (gc_object_moved_p(objspace, (VALUE)value)) {
        return ID_TABLE_REPLACE;
    }

    return ID_TABLE_CONTINUE;
}

static enum rb_id_table_iterator_result
update_id_table(VALUE *value, void *data, int existing)
{
    void *objspace = (void *)data;

    if (gc_object_moved_p(objspace, (VALUE)*value)) {
        *value = rb_gc_impl_location(objspace, (VALUE)*value);
    }

    return ID_TABLE_CONTINUE;
}

static void
update_m_tbl(void *objspace, struct rb_id_table *tbl)
{
    if (tbl) {
        rb_id_table_foreach_values_with_replace(tbl, check_id_table_move, update_id_table, objspace);
    }
}

static enum rb_id_table_iterator_result
update_cc_tbl_i(VALUE ccs_ptr, void *objspace)
{
    struct rb_class_cc_entries *ccs = (struct rb_class_cc_entries *)ccs_ptr;
    VM_ASSERT(vm_ccs_p(ccs));

    if (gc_object_moved_p(objspace, (VALUE)ccs->cme)) {
        ccs->cme = (const rb_callable_method_entry_t *)rb_gc_impl_location(objspace, (VALUE)ccs->cme);
    }

    for (int i=0; i<ccs->len; i++) {
        if (gc_object_moved_p(objspace, (VALUE)ccs->entries[i].ci)) {
            ccs->entries[i].ci = (struct rb_callinfo *)rb_gc_impl_location(objspace, (VALUE)ccs->entries[i].ci);
        }
        if (gc_object_moved_p(objspace, (VALUE)ccs->entries[i].cc)) {
            ccs->entries[i].cc = (struct rb_callcache *)rb_gc_impl_location(objspace, (VALUE)ccs->entries[i].cc);
        }
    }

    // do not replace
    return ID_TABLE_CONTINUE;
}

static void
update_cc_tbl(void *objspace, VALUE klass)
{
    struct rb_id_table *tbl = RCLASS_CC_TBL(klass);
    if (tbl) {
        rb_id_table_foreach_values(tbl, update_cc_tbl_i, objspace);
    }
}

static enum rb_id_table_iterator_result
update_cvc_tbl_i(VALUE cvc_entry, void *objspace)
{
    struct rb_cvar_class_tbl_entry *entry;

    entry = (struct rb_cvar_class_tbl_entry *)cvc_entry;

    if (entry->cref) {
        TYPED_UPDATE_IF_MOVED(objspace, rb_cref_t *, entry->cref);
    }

    entry->class_value = rb_gc_impl_location(objspace, entry->class_value);

    return ID_TABLE_CONTINUE;
}

static void
update_cvc_tbl(void *objspace, VALUE klass)
{
    struct rb_id_table *tbl = RCLASS_CVC_TBL(klass);
    if (tbl) {
        rb_id_table_foreach_values(tbl, update_cvc_tbl_i, objspace);
    }
}

static enum rb_id_table_iterator_result
update_const_table(VALUE value, void *objspace)
{
    rb_const_entry_t *ce = (rb_const_entry_t *)value;

    if (gc_object_moved_p(objspace, ce->value)) {
        ce->value = rb_gc_impl_location(objspace, ce->value);
    }

    if (gc_object_moved_p(objspace, ce->file)) {
        ce->file = rb_gc_impl_location(objspace, ce->file);
    }

    return ID_TABLE_CONTINUE;
}

static void
update_const_tbl(void *objspace, struct rb_id_table *tbl)
{
    if (!tbl) return;
    rb_id_table_foreach_values(tbl, update_const_table, objspace);
}

static void
update_subclass_entries(void *objspace, rb_subclass_entry_t *entry)
{
    while (entry) {
        UPDATE_IF_MOVED(objspace, entry->klass);
        entry = entry->next;
    }
}

static void
update_class_ext(void *objspace, rb_classext_t *ext)
{
    UPDATE_IF_MOVED(objspace, ext->origin_);
    UPDATE_IF_MOVED(objspace, ext->includer);
    UPDATE_IF_MOVED(objspace, ext->refined_class);
    update_subclass_entries(objspace, ext->subclasses);
}

static void
update_superclasses(void *objspace, VALUE obj)
{
    if (FL_TEST_RAW(obj, RCLASS_SUPERCLASSES_INCLUDE_SELF)) {
        for (size_t i = 0; i < RCLASS_SUPERCLASS_DEPTH(obj) + 1; i++) {
            UPDATE_IF_MOVED(objspace, RCLASS_SUPERCLASSES(obj)[i]);
        }
    }
}

void
rb_gc_update_object_references(void *objspace, VALUE obj)
{
    RVALUE *any = (RVALUE *)obj;

    gc_report(4, objspace, "update-refs: %p ->\n", (void *)obj);

    if (FL_TEST(obj, FL_EXIVAR)) {
        rb_ref_update_generic_ivar(obj);
    }

    switch (BUILTIN_TYPE(obj)) {
      case T_CLASS:
        if (FL_TEST(obj, FL_SINGLETON)) {
            UPDATE_IF_MOVED(objspace, RCLASS_ATTACHED_OBJECT(obj));
        }
        // Continue to the shared T_CLASS/T_MODULE
      case T_MODULE:
        if (RCLASS_SUPER((VALUE)obj)) {
            UPDATE_IF_MOVED(objspace, RCLASS(obj)->super);
        }
        update_m_tbl(objspace, RCLASS_M_TBL(obj));
        update_cc_tbl(objspace, obj);
        update_cvc_tbl(objspace, obj);
        update_superclasses(objspace, obj);

        if (rb_shape_obj_too_complex(obj)) {
            gc_ref_update_table_values_only(objspace, RCLASS_IV_HASH(obj));
        }
        else {
            for (attr_index_t i = 0; i < RCLASS_IV_COUNT(obj); i++) {
                UPDATE_IF_MOVED(objspace, RCLASS_IVPTR(obj)[i]);
            }
        }

        update_class_ext(objspace, RCLASS_EXT(obj));
        update_const_tbl(objspace, RCLASS_CONST_TBL(obj));

        UPDATE_IF_MOVED(objspace, RCLASS_EXT(obj)->classpath);
        break;

      case T_ICLASS:
        if (RICLASS_OWNS_M_TBL_P(obj)) {
            update_m_tbl(objspace, RCLASS_M_TBL(obj));
        }
        if (RCLASS_SUPER((VALUE)obj)) {
            UPDATE_IF_MOVED(objspace, RCLASS(obj)->super);
        }
        update_class_ext(objspace, RCLASS_EXT(obj));
        update_m_tbl(objspace, RCLASS_CALLABLE_M_TBL(obj));
        update_cc_tbl(objspace, obj);
        break;

      case T_IMEMO:
        rb_imemo_mark_and_move(obj, true);
        return;

      case T_NIL:
      case T_FIXNUM:
      case T_NODE:
      case T_MOVED:
      case T_NONE:
        /* These can't move */
        return;

      case T_ARRAY:
        gc_ref_update_array(objspace, obj);
        break;

      case T_HASH:
        gc_ref_update_hash(objspace, obj);
        UPDATE_IF_MOVED(objspace, any->as.hash.ifnone);
        break;

      case T_STRING:
        {
            if (STR_SHARED_P(obj)) {
                UPDATE_IF_MOVED(objspace, any->as.string.as.heap.aux.shared);
            }

            /* If, after move the string is not embedded, and can fit in the
             * slot it's been placed in, then re-embed it. */
            if (rb_gc_obj_slot_size(obj) >= rb_str_size_as_embedded(obj)) {
                if (!STR_EMBED_P(obj) && rb_str_reembeddable_p(obj)) {
                    rb_str_make_embedded(obj);
                }
            }

            break;
        }
      case T_DATA:
        /* Call the compaction callback, if it exists */
        {
            void *const ptr = RTYPEDDATA_P(obj) ? RTYPEDDATA_GET_DATA(obj) : DATA_PTR(obj);
            if (ptr) {
                if (RTYPEDDATA_P(obj) && gc_declarative_marking_p(any->as.typeddata.type)) {
                    size_t *offset_list = (size_t *)RTYPEDDATA(obj)->type->function.dmark;

                    for (size_t offset = *offset_list; offset != RUBY_REF_END; offset = *offset_list++) {
                        VALUE *ref = (VALUE *)((char *)ptr + offset);
                        if (SPECIAL_CONST_P(*ref)) continue;
                        *ref = rb_gc_impl_location(objspace, *ref);
                    }
                }
                else if (RTYPEDDATA_P(obj)) {
                    RUBY_DATA_FUNC compact_func = any->as.typeddata.type->function.dcompact;
                    if (compact_func) (*compact_func)(ptr);
                }
            }
        }
        break;

      case T_OBJECT:
        gc_ref_update_object(objspace, obj);
        break;

      case T_FILE:
        if (any->as.file.fptr) {
            UPDATE_IF_MOVED(objspace, any->as.file.fptr->self);
            UPDATE_IF_MOVED(objspace, any->as.file.fptr->pathv);
            UPDATE_IF_MOVED(objspace, any->as.file.fptr->tied_io_for_writing);
            UPDATE_IF_MOVED(objspace, any->as.file.fptr->writeconv_asciicompat);
            UPDATE_IF_MOVED(objspace, any->as.file.fptr->writeconv_pre_ecopts);
            UPDATE_IF_MOVED(objspace, any->as.file.fptr->encs.ecopts);
            UPDATE_IF_MOVED(objspace, any->as.file.fptr->write_lock);
        }
        break;
      case T_REGEXP:
        UPDATE_IF_MOVED(objspace, any->as.regexp.src);
        break;

      case T_SYMBOL:
        if (DYNAMIC_SYM_P((VALUE)any)) {
            UPDATE_IF_MOVED(objspace, RSYMBOL(any)->fstr);
        }
        break;

      case T_FLOAT:
      case T_BIGNUM:
        break;

      case T_MATCH:
        UPDATE_IF_MOVED(objspace, any->as.match.regexp);

        if (any->as.match.str) {
            UPDATE_IF_MOVED(objspace, any->as.match.str);
        }
        break;

      case T_RATIONAL:
        UPDATE_IF_MOVED(objspace, any->as.rational.num);
        UPDATE_IF_MOVED(objspace, any->as.rational.den);
        break;

      case T_COMPLEX:
        UPDATE_IF_MOVED(objspace, any->as.complex.real);
        UPDATE_IF_MOVED(objspace, any->as.complex.imag);

        break;

      case T_STRUCT:
        {
            long i, len = RSTRUCT_LEN(obj);
            VALUE *ptr = (VALUE *)RSTRUCT_CONST_PTR(obj);

            for (i = 0; i < len; i++) {
                UPDATE_IF_MOVED(objspace, ptr[i]);
            }
        }
        break;
      default:
#if GC_DEBUG
        rb_gcdebug_print_obj_condition((VALUE)obj);
        rb_obj_info_dump(obj);
        rb_bug("unreachable");
#endif
        break;

    }

    UPDATE_IF_MOVED(objspace, RBASIC(obj)->klass);

    gc_report(4, objspace, "update-refs: %p <-\n", (void *)obj);
}

extern rb_symbols_t ruby_global_symbols;
#define global_symbols ruby_global_symbols

void
rb_gc_update_vm_references(void *objspace)
{
    rb_execution_context_t *ec = GET_EC();
    rb_vm_t *vm = rb_ec_vm_ptr(ec);

    rb_vm_update_references(vm);
    rb_gc_update_global_tbl();
    global_symbols.ids = rb_gc_impl_location(objspace, global_symbols.ids);
    global_symbols.dsymbol_fstr_hash = rb_gc_impl_location(objspace, global_symbols.dsymbol_fstr_hash);
    gc_update_table_refs(objspace, global_symbols.str_sym);
}

// TODO: move GC_CAN_COMPILE_COMPACTION into gc_impl.c
#if GC_CAN_COMPILE_COMPACTION
/*
 *  call-seq:
 *     GC.latest_compact_info -> hash
 *
 * Returns information about object moved in the most recent \GC compaction.
 *
 * The returned +hash+ contains the following keys:
 *
 * [considered]
 *   Hash containing the type of the object as the key and the number of
 *   objects of that type that were considered for movement.
 * [moved]
 *   Hash containing the type of the object as the key and the number of
 *   objects of that type that were actually moved.
 * [moved_up]
 *   Hash containing the type of the object as the key and the number of
 *   objects of that type that were increased in size.
 * [moved_down]
 *   Hash containing the type of the object as the key and the number of
 *   objects of that type that were decreased in size.
 *
 * Some objects can't be moved (due to pinning) so these numbers can be used to
 * calculate compaction efficiency.
 */
static VALUE
gc_compact_stats(VALUE self)
{
    return rb_gc_impl_compact_stats(rb_gc_get_objspace());
}
#else
#  define gc_compact_stats rb_f_notimplement
#endif

#if GC_CAN_COMPILE_COMPACTION
/*
 *  call-seq:
 *     GC.compact -> hash
 *
 * This function compacts objects together in Ruby's heap. It eliminates
 * unused space (or fragmentation) in the heap by moving objects in to that
 * unused space.
 *
 * The returned +hash+ contains statistics about the objects that were moved;
 * see GC.latest_compact_info.
 *
 * This method is only expected to work on CRuby.
 *
 * To test whether \GC compaction is supported, use the idiom:
 *
 *   GC.respond_to?(:compact)
 */
static VALUE
gc_compact(VALUE self)
{
    /* Run GC with compaction enabled */
    rb_gc_impl_start(rb_gc_get_objspace(), true, true, true, true);

    return gc_compact_stats(self);
}
#else
#  define gc_compact rb_f_notimplement
#endif

#if GC_CAN_COMPILE_COMPACTION
static VALUE
gc_verify_compaction_references(rb_execution_context_t *ec, VALUE self, VALUE double_heap, VALUE expand_heap, VALUE toward_empty)
{
    if (RTEST(double_heap)) {
        rb_warn("double_heap is deprecated, please use expand_heap instead");
    }

    rb_gc_impl_verify_compaction_references(rb_gc_get_objspace(), RTEST(double_heap) || RTEST(expand_heap), RTEST(toward_empty));

    return rb_gc_impl_compact_stats(rb_gc_get_objspace());
}
#else
#  define gc_verify_compaction_references rb_f_notimplement
#endif

VALUE
rb_gc_start(void)
{
    rb_gc();
    return Qnil;
}

void
rb_gc(void)
{
    unless_objspace(objspace) { return; }

    rb_gc_impl_start(objspace, true, true, true, false);
}

int
rb_during_gc(void)
{
    unless_objspace(objspace) { return FALSE; }

    return rb_gc_impl_during_gc_p(objspace);
}

size_t
rb_gc_count(void)
{
    return rb_gc_impl_gc_count(rb_gc_get_objspace());
}

static VALUE
gc_count(rb_execution_context_t *ec, VALUE self)
{
    return SIZET2NUM(rb_gc_count());
}

VALUE
rb_gc_latest_gc_info(VALUE key)
{
    return rb_gc_impl_latest_gc_info(rb_gc_get_objspace(), key);
}

static VALUE
gc_latest_gc_info(rb_execution_context_t *ec, VALUE self, VALUE arg)
{
    if (NIL_P(arg)) {
        arg = rb_hash_new();
    }
    else if (!SYMBOL_P(arg) && !RB_TYPE_P(arg, T_HASH)) {
        rb_raise(rb_eTypeError, "non-hash or symbol given");
    }

    return rb_gc_latest_gc_info(arg);
}

static VALUE
gc_stat(rb_execution_context_t *ec, VALUE self, VALUE arg) // arg is (nil || hash || symbol)
{
    if (NIL_P(arg)) {
        arg = rb_hash_new();
    }
    else if (SYMBOL_P(arg)) {
        size_t value = rb_gc_impl_stat(rb_gc_get_objspace(), arg);
        return SIZET2NUM(value);
    }
    else if (RB_TYPE_P(arg, T_HASH)) {
        // ok
    }
    else {
        rb_raise(rb_eTypeError, "non-hash or symbol given");
    }

    rb_gc_impl_stat(rb_gc_get_objspace(), arg);

    return arg;
}

size_t
rb_gc_stat(VALUE key)
{
    if (SYMBOL_P(key)) {
        size_t value = rb_gc_impl_stat(rb_gc_get_objspace(), key);
        return value;
    }
    else {
        rb_gc_impl_stat(rb_gc_get_objspace(), key);
        return 0;
    }
}

static VALUE
gc_stat_heap(rb_execution_context_t *ec, VALUE self, VALUE heap_name, VALUE arg)
{
    if (NIL_P(heap_name)) {
        if (NIL_P(arg)) {
            arg = rb_hash_new();
        }
        else if (RB_TYPE_P(arg, T_HASH)) {
            // ok
        }
        else {
            rb_raise(rb_eTypeError, "non-hash given");
        }

        for (int i = 0; i < SIZE_POOL_COUNT; i++) {
            VALUE hash = rb_hash_aref(arg, INT2FIX(i));
            if (NIL_P(hash)) {
                hash = rb_hash_new();
                rb_hash_aset(arg, INT2FIX(i), hash);
            }

            rb_gc_impl_stat_heap(rb_gc_get_objspace(), i, hash);
        }
    }
    else if (FIXNUM_P(heap_name)) {
        int size_pool_idx = FIX2INT(heap_name);

        if (NIL_P(arg)) {
            arg = rb_hash_new();
        }
        else if (SYMBOL_P(arg)) {
            size_t value = rb_gc_impl_stat_heap(rb_gc_get_objspace(), size_pool_idx, arg);
            return SIZET2NUM(value);
        }
        else if (RB_TYPE_P(arg, T_HASH)) {
            // ok
        }
        else {
            rb_raise(rb_eTypeError, "non-hash or symbol given");
        }

        rb_gc_impl_stat_heap(rb_gc_get_objspace(), size_pool_idx, arg);
    }
    else {
        rb_raise(rb_eTypeError, "heap_name must be nil or an Integer");
    }

    return arg;
}

static VALUE
gc_stress_get(rb_execution_context_t *ec, VALUE self)
{
    return rb_gc_impl_stress_get(rb_gc_get_objspace());
}

static void
gc_stress_set(rb_objspace_t *objspace, VALUE flag)
{
    objspace->flags.gc_stressful = RTEST(flag);
    objspace->gc_stress_mode = flag;
}

static VALUE
gc_stress_set_m(rb_execution_context_t *ec, VALUE self, VALUE flag)
{
    rb_gc_impl_stress_set(rb_gc_get_objspace(), flag);

    return flag;
}

VALUE
rb_gc_enable(void)
{
    return rb_objspace_gc_enable(rb_gc_get_objspace());
}

VALUE
rb_objspace_gc_enable(void *objspace)
{
    bool disabled = !rb_gc_impl_gc_enabled_p(objspace);
    rb_gc_impl_gc_enable(objspace);
    return RBOOL(disabled);
}

static VALUE
gc_enable(rb_execution_context_t *ec, VALUE _)
{
    return rb_gc_enable();
}

VALUE
rb_gc_disable_no_rest(void)
{
    return gc_disable_no_rest(rb_gc_get_objspace());
}

static VALUE
gc_disable_no_rest(void *objspace)
{
    bool disabled = !rb_gc_impl_gc_enabled_p(objspace);
    rb_gc_impl_gc_disable(objspace, false);
    return RBOOL(disabled);
}

VALUE
rb_gc_disable(void)
{
    return rb_objspace_gc_disable(rb_gc_get_objspace());
}

VALUE
rb_objspace_gc_disable(void *objspace)
{
    bool disabled = !rb_gc_impl_gc_enabled_p(objspace);
    rb_gc_impl_gc_disable(objspace, true);
    return RBOOL(disabled);
}

static VALUE
gc_disable(rb_execution_context_t *ec, VALUE _)
{
    return rb_gc_disable();
}

#if GC_CAN_COMPILE_COMPACTION
/*
 *  call-seq:
 *     GC.auto_compact = flag
 *
 *  Updates automatic compaction mode.
 *
 *  When enabled, the compactor will execute on every major collection.
 *
 *  Enabling compaction will degrade performance on major collections.
 */
static VALUE
gc_set_auto_compact(VALUE _, VALUE v)
{
    GC_ASSERT(GC_COMPACTION_SUPPORTED);

    if (RTEST(v)) {
        rb_gc_impl_auto_compact_enable(rb_gc_get_objspace(), v);
    }
    else {
        rb_gc_impl_auto_compact_disable(rb_gc_get_objspace());
    }

    return v;
}
#else
#  define gc_set_auto_compact rb_f_notimplement
#endif

#if GC_CAN_COMPILE_COMPACTION
/*
 *  call-seq:
 *     GC.auto_compact    -> true or false
 *
 *  Returns whether or not automatic compaction has been enabled.
 */
static VALUE
gc_get_auto_compact(VALUE _)
{
    return RBOOL(rb_gc_impl_auto_compact_enabled_p(rb_gc_get_objspace()));
}
#else
#  define gc_get_auto_compact rb_f_notimplement
#endif

// TODO: think about moving ruby_gc_set_params into Init_heap or Init_gc
void
ruby_gc_set_params(void)
{
    rb_gc_impl_set_params(rb_gc_get_objspace());
}

void
rb_gc_reachable_objects_from_callback(VALUE obj)
{
    rb_ractor_t *cr = GET_RACTOR();
    cr->mfd->mark_func(obj, cr->mfd->data);
}

void
rb_objspace_reachable_objects_from(VALUE obj, void (func)(VALUE, void *), void *data)
{
    RB_VM_LOCK_ENTER();
    {
        if (rb_gc_impl_during_gc_p(rb_gc_get_objspace())) rb_bug("rb_objspace_reachable_objects_from() is not supported while during GC");

        if (is_markable_object(obj)) {
            rb_ractor_t *cr = GET_RACTOR();
            struct gc_mark_func_data_struct mfd = {
                .mark_func = func,
                .data = data,
            }, *prev_mfd = cr->mfd;

            cr->mfd = &mfd;
            rb_gc_mark_children(rb_gc_get_objspace(), obj);
            cr->mfd = prev_mfd;
        }
    }
    RB_VM_LOCK_LEAVE();
}

struct root_objects_data {
    const char *category;
    void (*func)(const char *category, VALUE, void *);
    void *data;
};

static void
root_objects_from(VALUE obj, void *ptr)
{
    const struct root_objects_data *data = (struct root_objects_data *)ptr;
    (*data->func)(data->category, obj, data->data);
}

void
rb_objspace_reachable_objects_from_root(void (func)(const char *category, VALUE, void *), void *passing_data)
{
    objspace_reachable_objects_from_root(rb_gc_get_objspace(), func, passing_data);
}

static void
objspace_reachable_objects_from_root(void *objspace, void (func)(const char *category, VALUE, void *), void *passing_data)
{
    if (rb_gc_impl_during_gc_p(objspace)) rb_bug("objspace_reachable_objects_from_root() is not supported while during GC");

    rb_ractor_t *cr = GET_RACTOR();
    struct root_objects_data data = {
        .func = func,
        .data = passing_data,
    };
    struct gc_mark_func_data_struct mfd = {
        .mark_func = root_objects_from,
        .data = &data,
    }, *prev_mfd = cr->mfd;

    cr->mfd = &mfd;
    gc_mark_roots(objspace, &data.category);
    cr->mfd = prev_mfd;
}

/*
  ------------------------ Extended allocator ------------------------
*/

struct gc_raise_tag {
    VALUE exc;
    const char *fmt;
    va_list *ap;
};

static void *
gc_vraise(void *ptr)
{
    struct gc_raise_tag *argv = ptr;
    rb_vraise(argv->exc, argv->fmt, *argv->ap);
    UNREACHABLE_RETURN(NULL);
}

static void
gc_raise(VALUE exc, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    struct gc_raise_tag argv = {
        exc, fmt, &ap,
    };

    if (ruby_thread_has_gvl_p()) {
        gc_vraise(&argv);
        UNREACHABLE;
    }
    else if (ruby_native_thread_p()) {
        rb_thread_call_with_gvl(gc_vraise, &argv);
        UNREACHABLE;
    }
    else {
        /* Not in a ruby thread */
        fprintf(stderr, "%s", "[FATAL] ");
        vfprintf(stderr, fmt, ap);
    }

    va_end(ap);
    abort();
}

static void
negative_size_allocation_error(const char *msg)
{
    gc_raise(rb_eNoMemError, "%s", msg);
}

static void *
ruby_memerror_body(void *dummy)
{
    rb_memerror();
    return 0;
}

NORETURN(static void ruby_memerror(void));
RBIMPL_ATTR_MAYBE_UNUSED()
static void
ruby_memerror(void)
{
    if (ruby_thread_has_gvl_p()) {
        rb_memerror();
    }
    else {
        if (ruby_native_thread_p()) {
            rb_thread_call_with_gvl(ruby_memerror_body, 0);
        }
        else {
            /* no ruby thread */
            fprintf(stderr, "[FATAL] failed to allocate memory\n");
        }
    }
    exit(EXIT_FAILURE);
}

void
rb_memerror(void)
{
    rb_execution_context_t *ec = GET_EC();
    VALUE exc = GET_VM()->special_exceptions[ruby_error_nomemory];

    if (!exc ||
        rb_ec_raised_p(ec, RAISED_NOMEMORY)) {
        fprintf(stderr, "[FATAL] failed to allocate memory\n");
        exit(EXIT_FAILURE);
    }
    if (rb_ec_raised_p(ec, RAISED_NOMEMORY)) {
        rb_ec_raised_clear(ec);
    }
    else {
        rb_ec_raised_set(ec, RAISED_NOMEMORY);
        exc = ruby_vm_special_exception_copy(exc);
    }
    ec->errinfo = exc;
    EC_JUMP_TAG(ec, TAG_RAISE);
}

#if CALC_EXACT_MALLOC_SIZE && USE_GC_MALLOC_OBJ_INFO_DETAILS

#define MALLOC_INFO_GEN_SIZE 100
#define MALLOC_INFO_SIZE_SIZE 10
static size_t malloc_info_gen_cnt[MALLOC_INFO_GEN_SIZE];
static size_t malloc_info_gen_size[MALLOC_INFO_GEN_SIZE];
static size_t malloc_info_size[MALLOC_INFO_SIZE_SIZE+1];
static st_table *malloc_info_file_table;

static int
mmalloc_info_file_i(st_data_t key, st_data_t val, st_data_t dmy)
{
    const char *file = (void *)key;
    const size_t *data = (void *)val;

    fprintf(stderr, "%s\t%"PRIdSIZE"\t%"PRIdSIZE"\n", file, data[0], data[1]);

    return ST_CONTINUE;
}

__attribute__((destructor))
void
rb_malloc_info_show_results(void)
{
    int i;

    fprintf(stderr, "* malloc_info gen statistics\n");
    for (i=0; i<MALLOC_INFO_GEN_SIZE; i++) {
        if (i == MALLOC_INFO_GEN_SIZE-1) {
            fprintf(stderr, "more\t%"PRIdSIZE"\t%"PRIdSIZE"\n", malloc_info_gen_cnt[i], malloc_info_gen_size[i]);
        }
        else {
            fprintf(stderr, "%d\t%"PRIdSIZE"\t%"PRIdSIZE"\n", i, malloc_info_gen_cnt[i], malloc_info_gen_size[i]);
        }
    }

    fprintf(stderr, "* malloc_info size statistics\n");
    for (i=0; i<MALLOC_INFO_SIZE_SIZE; i++) {
        int s = 16 << i;
        fprintf(stderr, "%d\t%"PRIdSIZE"\n", s, malloc_info_size[i]);
    }
    fprintf(stderr, "more\t%"PRIdSIZE"\n", malloc_info_size[i]);

    if (malloc_info_file_table) {
        fprintf(stderr, "* malloc_info file statistics\n");
        st_foreach(malloc_info_file_table, mmalloc_info_file_i, 0);
    }
}
#else
void
rb_malloc_info_show_results(void)
{
}
#endif

void *
ruby_xmalloc_body(size_t size)
{
    if ((ssize_t)size < 0) {
        negative_size_allocation_error("too large allocation size");
    }

    return rb_gc_impl_malloc(rb_gc_get_objspace(), size);
}

void
ruby_malloc_size_overflow(size_t count, size_t elsize)
{
    rb_raise(rb_eArgError,
             "malloc: possible integer overflow (%"PRIuSIZE"*%"PRIuSIZE")",
             count, elsize);
}

void *
ruby_xmalloc2_body(size_t n, size_t size)
{
    return rb_gc_impl_malloc(rb_gc_get_objspace(), xmalloc2_size(n, size));
}

void *
ruby_xcalloc_body(size_t n, size_t size)
{
    return rb_gc_impl_calloc(rb_gc_get_objspace(), xmalloc2_size(n, size));
}

#ifdef ruby_sized_xrealloc
#undef ruby_sized_xrealloc
#endif
void *
ruby_sized_xrealloc(void *ptr, size_t new_size, size_t old_size)
{
    if ((ssize_t)new_size < 0) {
        negative_size_allocation_error("too large allocation size");
    }

    return rb_gc_impl_realloc(rb_gc_get_objspace(), ptr, new_size, old_size);
}

void *
ruby_xrealloc_body(void *ptr, size_t new_size)
{
    return ruby_sized_xrealloc(ptr, new_size, 0);
}

#ifdef ruby_sized_xrealloc2
#undef ruby_sized_xrealloc2
#endif
void *
ruby_sized_xrealloc2(void *ptr, size_t n, size_t size, size_t old_n)
{
    size_t len = xmalloc2_size(n, size);
    return rb_gc_impl_realloc(rb_gc_get_objspace(), ptr, len, old_n * size);
}

void *
ruby_xrealloc2_body(void *ptr, size_t n, size_t size)
{
    return ruby_sized_xrealloc2(ptr, n, size, 0);
}

#ifdef ruby_sized_xfree
#undef ruby_sized_xfree
#endif
void
ruby_sized_xfree(void *x, size_t size)
{
    if (LIKELY(x)) {
        /* It's possible for a C extension's pthread destructor function set by pthread_key_create
         * to be called after ruby_vm_destruct and attempt to free memory. Fall back to mimfree in
         * that case. */
        if (LIKELY(GET_VM())) {
            rb_gc_impl_free(rb_gc_get_objspace(), x, size);
        }
        else {
            ruby_mimfree(x);
        }
    }
}

void
ruby_xfree(void *x)
{
    ruby_sized_xfree(x, 0);
}

void *
rb_xmalloc_mul_add(size_t x, size_t y, size_t z) /* x * y + z */
{
    size_t w = size_mul_add_or_raise(x, y, z, rb_eArgError);
    return ruby_xmalloc(w);
}

void *
rb_xcalloc_mul_add(size_t x, size_t y, size_t z) /* x * y + z */
{
    size_t w = size_mul_add_or_raise(x, y, z, rb_eArgError);
    return ruby_xcalloc(w, 1);
}

void *
rb_xrealloc_mul_add(const void *p, size_t x, size_t y, size_t z) /* x * y + z */
{
    size_t w = size_mul_add_or_raise(x, y, z, rb_eArgError);
    return ruby_xrealloc((void *)p, w);
}

void *
rb_xmalloc_mul_add_mul(size_t x, size_t y, size_t z, size_t w) /* x * y + z * w */
{
    size_t u = size_mul_add_mul_or_raise(x, y, z, w, rb_eArgError);
    return ruby_xmalloc(u);
}

void *
rb_xcalloc_mul_add_mul(size_t x, size_t y, size_t z, size_t w) /* x * y + z * w */
{
    size_t u = size_mul_add_mul_or_raise(x, y, z, w, rb_eArgError);
    return ruby_xcalloc(u, 1);
}

/* Mimic ruby_xmalloc, but need not rb_objspace.
 * should return pointer suitable for ruby_xfree
 */
void *
ruby_mimmalloc(size_t size)
{
    void *mem;
#if CALC_EXACT_MALLOC_SIZE
    size += sizeof(struct malloc_obj_info);
#endif
    mem = malloc(size);
#if CALC_EXACT_MALLOC_SIZE
    if (!mem) {
        return NULL;
    }
    else
    /* set 0 for consistency of allocated_size/allocations */
    {
        struct malloc_obj_info *info = mem;
        info->size = 0;
#if USE_GC_MALLOC_OBJ_INFO_DETAILS
        info->gen = 0;
        info->file = NULL;
        info->line = 0;
#endif
        mem = info + 1;
    }
#endif
    return mem;
}

void
ruby_mimfree(void *ptr)
{
#if CALC_EXACT_MALLOC_SIZE
    struct malloc_obj_info *info = (struct malloc_obj_info *)ptr - 1;
    ptr = info;
#endif
    free(ptr);
}

#if MALLOC_ALLOCATED_SIZE
/*
 *  call-seq:
 *     GC.malloc_allocated_size -> Integer
 *
 *  Returns the size of memory allocated by malloc().
 *
 *  Only available if ruby was built with +CALC_EXACT_MALLOC_SIZE+.
 */

static VALUE
gc_malloc_allocated_size(VALUE self)
{
    return UINT2NUM(rb_objspace.malloc_params.allocated_size);
}

/*
 *  call-seq:
 *     GC.malloc_allocations -> Integer
 *
 *  Returns the number of malloc() allocations.
 *
 *  Only available if ruby was built with +CALC_EXACT_MALLOC_SIZE+.
 */

static VALUE
gc_malloc_allocations(VALUE self)
{
    return UINT2NUM(rb_objspace.malloc_params.allocations);
}
#endif

void
rb_gc_adjust_memory_usage(ssize_t diff)
{
    unless_objspace(objspace) { return; }

    rb_gc_impl_adjust_memory_usage(objspace, diff);
}

const char *
rb_obj_info(VALUE obj)
{
    return rb_gc_impl_obj_info(rb_gc_get_objspace(), obj);
}

void
rb_obj_info_dump(VALUE obj)
{
    char buff[0x100];
    fprintf(stderr, "rb_obj_info_dump: %s\n", rb_gc_impl_full_obj_info(rb_gc_get_objspace(), buff, 0x100, obj));
}

void
rb_obj_info_dump_loc(VALUE obj, const char *file, int line, const char *func)
{
    char buff[0x100];
    fprintf(stderr, "<OBJ_INFO:%s@%s:%d> %s\n", func, file, line, rb_gc_impl_full_obj_info(rb_gc_get_objspace(), buff, 0x100, obj));
}

/*
 *  call-seq:
 *     GC.verify_internal_consistency                  -> nil
 *
 *  Verify internal consistency.
 *
 *  This method is implementation specific.
 *  Now this method checks generational consistency
 *  if RGenGC is supported.
 */
static VALUE
gc_verify_internal_consistency_m(VALUE dummy)
{
    rb_gc_impl_verify_internal_consistency(rb_gc_get_objspace());
    return Qnil;
}

void
rb_gc_verify_internal_consistency(void)
{
    rb_gc_impl_verify_internal_consistency(rb_gc_get_objspace());
}

/*
 * Document-module: ObjectSpace
 *
 *  The ObjectSpace module contains a number of routines
 *  that interact with the garbage collection facility and allow you to
 *  traverse all living objects with an iterator.
 *
 *  ObjectSpace also provides support for object finalizers, procs that will be
 *  called when a specific object is about to be destroyed by garbage
 *  collection. See the documentation for
 *  <code>ObjectSpace.define_finalizer</code> for important information on
 *  how to use this method correctly.
 *
 *     a = "A"
 *     b = "B"
 *
 *     ObjectSpace.define_finalizer(a, proc {|id| puts "Finalizer one on #{id}" })
 *     ObjectSpace.define_finalizer(b, proc {|id| puts "Finalizer two on #{id}" })
 *
 *     a = nil
 *     b = nil
 *
 *  _produces:_
 *
 *     Finalizer two on 537763470
 *     Finalizer one on 537763480
 */

/*  Document-class: GC::Profiler
 *
 *  The GC profiler provides access to information on GC runs including time,
 *  length and object space size.
 *
 *  Example:
 *
 *    GC::Profiler.enable
 *
 *    require 'rdoc/rdoc'
 *
 *    GC::Profiler.report
 *
 *    GC::Profiler.disable
 *
 *  See also GC.count, GC.malloc_allocated_size and GC.malloc_allocations
 */

#include "gc.rbinc"

void
Init_GC(void)
{
#undef rb_intern
    malloc_offset = gc_compute_malloc_offset();

    VALUE rb_mObjSpace;
    VALUE gc_constants;

    rb_mGC = rb_define_module("GC");

    rb_mObjSpace = rb_define_module("ObjectSpace");

    rb_define_module_function(rb_mObjSpace, "each_object", os_each_obj, -1);

    rb_define_module_function(rb_mObjSpace, "define_finalizer", define_final, -1);
    rb_define_module_function(rb_mObjSpace, "undefine_finalizer", undefine_final, 1);

    rb_define_module_function(rb_mObjSpace, "_id2ref", os_id2ref, 1);

    rb_vm_register_special_exception(ruby_error_nomemory, rb_eNoMemError, "failed to allocate memory");

    rb_define_method(rb_cBasicObject, "__id__", rb_obj_id, 0);
    rb_define_method(rb_mKernel, "object_id", rb_obj_id, 0);

    rb_define_module_function(rb_mObjSpace, "count_objects", count_objects, -1);

    /* internal methods */
    rb_define_singleton_method(rb_mGC, "verify_internal_consistency", gc_verify_internal_consistency_m, 0);
#if MALLOC_ALLOCATED_SIZE
    rb_define_singleton_method(rb_mGC, "malloc_allocated_size", gc_malloc_allocated_size, 0);
    rb_define_singleton_method(rb_mGC, "malloc_allocations", gc_malloc_allocations, 0);
#endif

    if (GC_COMPACTION_SUPPORTED) {
        rb_define_singleton_method(rb_mGC, "compact", gc_compact, 0);
        rb_define_singleton_method(rb_mGC, "auto_compact", gc_get_auto_compact, 0);
        rb_define_singleton_method(rb_mGC, "auto_compact=", gc_set_auto_compact, 1);
        rb_define_singleton_method(rb_mGC, "latest_compact_info", gc_compact_stats, 0);
    }
    else {
        rb_define_singleton_method(rb_mGC, "compact", rb_f_notimplement, 0);
        rb_define_singleton_method(rb_mGC, "auto_compact", rb_f_notimplement, 0);
        rb_define_singleton_method(rb_mGC, "auto_compact=", rb_f_notimplement, 1);
        rb_define_singleton_method(rb_mGC, "latest_compact_info", rb_f_notimplement, 0);
        /* When !GC_COMPACTION_SUPPORTED, this method is not defined in gc.rb */
        rb_define_singleton_method(rb_mGC, "verify_compaction_references", rb_f_notimplement, -1);
    }

    {
        VALUE opts;
        /* \GC build options */
        rb_define_const(rb_mGC, "OPTS", opts = rb_ary_new());
#define OPT(o) if (o) rb_ary_push(opts, rb_fstring_lit(#o))
        OPT(GC_DEBUG);
        OPT(USE_RGENGC);
        OPT(RGENGC_DEBUG);
        OPT(RGENGC_CHECK_MODE);
        OPT(RGENGC_PROFILE);
        OPT(RGENGC_ESTIMATE_OLDMALLOC);
        OPT(GC_PROFILE_MORE_DETAIL);
        OPT(GC_ENABLE_LAZY_SWEEP);
        OPT(CALC_EXACT_MALLOC_SIZE);
        OPT(MALLOC_ALLOCATED_SIZE);
        OPT(MALLOC_ALLOCATED_SIZE_CHECK);
        OPT(GC_PROFILE_DETAIL_MEMORY);
        OPT(GC_COMPACTION_SUPPORTED);
#undef OPT
        OBJ_FREEZE(opts);
    }
}

#ifdef ruby_xmalloc
#undef ruby_xmalloc
#endif
#ifdef ruby_xmalloc2
#undef ruby_xmalloc2
#endif
#ifdef ruby_xcalloc
#undef ruby_xcalloc
#endif
#ifdef ruby_xrealloc
#undef ruby_xrealloc
#endif
#ifdef ruby_xrealloc2
#undef ruby_xrealloc2
#endif

void *
ruby_xmalloc(size_t size)
{
#if USE_GC_MALLOC_OBJ_INFO_DETAILS
    ruby_malloc_info_file = __FILE__;
    ruby_malloc_info_line = __LINE__;
#endif
    return ruby_xmalloc_body(size);
}

void *
ruby_xmalloc2(size_t n, size_t size)
{
#if USE_GC_MALLOC_OBJ_INFO_DETAILS
    ruby_malloc_info_file = __FILE__;
    ruby_malloc_info_line = __LINE__;
#endif
    return ruby_xmalloc2_body(n, size);
}

void *
ruby_xcalloc(size_t n, size_t size)
{
#if USE_GC_MALLOC_OBJ_INFO_DETAILS
    ruby_malloc_info_file = __FILE__;
    ruby_malloc_info_line = __LINE__;
#endif
    return ruby_xcalloc_body(n, size);
}

void *
ruby_xrealloc(void *ptr, size_t new_size)
{
#if USE_GC_MALLOC_OBJ_INFO_DETAILS
    ruby_malloc_info_file = __FILE__;
    ruby_malloc_info_line = __LINE__;
#endif
    return ruby_xrealloc_body(ptr, new_size);
}

void *
ruby_xrealloc2(void *ptr, size_t n, size_t new_size)
{
#if USE_GC_MALLOC_OBJ_INFO_DETAILS
    ruby_malloc_info_file = __FILE__;
    ruby_malloc_info_line = __LINE__;
#endif
    return ruby_xrealloc2_body(ptr, n, new_size);
}
