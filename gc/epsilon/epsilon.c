#include "ruby/internal/config.h"

#ifndef _WIN32
# include <sys/mman.h>
# include <unistd.h>
#endif

#ifdef BUILDING_MODULAR_GC
# define nlz_int64(x) (x == 0 ? 64 : (unsigned int)__builtin_clzll((unsigned long long)x))
#else
# include "internal/bits.h"
#endif

#include "ruby/ruby.h"
#include "ruby/atomic.h"
#include "ruby/debug.h"
#include "ccan/list/list.h"
#include "gc/gc.h"
#include "gc/gc_impl.h"

#ifdef BUILDING_MODULAR_GC
# define rb_asan_poison_object(obj) ((void)(obj))
# define rb_asan_unpoison_object(obj, newobj_p) ((void)(obj), (void)(newobj_p))
# define asan_unpoisoning_object(obj) if ((obj) || true)
# define asan_poison_memory_region(ptr, size) ((void)(ptr), (void)(size))
# define asan_unpoison_memory_region(ptr, size, malloc_p) ((void)(ptr), (size), (malloc_p))
# define asan_unpoisoning_memory_region(ptr, size) if ((ptr) || (size) || true)

# define VALGRIND_MAKE_MEM_DEFINED(ptr, size) ((void)(ptr), (void)(size))
# define VALGRIND_MAKE_MEM_UNDEFINED(ptr, size) ((void)(ptr), (void)(size))
#else
# include "internal/sanitizers.h"
#endif

#ifndef HAVE_MALLOC_USABLE_SIZE
# ifdef _WIN32
#  define HAVE_MALLOC_USABLE_SIZE
#  define malloc_usable_size(a) _msize(a)
# elif defined HAVE_MALLOC_SIZE
#  define HAVE_MALLOC_USABLE_SIZE
#  define malloc_usable_size(a) malloc_size(a)
# endif
#endif

#ifdef HAVE_MALLOC_USABLE_SIZE
# ifdef RUBY_ALTERNATIVE_MALLOC_HEADER
# elif defined(HAVE_MALLOC_H)
#  include <malloc.h>
# elif defined(HAVE_MALLOC_NP_H)
#  include <malloc_np.h>
# elif defined(HAVE_MALLOC_MALLOC_H)
#  include <malloc/malloc.h>
# endif
#endif

#ifndef RUBY_DEBUG_LOG
# define RUBY_DEBUG_LOG(...)
#endif

#ifndef HEAP_PAGE_ALIGN_LOG
#define HEAP_PAGE_ALIGN_LOG 16
#endif

#ifndef MAX
# define MAX(a, b) (((a) > (b)) ? (a) : (b))
#endif
#ifndef MIN
# define MIN(a, b) (((a) < (b)) ? (a) : (b))
#endif
#define roomof(x, y) (((x) + (y) - 1) / (y))
#define CEILDIV(i, mod) roomof(i, mod)

#undef INIT_HEAP_PAGE_ALLOC_USE_MMAP

#ifndef HAVE_MMAP
static const bool HEAP_PAGE_ALLOC_USE_MMAP = false;

#elif defined(__wasm__)
static const bool HEAP_PAGE_ALLOC_USE_MMAP = false;

#elif HAVE_CONST_PAGE_SIZE
static const bool HEAP_PAGE_ALLOC_USE_MMAP = (PAGE_SIZE <= HEAP_PAGE_SIZE);

#elif defined(PAGE_MAX_SIZE) && (PAGE_MAX_SIZE <= (1 << HEAP_PAGE_ALIGN_LOG))
static const bool HEAP_PAGE_ALLOC_USE_MMAP = true;

#elif defined(PAGE_SIZE)
# define INIT_HEAP_PAGE_ALLOC_USE_MMAP (PAGE_SIZE <= HEAP_PAGE_SIZE)

#elif defined(HAVE_SYSCONF) && defined(_SC_PAGE_SIZE)
# define INIT_HEAP_PAGE_ALLOC_USE_MMAP (sysconf(_SC_PAGE_SIZE) <= HEAP_PAGE_SIZE)

#else
static const bool HEAP_PAGE_ALLOC_USE_MMAP = false;
#endif

#ifdef INIT_HEAP_PAGE_ALLOC_USE_MMAP
# define HEAP_PAGE_ALLOC_USE_MMAP (heap_page_alloc_use_mmap != false)

static bool heap_page_alloc_use_mmap;
#endif

#ifndef VM_CHECK_MODE
# define VM_CHECK_MODE RUBY_DEBUG
#endif

#ifndef RACTOR_CHECK_MODE
# define RACTOR_CHECK_MODE (VM_CHECK_MODE || RUBY_DEBUG) && (SIZEOF_UINT64_T == SIZEOF_VALUE)
#endif

#if RACTOR_CHECK_MODE || GC_DEBUG
struct rvalue_overhead {
# if RACTOR_CHECK_MODE
    uint32_t _ractor_belonging_id;
# endif
# if GC_DEBUG
    const char *file;
    int line;
# endif
};

# define RVALUE_OVERHEAD (sizeof(struct { \
    union { \
        struct rvalue_overhead overhead; \
        VALUE value; \
    }; \
}))
#else
# ifndef RVALUE_OVERHEAD
#  define RVALUE_OVERHEAD 0
# endif
#endif

#ifndef OBJ_SIZE_MULTIPLES
# define OBJ_SIZE_MULTIPLES 5
#endif

#define BASE_SLOT_SIZE (sizeof(struct RBasic) + sizeof(VALUE[RBIMPL_RVALUE_EMBED_LEN_MAX]) + RVALUE_OVERHEAD)

struct heap_page_header {
    struct heap_page *page;
};

struct heap_page_body {
    struct heap_page_header header;
};

typedef struct rb_objspace {
    struct {
        size_t limit;
        size_t increase;
    } malloc_params;

    struct {
        unsigned int has_newobj_hook: 1;
    } flags;

    rb_event_flag_t hook_events;

    struct heap_page *free_page_cache[OBJ_SIZE_MULTIPLES];

    struct {
        size_t allocatable_pages;
        size_t total_allocated_pages;
        size_t total_allocated_objects;
        size_t empty_slots;

        struct heap_page *free_pages;
        struct ccan_list_head pages;
        size_t total_pages;
        size_t total_slots;
    } heap;

    struct {
        rb_atomic_t finalizing;
    } atomic_flags;

    struct {
        struct heap_page **sorted;
        size_t allocated_pages;
        size_t allocatable_pages;
        size_t sorted_length;
        uintptr_t range[2];
        size_t freeable_pages;

        size_t final_slots;
        VALUE deferred_final;
    } heap_pages;

    st_table *finalizer_table;

    rb_postponed_job_handle_t finalize_deferred_pjob;
    unsigned long live_ractor_cache_count;
} rb_objspace_t;

struct free_slot {
    VALUE flags;
    struct free_slot *next;
};

typedef struct heap_page {
    short slot_size;
    short total_slots;
    short free_slots;
    short final_slots;

    struct heap_page *free_next;
    uintptr_t start;
    struct free_slot *freelist;
    struct ccan_list_node page_node;
} rb_heap_page_t;

struct RZombie {
    VALUE flags;
    VALUE next;
    void (*dfree)(void *);
    void *data;
};

#define RZOMBIE(o) ((struct RZombie *)(o))
enum {
    HEAP_PAGE_ALIGN = (1UL << HEAP_PAGE_ALIGN_LOG),
    HEAP_PAGE_ALIGN_MASK = (~(~0UL << HEAP_PAGE_ALIGN_LOG)),
    HEAP_PAGE_SIZE = HEAP_PAGE_ALIGN,
    HEAP_PAGE_OBJ_LIMIT = (unsigned int)((HEAP_PAGE_SIZE - sizeof(struct heap_page_header)) / BASE_SLOT_SIZE),
};
#define HEAP_PAGE_ALIGN (1 << HEAP_PAGE_ALIGN_LOG)
#define HEAP_PAGE_SIZE HEAP_PAGE_ALIGN
#define GET_PAGE_BODY(x)   ((struct heap_page_body *)((uintptr_t)(x) & ~(HEAP_PAGE_ALIGN_MASK)))
#define GET_PAGE_HEADER(x) (&GET_PAGE_BODY(x)->header)
#define GET_HEAP_PAGE(x)   (GET_PAGE_HEADER(x)->page)
#define NUM_IN_PAGE(p)   (((uintptr_t)(p) & HEAP_PAGE_ALIGN_MASK) / BASE_SLOT_SIZE)

#define malloc_increase           objspace->malloc_params.increase
#define heap_pages_sorted         objspace->heap_pages.sorted
#define heap_allocated_pages      objspace->heap_pages.allocated_pages
#define heap_pages_sorted_length  objspace->heap_pages.sorted_length
#define heap_pages_lomem          objspace->heap_pages.range[0]
#define heap_pages_himem          objspace->heap_pages.range[1]
#define heap_pages_final_slots    objspace->heap_pages.final_slots
#define heap_pages_deferred_final objspace->heap_pages.deferred_final
#define finalizing                objspace->atomic_flags.finalizing
#define finalizer_table           objspace->finalizer_table

#define ZOMBIE_OBJ_KEPT_FLAGS (FL_FINALIZE)

#ifdef RUBY_DEBUG
# ifndef RNOGC_DEBUG
#  define RNOGC_DEBUG 0
# endif
#endif

#define gc_report(objspace, ...) \
    if (!(RUBY_DEBUG && RNOGC_DEBUG)) {} else gc_report_body(objspace, __VA_ARGS__)

PRINTF_ARGS(static void gc_report_body(rb_objspace_t *objspace, const char *fmt, ...), 2, 3);

static void gc_finalize_deferred(void *dmy);

static void
asan_lock_freelist(struct heap_page *page)
{
    asan_poison_memory_region(&page->freelist, sizeof(struct free_slot *));
}

static void
asan_unlock_freelist(struct heap_page *page)
{
    asan_unpoison_memory_region(&page->freelist, sizeof(struct free_slot *), false);
}

static inline void *
calloc1(size_t n)
{
    return calloc(1, n);
}

static void
heap_pages_expand_sorted_to(rb_objspace_t *objspace, size_t next_length)
{
    struct heap_page **sorted;
    size_t size = rb_size_mul_or_raise(next_length, sizeof(struct heap_page *), rb_eRuntimeError);

    gc_report(objspace, "heap_pages_expand_sorted: next_length: %"PRIdSIZE", size: %"PRIdSIZE"\n",
              next_length, size);

    if (heap_pages_sorted_length > 0) {
        sorted = (struct heap_page **)realloc(heap_pages_sorted, size);
        if (sorted) heap_pages_sorted = sorted;
    }
    else {
        sorted = heap_pages_sorted = (struct heap_page **)malloc(size);
    }

    if (sorted == 0) {
        rb_memerror();
    }

    heap_pages_sorted_length = next_length;
}

static void
heap_pages_expand_sorted(rb_objspace_t *objspace)
{
    size_t next_length = objspace->heap.allocatable_pages +
        objspace->heap.total_pages;

    if (next_length > heap_pages_sorted_length) {
        heap_pages_expand_sorted_to(objspace, next_length);
    }

    GC_ASSERT(objspace->heap.allocatable_pages + objspace->heap.total_pages <= heap_pages_sorted_length);
    GC_ASSERT(objspace->heap_pages.allocated_pages <= heap_pages_sorted_length);
}

static inline void
heap_page_add_freeobj(rb_objspace_t *objspace, struct heap_page *page, VALUE obj)
{
    rb_asan_unpoison_object(obj, false);
    asan_unlock_freelist(page);

    struct free_slot *slot = (struct free_slot *)obj;
    slot->flags = 0;
    slot->next = page->freelist;
    page->freelist = slot;

    asan_lock_freelist(page);
    rb_asan_poison_object(obj);
    gc_report(objspace, "heap_page_add_freeobj: add %p to freelist\n", (void *)obj);
}

static inline void
heap_add_freepage(rb_objspace_t *objspace, struct heap_page *page)
{
    asan_unlock_freelist(page);
    GC_ASSERT(page->free_slots != 0);
    GC_ASSERT(page->freelist != NULL);

    page->free_next = objspace->heap.free_pages;
    objspace->heap.free_pages = page;

    RUBY_DEBUG_LOG("page:%p freelist:%p", (void *)page, (void *)page->freelist);

    asan_lock_freelist(page);
}

static void
gc_aligned_free(void *ptr, size_t size)
{
#if defined(HAVE_POSIX_MEMALIGN) || defined(HAVE_MEMALIGN)
    free(ptr);
#else
    free(((void**)ptr)[-1]);
#endif
}

static void
heap_page_body_free(struct heap_page_body *page_body)
{
    GC_ASSERT((uintptr_t)page_body % HEAP_PAGE_ALIGN == 0);

    if (HEAP_PAGE_ALLOC_USE_MMAP) {
#ifdef HAVE_MMAP
        GC_ASSERT(HEAP_PAGE_SIZE % sysconf(_SC_PAGE_SIZE) == 0);
        if (munmap(page_body, HEAP_PAGE_SIZE)) {
            rb_bug("heap_page_body_free: munmap failed");
        }
#endif
    }
    else {
        gc_aligned_free(page_body, HEAP_PAGE_SIZE);
    }
}

static void *
gc_aligned_malloc(size_t alignment, size_t size)
{
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

static struct heap_page_body *
heap_page_body_allocate(void)
{
    struct heap_page_body *page_body;

    if (HEAP_PAGE_ALLOC_USE_MMAP) {
#ifdef HAVE_MMAP
        GC_ASSERT(HEAP_PAGE_ALIGN % sysconf(_SC_PAGE_SIZE) == 0);

        char *ptr = mmap(NULL, HEAP_PAGE_ALIGN + HEAP_PAGE_SIZE,
                         PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
        if (ptr == MAP_FAILED) {
            return NULL;
        }

        char *aligned = ptr + HEAP_PAGE_ALIGN;
        aligned -= ((VALUE)aligned & (HEAP_PAGE_ALIGN - 1));
        GC_ASSERT(aligned > ptr);
        GC_ASSERT(aligned <= ptr + HEAP_PAGE_ALIGN);

        size_t start_out_of_range_size = aligned - ptr;
        GC_ASSERT(start_out_of_range_size % sysconf(_SC_PAGE_SIZE) == 0);
        if (start_out_of_range_size > 0) {
            if (munmap(ptr, start_out_of_range_size)) {
                rb_bug("heap_page_body_allocate: munmap failed for start");
            }
        }

        size_t end_out_of_range_size = HEAP_PAGE_ALIGN - start_out_of_range_size;
        GC_ASSERT(end_out_of_range_size % sysconf(_SC_PAGE_SIZE) == 0);
        if (end_out_of_range_size > 0) {
            if (munmap(aligned + HEAP_PAGE_SIZE, end_out_of_range_size)) {
                rb_bug("heap_page_body_allocate: munmap failed for end");
            }
        }

        page_body = (struct heap_page_body *)aligned;
#endif
    }
    else {
        page_body = gc_aligned_malloc(HEAP_PAGE_ALIGN, HEAP_PAGE_SIZE);
    }

    GC_ASSERT((uintptr_t)page_body % HEAP_PAGE_ALIGN == 0);

    return page_body;
}

static struct heap_page *
heap_page_allocate(rb_objspace_t *objspace, size_t slot_size)
{
    uintptr_t start, end, p;
    struct heap_page *page;
    uintptr_t hi, lo, mid;
    size_t stride = slot_size;
    unsigned int limit = (unsigned int)((HEAP_PAGE_SIZE - sizeof(struct heap_page_header)))/(int)stride;

    struct heap_page_body *page_body = heap_page_body_allocate();
    if (page_body == 0) {
        rb_memerror();
    }

    page = calloc1(sizeof(struct heap_page));
    if (page == 0) {
        heap_page_body_free(page_body);
        rb_memerror();
    }

    start = (uintptr_t)((VALUE)page_body + sizeof(struct heap_page_header));

    if (start % BASE_SLOT_SIZE != 0) {
        int delta = BASE_SLOT_SIZE - (start % BASE_SLOT_SIZE);
        start = start + delta;
        GC_ASSERT(NUM_IN_PAGE(start) == 0 || NUM_IN_PAGE(start) == 1);

        if (NUM_IN_PAGE(start) == 1) {
            start += stride - BASE_SLOT_SIZE;
        }

        GC_ASSERT(NUM_IN_PAGE(start) * BASE_SLOT_SIZE % stride == 0);

        limit = (HEAP_PAGE_SIZE - (int)(start - (uintptr_t)page_body))/(int)stride;
    }
    end = start + (limit * (int)stride);

    lo = 0;
    hi = (uintptr_t)heap_allocated_pages;
    while (lo < hi) {
        struct heap_page *mid_page;

        mid = (lo + hi) / 2;
        mid_page = heap_pages_sorted[mid];
        if ((uintptr_t)mid_page->start < start) {
            lo = mid + 1;
        }
        else if ((uintptr_t)mid_page->start > start) {
            hi = mid;
        }
        else {
            rb_bug("same heap page is allocated: %p at %"PRIuVALUE, (void *)page_body, (VALUE)mid);
        }
    }

    if (hi < (uintptr_t)heap_allocated_pages) {
        MEMMOVE(&heap_pages_sorted[hi+1], &heap_pages_sorted[hi], struct heap_page_header*, heap_allocated_pages - hi);
    }

    heap_pages_sorted[hi] = page;
    heap_allocated_pages++;

    GC_ASSERT(objspace->heap.total_pages + objspace->heap.allocatable_pages <= heap_pages_sorted_length);
    GC_ASSERT(heap_allocated_pages <= heap_pages_sorted_length);

    objspace->heap.total_allocated_pages++;

    if (heap_allocated_pages > heap_pages_sorted_length) {
        rb_bug("heap_page_allocate: allocated(%"PRIdSIZE") > sorted(%"PRIdSIZE")",
               heap_allocated_pages, heap_pages_sorted_length);
    }

    if (heap_pages_lomem == 0 || heap_pages_lomem > start) heap_pages_lomem = start;
    if (heap_pages_himem < end) heap_pages_himem = end;

    page->start = start;
    page->total_slots = limit;
    page->slot_size = slot_size;
    page_body->header.page = page;

    for (p = start; p != end; p += stride) {
        gc_report(objspace, "assign_heap_page: %p is added to freelist\n", (void *)p);
        heap_page_add_freeobj(objspace, page, (VALUE)p);
    }
    page->free_slots = limit;

    asan_lock_freelist(page);
    return page;
}

static struct heap_page *
heap_page_create(rb_objspace_t *objspace, size_t slot_size)
{
    objspace->heap.allocatable_pages--;
    return heap_page_allocate(objspace, slot_size);
}

static void
heap_add_page(rb_objspace_t *objspace, struct heap_page *page)
{
    ccan_list_add_tail(&objspace->heap.pages, &page->page_node);
    objspace->heap.total_pages++;
    objspace->heap.total_slots += page->total_slots;
}

static rb_heap_page_t *
heap_assign_page(rb_objspace_t *objspace, size_t slot_size)
{
    struct heap_page *page = heap_page_create(objspace, slot_size);
    heap_add_page(objspace, page);
    heap_add_freepage(objspace, page);

    return page;
}

static rb_heap_page_t *
heap_increment(rb_objspace_t *objspace, size_t slot_size)
{
    rb_heap_page_t *page = NULL;
    if (objspace->heap.allocatable_pages > 0) {
        gc_report(objspace, "heap_increment: heap_pages_sorted_length: %"PRIdSIZE", "
                  "heap_pages_inc: %"PRIdSIZE", heap->total_pages: %"PRIdSIZE"\n",
                  heap_pages_sorted_length, objspace->heap.allocatable_pages, objspace->heap.total_pages);

        GC_ASSERT(objspace->heap.allocatable_pages + objspace->heap.total_pages <= heap_pages_sorted_length);
        GC_ASSERT(heap_allocated_pages <= heap_pages_sorted_length);

        page = heap_assign_page(objspace, slot_size);
    }
    return page;
}

static inline size_t
goal_allocatable_pages_count(rb_objspace_t *objspace)
{
    size_t allocated_pages = objspace->heap.total_allocated_pages;
    size_t allocatable_pages = objspace->heap.allocatable_pages;

    if (allocated_pages / allocatable_pages >= 0.75) {
        allocatable_pages = allocatable_pages * 2;
    }
    return allocatable_pages;
}

static rb_heap_page_t *
heap_prepare(rb_objspace_t *objspace, size_t slot_size)
{
    rb_heap_page_t *page = NULL;
    size_t extend_page_count = goal_allocatable_pages_count(objspace);
    if (extend_page_count > objspace->heap.allocatable_pages) {
        objspace->heap.allocatable_pages = extend_page_count;
        heap_pages_expand_sorted(objspace);
    }
    GC_ASSERT(objspace->heap.allocatable_pages > 0);
    page = heap_increment(objspace, slot_size);
    GC_ASSERT(objspace->heap.free_pages != NULL);

    return page;
}

static inline size_t
valid_object_sizes_ordered_idx(unsigned char pool_id)
{
    GC_ASSERT(pool_id < OBJ_SIZE_MULTIPLES);
    return (1 << pool_id) * BASE_SLOT_SIZE;
}

bool
rb_gc_impl_size_allocatable_p(size_t size)
{
    return size <= valid_object_sizes_ordered_idx(OBJ_SIZE_MULTIPLES - 1) - RVALUE_OVERHEAD;
}

static inline size_t
page_slot_size_idx_for_size(size_t size)
{
    size += RVALUE_OVERHEAD;

    size_t slot_count = CEILDIV(size, BASE_SLOT_SIZE);
    size_t ordered_object_size_idx = 64 - nlz_int64(slot_count - 1);

    if (ordered_object_size_idx >= OBJ_SIZE_MULTIPLES) {
        rb_bug("page_slot_size_idx_for_size: allocation size too large "
               "(size=%"PRIuSIZE"u, ordered_object_size_idx=%"PRIuSIZE"u)",
               size, ordered_object_size_idx);
    }

    return ordered_object_size_idx;
}

static size_t heap_sizes[OBJ_SIZE_MULTIPLES + 1] = { 0 };

size_t *
rb_gc_impl_heap_sizes(void *objspace_ptr)
{
    if (heap_sizes[0] == 0) {
        for (unsigned char i = 0; i < OBJ_SIZE_MULTIPLES; i++) {
            heap_sizes[i] = valid_object_sizes_ordered_idx(i) - RVALUE_OVERHEAD;
        }
    }

    return heap_sizes;
}

static VALUE
newobj_alloc(rb_objspace_t *objspace, size_t cache_idx, size_t slot_size)
{
    unsigned int lev = RB_GC_CR_LOCK();

    GC_ASSERT(objspace->free_page_cache[cache_idx]);
    struct heap_page *page = objspace->free_page_cache[cache_idx];

    if (page->free_slots == 0) {
        page = heap_prepare(objspace, slot_size);
        objspace->free_page_cache[cache_idx] = page;
    }

    struct free_slot *obj = page->freelist;
    GC_ASSERT(RB_TYPE_P((VALUE)obj, T_NONE));

    page->freelist = obj->next;
    page->free_slots--;
    RB_GC_CR_UNLOCK(lev);

    objspace->heap.total_allocated_objects++;
    return (VALUE)obj;
}

static int
ptr_in_page_body_p(const void *ptr, const void *memb)
{
    struct heap_page *page = *(struct heap_page **)memb;
    uintptr_t p_body = (uintptr_t)GET_PAGE_BODY(page->start);

    if ((uintptr_t)ptr >= p_body) {
        return (uintptr_t)ptr < (p_body + HEAP_PAGE_SIZE) ? 0 : 1;
    }
    else {
        return -1;
    }
}

PUREFUNC(static inline struct heap_page *heap_page_for_ptr(rb_objspace_t *objspace, uintptr_t ptr);)
static inline struct heap_page *
heap_page_for_ptr(rb_objspace_t *objspace, uintptr_t ptr)
{
    struct heap_page **res;

    if (ptr < (uintptr_t)heap_pages_lomem ||
            ptr > (uintptr_t)heap_pages_himem) {
        return NULL;
    }

    res = bsearch((void *)ptr, heap_pages_sorted,
                  (size_t)heap_allocated_pages, sizeof(struct heap_page *),
                  ptr_in_page_body_p);

    if (res) {
        return *res;
    }
    else {
        return NULL;
    }
}

typedef int each_obj_callback(void *, void *, size_t, void *);
typedef int each_page_callback(struct heap_page *, void *);

struct each_obj_data {
    rb_objspace_t *objspace;
    bool reenable_incremental;

    each_obj_callback *each_obj_callback;
    each_page_callback *each_page_callback;
    void *data;

    struct heap_page **pages;
    size_t pages_count;
};

static VALUE
objspace_each_objects_ensure(VALUE arg)
{
    struct each_obj_data *data = (struct each_obj_data *)arg;
    free(data->pages);
    return Qnil;
}

static VALUE
objspace_each_objects_try(VALUE arg)
{
    struct each_obj_data *data = (struct each_obj_data *)arg;
    rb_objspace_t *objspace = data->objspace;

    size_t size = objspace->heap.total_pages * sizeof(rb_heap_page_t *);
    rb_heap_page_t **pages = malloc(size);
    if (!pages) rb_memerror();

    rb_heap_page_t *page = NULL;
    size_t pages_count = 0;
    ccan_list_for_each(&objspace->heap.pages, page, page_node) {
        pages[pages_count] = page;
        pages_count++;
    }

    data->pages = pages;
    data->pages_count = pages_count;

    GC_ASSERT(pages_count == data->pages_count &&
              pages_count == objspace->heap.total_pages);

    page = ccan_list_top(&objspace->heap.pages, struct heap_page, page_node);

    for (size_t i = 0; i < pages_count; i++) {
        if (page == NULL) break;

        if (data->pages[i] != page) continue;

        uintptr_t pstart = (uintptr_t)page->start;
        uintptr_t pend = pstart + (page->total_slots * page->slot_size);

        if (data->each_obj_callback &&
                (*data->each_obj_callback)((void *)pstart, (void *)pend, page->slot_size, data->data)) {
            break;
        }
        if (data->each_page_callback &&
                (*data->each_page_callback)(page, data->data)) {
            break;
        }

        page = ccan_list_next(&objspace->heap.pages, page, page_node);
    }

    return Qnil;
}

static void
objspace_each_exec(bool protected, struct each_obj_data *each_obj_data)
{
    each_obj_data->reenable_incremental = FALSE;
    memset(&each_obj_data->pages, 0, sizeof(each_obj_data->pages));
    memset(&each_obj_data->pages_count, 0, sizeof(each_obj_data->pages_count));
    rb_ensure(objspace_each_objects_try, (VALUE)each_obj_data,
              objspace_each_objects_ensure, (VALUE)each_obj_data);
}

static void
objspace_each_objects(rb_objspace_t *objspace, each_obj_callback *callback, void *data, bool protected)
{
    struct each_obj_data each_obj_data = {
        .objspace = objspace,
        .each_obj_callback = callback,
        .each_page_callback = NULL,
        .data = data,
    };
    objspace_each_exec(protected, &each_obj_data);
}

static VALUE
get_final(long i, void *data)
{
    VALUE table = (VALUE)data;

    return RARRAY_AREF(table, i + 1);
}

static unsigned int
run_final(rb_objspace_t *objspace, VALUE zombie, unsigned int lev)
{
    if (RZOMBIE(zombie)->dfree) {
        RZOMBIE(zombie)->dfree(RZOMBIE(zombie)->data);
    }

    st_data_t key = (st_data_t)zombie;
    if (FL_TEST_RAW(zombie, FL_FINALIZE)) {
        FL_UNSET(zombie, FL_FINALIZE);
        st_data_t table;
        if (st_delete(finalizer_table, &key, &table)) {
            RB_GC_VM_UNLOCK(lev);
            rb_gc_run_obj_finalizer(RARRAY_AREF(table, 0), RARRAY_LEN(table) - 1, get_final, (void *)table);
            lev = RB_GC_VM_LOCK();
        }
        else {
            rb_bug("FL_FINALIZE flag is set, but finalizers are not found");
        }
    }
    else {
        GC_ASSERT(!st_lookup(finalizer_table, key, NULL));
    }
    return lev;
}

static void
finalize_list(rb_objspace_t *objspace, VALUE zombie)
{
    while (zombie) {
        VALUE next_zombie;
        struct heap_page *page;
        rb_asan_unpoison_object(zombie, false);
        next_zombie = RZOMBIE(zombie)->next;
        page = GET_HEAP_PAGE(zombie);

        unsigned int lev = RB_GC_VM_LOCK();

        lev = run_final(objspace, zombie, lev);
        {
            GC_ASSERT(BUILTIN_TYPE(zombie) == T_ZOMBIE);
            GC_ASSERT(heap_pages_final_slots > 0);
            GC_ASSERT(page->final_slots > 0);

            heap_pages_final_slots--;
            page->final_slots--;
            page->free_slots++;
            heap_page_add_freeobj(objspace, page, zombie);
        }
        RB_GC_VM_UNLOCK(lev);

        zombie = next_zombie;
    }
}

static void
finalize_deferred_heap_pages(rb_objspace_t *objspace)
{
    VALUE zombie;
    while ((zombie = RUBY_ATOMIC_VALUE_EXCHANGE(heap_pages_deferred_final, 0)) != 0) {
        finalize_list(objspace, zombie);
    }
}

static void
finalize_deferred(rb_objspace_t *objspace)
{
    rb_gc_set_pending_interrupt();
    finalize_deferred_heap_pages(objspace);
    rb_gc_unset_pending_interrupt();
}

static void
gc_finalize_deferred(void *dmy)
{
    rb_objspace_t *objspace = dmy;
    if (RUBY_ATOMIC_EXCHANGE(finalizing, 1)) return;

    finalize_deferred(objspace);
    RUBY_ATOMIC_SET(finalizing, 0);
}

static void
gc_report_body(rb_objspace_t *objspace, const char *fmt, ...)
{
    char buf[1024];
    FILE *out = stderr;
    va_list args;
    const char *status = " ";

    va_start(args, fmt);
    vsnprintf(buf, 1024, fmt, args);
    va_end(args);

    fprintf(out, "%s|", status);
    fputs(buf, out);
}

enum gc_stat_sym {
    gc_stat_sym_heap_allocated_pages,
    gc_stat_sym_heap_sorted_length,
    gc_stat_sym_heap_allocatable_pages,
    gc_stat_sym_heap_available_slots,
    gc_stat_sym_heap_live_slots,
    gc_stat_sym_heap_free_slots,
    gc_stat_sym_heap_final_slots,
    gc_stat_sym_heap_eden_pages,
    gc_stat_sym_total_allocated_pages,
    gc_stat_sym_total_allocated_objects,
    gc_stat_sym_malloc_increase_bytes,
    gc_stat_sym_last
};

static VALUE gc_stat_symbols[gc_stat_sym_last];

static void
setup_gc_stat_symbols(void)
{
    if (gc_stat_symbols[0] == 0) {
#define S(s) gc_stat_symbols[gc_stat_sym_##s] = ID2SYM(rb_intern_const(#s))
        S(heap_allocated_pages);
        S(heap_sorted_length);
        S(heap_allocatable_pages);
        S(heap_available_slots);
        S(heap_live_slots);
        S(heap_free_slots);
        S(heap_final_slots);
        S(heap_eden_pages);
        S(total_allocated_pages);
        S(total_allocated_objects);
        S(malloc_increase_bytes);
#undef S
    }
}

static size_t
objspace_live_slots(rb_objspace_t *objspace)
{
    return objspace->heap.total_allocated_objects - heap_pages_final_slots;
}

static size_t
objspace_free_slots(rb_objspace_t *objspace)
{
    return objspace->heap.total_slots - objspace_live_slots(objspace) - heap_pages_final_slots;
}

enum gc_stat_heap_sym {
    gc_stat_heap_sym_heap_allocatable_pages,
    gc_stat_heap_sym_heap_eden_pages,
    gc_stat_heap_sym_heap_eden_slots,
    gc_stat_heap_sym_total_allocated_pages,
    gc_stat_heap_sym_total_allocated_objects,
    gc_stat_heap_sym_last
};

static VALUE gc_stat_heap_symbols[gc_stat_heap_sym_last];

enum memop_type {
    MEMOP_TYPE_MALLOC  = 0,
    MEMOP_TYPE_FREE,
    MEMOP_TYPE_REALLOC
};

static inline void
atomic_sub_nounderflow(size_t *var, size_t sub)
{
    if (sub == 0) return;

    while (1) {
        size_t val = *var;
        if (val < sub) sub = val;
        if (RUBY_ATOMIC_SIZE_CAS(*var, val, val-sub) == val) break;
    }
}

static bool
objspace_malloc_increase_body(rb_objspace_t *objspace, void *mem, size_t new_size, size_t old_size, enum memop_type type)
{
    if (new_size > old_size) {
        RUBY_ATOMIC_SIZE_ADD(malloc_increase, new_size - old_size);
    }
    else {
        atomic_sub_nounderflow(&malloc_increase, old_size - new_size);
    }

    return true;
}

#define objspace_malloc_increase(...) \
    for (bool malloc_increase_done = false; \
         !malloc_increase_done; \
         malloc_increase_done = objspace_malloc_increase_body(__VA_ARGS__))

static inline void *
objspace_malloc_fixup(rb_objspace_t *objspace, void *mem, size_t size)
{
    size = malloc_size(mem);
    objspace_malloc_increase(objspace, mem, size, 0, MEMOP_TYPE_MALLOC) {}
    return mem;
}

#if defined(__GNUC__) && RUBY_DEBUG
#define RB_BUG_INSTEAD_OF_RB_MEMERROR 1
#endif

#ifndef RB_BUG_INSTEAD_OF_RB_MEMERROR
# define RB_BUG_INSTEAD_OF_RB_MEMERROR 0
#endif

#define GC_MEMERROR(...) \
    ((RB_BUG_INSTEAD_OF_RB_MEMERROR+0) ? rb_bug("" __VA_ARGS__) : rb_memerror())

void rb_gc_impl_set_event_hook(void *objspace_ptr, const rb_event_flag_t event)
{
    rb_objspace_t *objspace = objspace_ptr;
    objspace->hook_events = event & RUBY_INTERNAL_EVENT_OBJSPACE_MASK;
    objspace->flags.has_newobj_hook = !!(objspace->hook_events & RUBY_INTERNAL_EVENT_NEWOBJ);
}

size_t
rb_gc_impl_heap_id_for_size(void *objspace_ptr, size_t size)
{
    return page_slot_size_idx_for_size(size);
}

VALUE
rb_gc_impl_new_obj(void *objspace_ptr, void *cache_ptr, VALUE klass, VALUE flags, bool wb_protected, size_t alloc_size)
{
    rb_objspace_t *objspace = objspace_ptr;

    size_t cache_slot_idx = page_slot_size_idx_for_size(alloc_size);
    size_t slot_size = valid_object_sizes_ordered_idx(cache_slot_idx);
    VALUE obj = newobj_alloc(objspace, cache_slot_idx, slot_size);

    rb_asan_unpoison_object(obj, true);

    MEMZERO((char *)obj, char, slot_size);

    RBASIC(obj)->flags = flags;
    *((VALUE *)&RBASIC(obj)->klass) = klass;

#if RACTOR_CHECK_MODE
    void rb_ractor_setup_belonging(VALUE obj);
    rb_ractor_setup_belonging(obj);
#endif

    gc_report(objspace, "newobj: %s\n", rb_obj_info(obj));

    RUBY_DEBUG_LOG("obj:%p (%s)", (void *)obj, rb_obj_info(obj));

    return obj;
}

bool
rb_gc_impl_pointer_to_heap_p(void *objspace_ptr, const void *ptr)
{
    rb_objspace_t *objspace = objspace_ptr;
    register uintptr_t p = (uintptr_t)ptr;
    register struct heap_page *page;

    if (p < heap_pages_lomem || p > heap_pages_himem) return FALSE;
    if (p % BASE_SLOT_SIZE != 0) return FALSE;
    page = heap_page_for_ptr(objspace, (uintptr_t)ptr);
    if (page) {
        if (p < page->start) return FALSE;
        if (p >= page->start + (page->total_slots * page->slot_size)) return FALSE;
        if ((NUM_IN_PAGE(p) * BASE_SLOT_SIZE) % page->slot_size != 0) return FALSE;

        return TRUE;
    }
    return FALSE;
}

void
rb_gc_impl_make_zombie(void *objspace_ptr, VALUE obj, void (*dfree)(void *), void *data)
{
    rb_objspace_t *objspace = objspace_ptr;

    struct RZombie *zombie = RZOMBIE(obj);
    zombie->flags = T_ZOMBIE | (zombie->flags & ZOMBIE_OBJ_KEPT_FLAGS);
    zombie->dfree = dfree;
    zombie->data = data;
    VALUE prev, next = heap_pages_deferred_final;
    do {
        zombie->next = prev = next;
        next = RUBY_ATOMIC_VALUE_CAS(heap_pages_deferred_final, prev, obj);
    } while (next != prev);

    struct heap_page *page = GET_HEAP_PAGE(obj);
    page->final_slots++;
    heap_pages_final_slots++;
}

void
rb_gc_impl_each_objects(void *objspace_ptr, each_obj_callback *callback, void *data)
{
    objspace_each_objects(objspace_ptr, callback, data, TRUE);
}

VALUE
rb_gc_impl_define_finalizer(void *objspace_ptr, VALUE obj, VALUE block)
{
    rb_objspace_t *objspace = objspace_ptr;
    VALUE table;
    st_data_t data;

    GC_ASSERT(!OBJ_FROZEN(obj));

    RBASIC(obj)->flags |= FL_FINALIZE;

    unsigned int lev = RB_GC_VM_LOCK();

    if (st_lookup(finalizer_table, obj, &data)) {
        table = (VALUE)data;
        VALUE dup_table = rb_ary_dup(table);

        RB_GC_VM_UNLOCK(lev);
        {
            long len = RARRAY_LEN(table);
            long i;

            for (i = 0; i < len; i++) {
                VALUE recv = RARRAY_AREF(dup_table, i);
                if (rb_equal(recv, block)) {
                    return recv;
                }
            }
        }
        lev = RB_GC_VM_LOCK();
        RB_GC_GUARD(dup_table);

        rb_ary_push(table, block);
    }
    else {
        table = rb_ary_new3(2, rb_obj_id(obj), block);
        rb_obj_hide(table);
        st_add_direct(finalizer_table, obj, table);
    }

    RB_GC_VM_UNLOCK(lev);

    return block;
}

void
rb_gc_impl_undefine_finalizer(void *objspace_ptr, VALUE obj)
{
    rb_objspace_t *objspace = objspace_ptr;

    GC_ASSERT(!OBJ_FROZEN(obj));

    st_data_t data = obj;

    int lev = RB_GC_VM_LOCK();
    st_delete(finalizer_table, &data, 0);
    RB_GC_VM_UNLOCK(lev);

    FL_UNSET(obj, FL_FINALIZE);
}

void
rb_gc_impl_copy_finalizer(void *objspace_ptr, VALUE dest, VALUE obj)
{
    rb_objspace_t *objspace = objspace_ptr;
    VALUE table;
    st_data_t data;

    if (!FL_TEST(obj, FL_FINALIZE)) return;

    int lev = RB_GC_VM_LOCK();
    if (RB_LIKELY(st_lookup(finalizer_table, obj, &data))) {
        table = rb_ary_dup((VALUE)data);
        RARRAY_ASET(table, 0, rb_obj_id(dest));
        st_insert(finalizer_table, dest, table);
        FL_SET(dest, FL_FINALIZE);
    }
    else {
        rb_bug("rb_gc_copy_finalizer: FL_FINALIZE set but not found in finalizer_table: %s", rb_obj_info(obj));
    }
    RB_GC_VM_UNLOCK(lev);
}

void
rb_gc_impl_shutdown_free_objects(void *objspace_ptr)
{
    rb_objspace_t *objspace = objspace_ptr;

    for (size_t i = 0; i < heap_allocated_pages; i++) {
        struct heap_page *page = heap_pages_sorted[i];
        short stride = page->slot_size;

        uintptr_t p = (uintptr_t)page->start;
        uintptr_t pend = p + page->total_slots * stride;
        for (; p < pend; p += stride) {
            VALUE vp = (VALUE)p;
            asan_unpoisoning_object(vp) {
                if (RB_BUILTIN_TYPE(vp) != T_NONE) {
                    rb_gc_obj_free_vm_weak_references(vp);
                    if (rb_gc_obj_free(objspace, vp)) {
                        RBASIC(vp)->flags = 0;
                    }
                }
            }
        }
    }
}

static int
shutdown_call_finalizer_i(st_data_t key, st_data_t val, st_data_t _data)
{
    VALUE obj = (VALUE)key;
    VALUE table = (VALUE)val;

    GC_ASSERT(RB_FL_TEST(obj, FL_FINALIZE));
    GC_ASSERT(RB_BUILTIN_TYPE(val) == T_ARRAY);

    rb_gc_run_obj_finalizer(RARRAY_AREF(table, 0), RARRAY_LEN(table) - 1, get_final, (void *)table);

    FL_UNSET(obj, FL_FINALIZE);

    return ST_DELETE;
}

void
rb_gc_impl_shutdown_call_finalizer(void *objspace_ptr)
{
    rb_objspace_t *objspace = objspace_ptr;

    if (RUBY_ATOMIC_EXCHANGE(finalizing, 1)) return;

    while (finalizer_table->num_entries) {
        st_foreach(finalizer_table, shutdown_call_finalizer_i, 0);
    }

    finalize_deferred(objspace);
    GC_ASSERT(heap_pages_deferred_final == 0);

    for (size_t i = 0; i < heap_allocated_pages; i++) {
        struct heap_page *page = heap_pages_sorted[i];
        short stride = page->slot_size;

        uintptr_t p = (uintptr_t)page->start;
        uintptr_t pend = p + page->total_slots * stride;
        for (; p < pend; p += stride) {
            VALUE vp = (VALUE)p;
            asan_unpoisoning_object(vp) {
                if (rb_gc_shutdown_call_finalizer_p(vp)) {
                    rb_gc_obj_free(objspace, vp);
                }
            }
        }
    }

    finalize_deferred_heap_pages(objspace);

    st_free_table(finalizer_table);
    finalizer_table = 0;
    RUBY_ATOMIC_SET(finalizing, 0);
}

void
rb_gc_impl_each_object(void *objspace_ptr, void (*func)(VALUE obj, void *data), void *data)
{
    rb_objspace_t *objspace = objspace_ptr;

    for (size_t i = 0; i < heap_allocated_pages; i++) {
        struct heap_page *page = heap_pages_sorted[i];
        short stride = page->slot_size;

        uintptr_t p = (uintptr_t)page->start;
        uintptr_t pend = p + page->total_slots * stride;
        for (; p < pend; p += stride) {
            VALUE obj = (VALUE)p;

            asan_unpoisoning_object(obj) {
                func(obj, data);
            }
        }
    }
}

VALUE
rb_gc_impl_stat(void *objspace_ptr, VALUE hash_or_sym)
{
    rb_objspace_t *objspace = objspace_ptr;
    VALUE hash = Qnil, key = Qnil;

    setup_gc_stat_symbols();

    if (RB_TYPE_P(hash_or_sym, T_HASH)) {
        hash = hash_or_sym;
    }
    else if (SYMBOL_P(hash_or_sym)) {
        key = hash_or_sym;
    }
    else {
        rb_raise(rb_eTypeError, "non-hash or symbol argument");
    }

#define SET(name, attr) \
    if (key == gc_stat_symbols[gc_stat_sym_##name]) \
        return SIZET2NUM(attr); \
    else if (hash != Qnil) \
        rb_hash_aset(hash, gc_stat_symbols[gc_stat_sym_##name], SIZET2NUM(attr));

    SET(heap_allocated_pages, heap_allocated_pages);
    SET(heap_sorted_length, heap_pages_sorted_length);
    SET(heap_allocatable_pages, objspace->heap.allocatable_pages);
    SET(heap_available_slots, objspace->heap.total_slots);
    SET(heap_live_slots, objspace_live_slots(objspace));
    SET(heap_free_slots, objspace_free_slots(objspace));
    SET(heap_final_slots, heap_pages_final_slots);
    SET(heap_eden_pages, objspace->heap.total_pages);
    SET(total_allocated_pages, objspace->heap.total_allocated_pages);
    SET(total_allocated_objects, objspace->heap.total_allocated_objects);
    SET(malloc_increase_bytes, malloc_increase);
#undef SET

    if (!NIL_P(key)) {
        rb_raise(rb_eArgError, "unknown key: %"PRIsVALUE, rb_sym2str(key));
    }

    return hash;
}

static void
setup_gc_stat_heap_symbols(void)
{
    if (gc_stat_heap_symbols[0] == 0) {
#define S(s) gc_stat_heap_symbols[gc_stat_heap_sym_##s] = ID2SYM(rb_intern_const(#s))
        S(heap_allocatable_pages);
        S(heap_eden_pages);
        S(heap_eden_slots);
        S(total_allocated_pages);
        S(total_allocated_objects);
#undef S
    }
}

VALUE
rb_gc_impl_stat_heap(void *objspace_ptr, VALUE heap_name, VALUE hash_or_sym)
{
    rb_objspace_t *objspace = objspace_ptr;
    VALUE hash = Qnil, key = Qnil;

    setup_gc_stat_heap_symbols();

    if (RB_TYPE_P(hash_or_sym, T_HASH)) {
        hash = hash_or_sym;
    }
    else if (SYMBOL_P(hash_or_sym)) {
        key = hash_or_sym;
    }
    else {
        rb_raise(rb_eTypeError, "non-hash or symbol argument");
    }

#define SET(name, attr) \
    if (key == gc_stat_heap_symbols[gc_stat_heap_sym_##name]) \
        return SIZET2NUM(attr); \
    else if (hash != Qnil) \
        rb_hash_aset(hash, gc_stat_heap_symbols[gc_stat_heap_sym_##name], SIZET2NUM(attr));

    SET(heap_allocatable_pages, objspace->heap.allocatable_pages);
    SET(heap_eden_pages, objspace->heap.total_pages);
    SET(heap_eden_slots, objspace->heap.total_slots);
    SET(total_allocated_pages, objspace->heap.total_allocated_pages);
    SET(total_allocated_objects, objspace->heap.total_allocated_objects);
#undef SET

    if (!NIL_P(key)) {
        rb_raise(rb_eArgError, "unknown key: %"PRIsVALUE, rb_sym2str(key));
    }

    return hash;
}

void
rb_gc_impl_free(void *objspace_ptr, void *ptr, size_t old_size)
{
    rb_objspace_t *objspace = objspace_ptr;

    if (!ptr) {
        return;
    }
    old_size = malloc_size(ptr);

    objspace_malloc_increase(objspace, ptr, 0, old_size, MEMOP_TYPE_FREE) {
        free(ptr);
        ptr = NULL;
    }
}

void *
rb_gc_impl_malloc(void *objspace_ptr, size_t size, bool gc_allowed)
{
    rb_objspace_t *objspace = objspace_ptr;

    if (size == 0) size = 1;
    void *mem = malloc(size);
    if (!mem) return NULL;
    return objspace_malloc_fixup(objspace, mem, size);
}

void *
rb_gc_impl_calloc(void *objspace_ptr, size_t size, bool gc_allowed)
{
    rb_objspace_t *objspace = objspace_ptr;

    if (size == 0) size = 1;
    void *mem = calloc1(size);
    if (!mem) return NULL;
    return objspace_malloc_fixup(objspace, mem, size);
}

void *
rb_gc_impl_realloc(void *objspace_ptr, void *ptr, size_t new_size, size_t old_size, bool gc_allowed)
{
    rb_objspace_t *objspace = objspace_ptr;
    void *mem;

    if (!ptr) return rb_gc_impl_malloc(objspace, new_size, gc_allowed);

    if (new_size == 0) {
        if ((mem = rb_gc_impl_malloc(objspace, 0, gc_allowed)) != NULL) {
            rb_gc_impl_free(objspace, ptr, old_size);
            return mem;
        }
        else {
            new_size = 1;
        }
    }

    old_size = malloc_size(ptr);
    mem = RB_GNUC_EXTENSION_BLOCK(realloc(ptr, new_size));
    if (!mem) return NULL;
    new_size = malloc_size(mem);

    objspace_malloc_increase(objspace, mem, new_size, old_size, MEMOP_TYPE_REALLOC);

    return mem;
}

void
rb_gc_impl_adjust_memory_usage(void *objspace_ptr, ssize_t diff)
{
    rb_objspace_t *objspace = objspace_ptr;

    if (diff > 0) {
        objspace_malloc_increase(objspace, 0, diff, 0, MEMOP_TYPE_REALLOC);
    }
    else if (diff < 0) {
        objspace_malloc_increase(objspace, 0, 0, -diff, MEMOP_TYPE_REALLOC);
    }
}

void *
rb_gc_impl_objspace_alloc(void)
{
    rb_objspace_t *objspace = calloc1(sizeof(rb_objspace_t));
    return objspace;
}

void
rb_gc_impl_objspace_init(void *objspace_ptr)
{
#if defined(INIT_HEAP_PAGE_ALLOC_USE_MMAP)
    heap_page_alloc_use_mmap = INIT_HEAP_PAGE_ALLOC_USE_MMAP;
#endif

    rb_objspace_t *objspace = objspace_ptr;

    objspace->finalize_deferred_pjob = rb_postponed_job_preregister(0, gc_finalize_deferred, objspace);
    if (objspace->finalize_deferred_pjob == POSTPONED_JOB_HANDLE_INVALID) {
        rb_bug("Could not preregister postponed job for GC");
    }

    ccan_list_head_init(&objspace->heap.pages);

    objspace->heap.allocatable_pages = OBJ_SIZE_MULTIPLES * 10;
    heap_pages_expand_sorted(objspace);

    for (int i = 0; i < OBJ_SIZE_MULTIPLES; i++) {
        rb_heap_page_t *page = heap_prepare(objspace, (1 << i) * BASE_SLOT_SIZE);
        objspace->free_page_cache[i] = page;
    }

    finalizer_table = st_init_numtable();
}

size_t
rb_gc_impl_obj_slot_size(VALUE obj)
{
    return GET_HEAP_PAGE(obj)->slot_size - RVALUE_OVERHEAD;
}

void
rb_gc_impl_init(void)
{
    VALUE gc_constants = rb_hash_new();
    rb_hash_aset(gc_constants, ID2SYM(rb_intern("BASE_SLOT_SIZE")), SIZET2NUM(BASE_SLOT_SIZE - RVALUE_OVERHEAD));
    rb_hash_aset(gc_constants, ID2SYM(rb_intern("HEAP_PAGE_OBJ_LIMIT")), SIZET2NUM(HEAP_PAGE_OBJ_LIMIT));
    rb_hash_aset(gc_constants, ID2SYM(rb_intern("HEAP_PAGE_BITMAP_SIZE")), SIZET2NUM(0));
    rb_hash_aset(gc_constants, ID2SYM(rb_intern("HEAP_PAGE_SIZE")), SIZET2NUM(HEAP_PAGE_SIZE));
    rb_hash_aset(gc_constants, ID2SYM(rb_intern("OBJ_SIZE_MULTIPLES")), LONG2FIX(OBJ_SIZE_MULTIPLES));
    rb_hash_aset(gc_constants, ID2SYM(rb_intern("RVARGC_MAX_ALLOCATE_SIZE")),
            LONG2FIX(valid_object_sizes_ordered_idx(OBJ_SIZE_MULTIPLES - 1) - RVALUE_OVERHEAD));
    if (RB_BUG_INSTEAD_OF_RB_MEMERROR+0) {
        rb_hash_aset(gc_constants, ID2SYM(rb_intern("RB_BUG_INSTEAD_OF_RB_MEMERROR")), Qtrue);
    }
    OBJ_FREEZE(gc_constants);
    rb_define_const(rb_mGC, "INTERNAL_CONSTANTS", gc_constants);

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

struct rb_gc_object_metadata_entry *
rb_gc_impl_object_metadata(void *objspace_ptr, VALUE obj)
{
    static struct rb_gc_object_metadata_entry entries[2];
    static ID id_object_id;

    if (!id_object_id) {
        id_object_id = rb_intern("object_id");
    }

    size_t n = 0;
    if (rb_obj_id_p(obj)) {
        entries[n].name = id_object_id;
        entries[n].val = rb_obj_id(obj);
        n++;
    }

    entries[n].name = 0;
    entries[n].val = 0;

    return entries;
}

const char *
rb_gc_impl_active_gc_name(void)
{
    return "epsilon";
}

VALUE
rb_gc_impl_config_get(void *objspace_ptr)
{
    return rb_hash_new();
}

void
rb_gc_impl_config_set(void *objspace_ptr, VALUE hash)
{
}

size_t rb_gc_impl_gc_count(void *objspace)                                      { return 0; }
void * rb_gc_impl_ractor_cache_alloc(void *objspace, void *ractor)              { return NULL; }
VALUE rb_gc_impl_stress_get(void *objspace)                                     { return Qfalse; }
bool rb_gc_impl_get_measure_total_time(void *objspace)                          { return false; }
unsigned long long rb_gc_impl_get_total_time(void *objspace)                    { return 0; }
VALUE rb_gc_impl_location(void *objspace_ptr, VALUE value)                      { return value; }
VALUE rb_gc_impl_latest_gc_info(void *objspace_ptr, VALUE key)                  { return Qnil; }
bool rb_gc_impl_object_moved_p(void *objspace, VALUE obj)                       { return FALSE; }
bool rb_gc_impl_during_gc_p(void *objspace)                                     { return FALSE; }
bool rb_gc_impl_gc_enabled_p(void *objspace)                                    { return FALSE; }
bool rb_gc_impl_garbage_object_p(void *objspace, VALUE ptr)                     { return false; }
bool rb_gc_impl_handle_weak_references_alive_p(void *objspace, VALUE obj)       { return false; }

void rb_gc_impl_stress_set(void *objspace_ptr, VALUE flag)                      { }
void rb_gc_impl_set_measure_total_time(void *objspace, VALUE f)                 { }
void rb_gc_impl_set_params(void *objspace_ptr)                                  { }
void rb_gc_impl_gc_enable(void *objspace_ptr)                                   { }
void rb_gc_impl_gc_disable(void *objspace_ptr, bool finish_current_gc)          { }
void rb_gc_impl_mark_and_move(void *objspace_ptr, VALUE *ptr)                   { }
void rb_gc_impl_mark(void *objspace_ptr, VALUE obj)                             { }
void rb_gc_impl_mark_and_pin(void *objspace_ptr, VALUE obj)                     { }
void rb_gc_impl_mark_maybe(void *objspace_ptr, VALUE obj)                       { }
void rb_gc_impl_declare_weak_references(void *objspace_ptr, VALUE obj)          { }
void rb_gc_impl_register_pinning_obj(void *objspace_ptr, VALUE obj)             { }
void rb_gc_impl_writebarrier(void *objspace_ptr, VALUE a, VALUE b)              { }
void rb_gc_impl_writebarrier_unprotect(void *objspace_ptr, VALUE obj)           { }
void rb_gc_impl_copy_attributes(void *objspace_ptr, VALUE des, VALUE obj)       { }
void rb_gc_impl_writebarrier_remember(void *objspace_ptr, VALUE obj)            { }
void rb_gc_impl_ractor_cache_free(void *objspace_ptr, void *cache)              { }
void rb_gc_impl_prepare_heap(void *objspace_ptr)                                { }
void rb_gc_impl_start(void *objspace_ptr, bool f, bool m, bool s, bool c)      { }
void rb_gc_impl_objspace_free(void *objspace_ptr)                               { }
void rb_gc_impl_before_fork(void *objspace_ptr)                                 { }
void rb_gc_impl_after_fork(void *objspace_ptr, rb_pid_t pid)                    { }
