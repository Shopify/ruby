#ifndef RUBY_DARY_H
#define RUBY_DARY_H

#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>

// Type for a dynamic array. Use to declare a dynamic array.
// It is a pointer so it fits in st_table nicely. Designed
// to be fairly type-safe.
//
// NULL is a valid empty dynamic array.
//
// Example:
//      rb_dary(char) char_array = NULL;
//      if (!rb_dary_append(&char_array, 'e')) abort();
//      printf("pushed %c\n", *rb_dary_ref(char_array, 0));
//      rb_dary_free(char_array);
//
#define rb_dary(T) struct { rb_dary_meta_t meta; T data[]; } *

// Copy an element out of the array. Warning: not bounds checked.
//
// T rb_dary_get(rb_dary(T) ary, int32_t idx);
//
#define rb_dary_get(ary, idx) ((ary)->data[(idx)])

// Assign to an element. Warning: not bounds checked.
//
// void rb_dary_set(rb_dary(T) ary, int32_t idx, T element);
//
#define rb_dary_set(ary, idx, element) ((ary)->data[(idx)] = (element))

// Get a pointer to an element. Warning: not bounds checked.
//
// T *rb_dary_ref(rb_dary(T) ary, int32_t idx);
//
#define rb_dary_ref(ary, idx) (&((ary)->data[(idx)]))

// Copy a new element into the array. Return 1 on success and 0 on failure.
// ptr_to_ary is evaluated multiple times.
//
// bool rb_dary_append(rb_dary(T) *ptr_to_ary, T element);
//
#define rb_dary_append(ptr_to_ary, element) ( \
    rb_dary_ensure_space((ptr_to_ary)) ? (    \
        rb_dary_set(*(ptr_to_ary),         \
                    (*(ptr_to_ary))->meta.size, \
                    (element)),            \
        ++((*(ptr_to_ary))->meta.size),       \
        1                                     \
    ) : 0)

// Iterate over items of the array in a for loop
//
#define rb_dary_foreach(ary, idx_name, elem_ptr_var) \
    for (int idx_name = 0; idx_name < rb_dary_size(ary) && ((elem_ptr_var) = rb_dary_ref(ary, idx_name)); ++idx_name)

typedef struct rb_dary_meta {
    int32_t size;
    int32_t capa;
} rb_dary_meta_t;

// Get the size of the dynamic array.
//
static inline int32_t
rb_dary_size(const void *ary)
{
    const rb_dary_meta_t *meta = ary;
    return meta ? meta->size : 0;
}

// Get the capacity of the dynamic array.
//
static inline int32_t
rb_dary_capa(const void *ary)
{
    const rb_dary_meta_t *meta = ary;
    return meta ? meta->capa : 0;
}

// Free the dynamic array.
//
static inline void
rb_dary_free(void *ary)
{
    free(ary);
}

// Remove the last element of the array.
//
#define rb_dary_pop_back(ary) ((ary)->meta.size--)

// Internal macro
// Ensure there is space for one more element. Return 1 on success and 0 on failure.
// `ptr_to_ary` is evaluated multiple times.
#define rb_dary_ensure_space(ptr_to_ary) ( \
    (rb_dary_capa(*(ptr_to_ary)) > rb_dary_size(*(ptr_to_ary))) ? \
        1 : \
        rb_dary_double(ptr_to_ary, sizeof((*(ptr_to_ary))->data[0])))

// Internal function
static inline int 
rb_dary_double(void *ptr_to_ary, size_t element_size)
{
    rb_dary_meta_t **ptr_to_ptr_to_meta = ptr_to_ary;
    const rb_dary_meta_t *meta = *ptr_to_ptr_to_meta;
    int32_t current_capa = rb_dary_capa(meta);

    int32_t new_capa;
    // Calculate new capacity
    if (current_capa == 0) {
        new_capa = 1;
    }
    else {
        int64_t doubled = 2 * (int64_t)current_capa;
        new_capa = (int32_t)doubled;
        if (new_capa != doubled) return 0;
    }

    // Calculate new buffer size
    size_t current_buffer_size = element_size * (size_t)current_capa + (meta ? sizeof(*meta) : 0);
    size_t new_buffer_size = element_size * (size_t)new_capa + sizeof(*meta);
    if (new_buffer_size <= current_buffer_size) return 0;

    rb_dary_meta_t *doubled_ary = realloc(*ptr_to_ptr_to_meta, new_buffer_size);
    if (!doubled_ary) return 0;

    if (meta == NULL) {
        // First allocation. Initialize size. On subsequence allocations
        // realloc takes care of carrying over the size.
        doubled_ary->size = 0;
    }

    doubled_ary->capa = new_capa;

    *ptr_to_ptr_to_meta = doubled_ary;
    return 1;
}

#endif /* RUBY_DARY_H */
