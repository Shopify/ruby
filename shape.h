#ifndef RUBY_SHAPE_H
#define RUBY_SHAPE_H

#include "internal/gc.h"

#if (SIZEOF_UINT64_T <= SIZEOF_VALUE)

#define SIZEOF_SHAPE_T 4
typedef uint32_t attr_index_t;
typedef uint32_t shape_id_t;
# define SHAPE_ID_NUM_BITS 32

#else

#define SIZEOF_SHAPE_T 2
typedef uint16_t attr_index_t;
typedef uint16_t shape_id_t;
# define SHAPE_ID_NUM_BITS 16

#endif

typedef uint32_t redblack_id_t;

#define SHAPE_MAX_FIELDS (attr_index_t)(-1)

# define SHAPE_FLAG_MASK (((VALUE)-1) >> SHAPE_ID_NUM_BITS)

# define SHAPE_FLAG_SHIFT ((SIZEOF_VALUE * 8) - SHAPE_ID_NUM_BITS)

# define SHAPE_MAX_VARIATIONS 8

# define INVALID_SHAPE_ID (((uintptr_t)1 << SHAPE_ID_NUM_BITS) - 1)
#define ATTR_INDEX_NOT_SET (attr_index_t)-1

#define ROOT_SHAPE_ID               0x0
#define SPECIAL_CONST_SHAPE_ID      0x1
//      ROOT_TOO_COMPLEX_SHAPE_ID   0x2
#define FIRST_T_OBJECT_SHAPE_ID     0x3

extern ID ruby_internal_object_id;

typedef struct redblack_node redblack_node_t;

struct rb_shape {
    VALUE edges; // id_table from ID (ivar) to next shape
    ID edge_name; // ID (ivar) for transition from parent to rb_shape
    attr_index_t next_field_index; // Fields are either ivars or internal properties like `object_id`
    attr_index_t capacity; // Total capacity of the object with this shape
    uint8_t type;
    uint8_t heap_index;
    uint8_t flags;
    shape_id_t parent_id;
    redblack_node_t *ancestor_index;
};

typedef struct rb_shape rb_shape_t;

struct redblack_node {
    ID key;
    rb_shape_t *value;
    redblack_id_t l;
    redblack_id_t r;
};

enum shape_type {
    SHAPE_ROOT,
    SHAPE_IVAR,
    SHAPE_OBJ_ID,
    SHAPE_FROZEN,
    SHAPE_T_OBJECT,
    SHAPE_OBJ_TOO_COMPLEX,
};

typedef struct {
    /* object shapes */
    rb_shape_t *shape_list;
    rb_shape_t *root_shape;
    rb_atomic_t next_shape_id;

    redblack_node_t *shape_cache;
    unsigned int cache_size;
} rb_shape_tree_t;
RUBY_EXTERN rb_shape_tree_t *rb_shape_tree_ptr;

static inline rb_shape_tree_t *
rb_current_shape_tree(void)
{
    return rb_shape_tree_ptr;
}
#define GET_SHAPE_TREE() rb_current_shape_tree()

static inline shape_id_t
RBASIC_SHAPE_ID(VALUE obj)
{
    RUBY_ASSERT(!RB_SPECIAL_CONST_P(obj));
    RUBY_ASSERT(!RB_TYPE_P(obj, T_IMEMO));
#if RBASIC_SHAPE_ID_FIELD
    return (shape_id_t)((RBASIC(obj)->shape_id));
#else
    return (shape_id_t)((RBASIC(obj)->flags) >> SHAPE_FLAG_SHIFT);
#endif
}

static inline void
RBASIC_SET_SHAPE_ID(VALUE obj, shape_id_t shape_id)
{
    RUBY_ASSERT(!RB_SPECIAL_CONST_P(obj));
    RUBY_ASSERT(!RB_TYPE_P(obj, T_IMEMO));
#if RBASIC_SHAPE_ID_FIELD
    RBASIC(obj)->shape_id = (VALUE)shape_id;
#else
    // Ractors are occupying the upper 32 bits of flags, but only in debug mode
    // Object shapes are occupying top bits
    RBASIC(obj)->flags &= SHAPE_FLAG_MASK;
    RBASIC(obj)->flags |= ((VALUE)(shape_id) << SHAPE_FLAG_SHIFT);
#endif
}

#define RSHAPE rb_shape_lookup

int32_t rb_shape_id_offset(void);

RUBY_FUNC_EXPORTED rb_shape_t *rb_shape_lookup(shape_id_t shape_id);
RUBY_FUNC_EXPORTED shape_id_t rb_obj_shape_id(VALUE obj);
shape_id_t rb_shape_get_next_iv_shape(shape_id_t shape_id, ID id);
bool rb_shape_get_iv_index(shape_id_t shape_id, ID id, attr_index_t *value);
bool rb_shape_get_iv_index_with_hint(shape_id_t shape_id, ID id, attr_index_t *value, shape_id_t *shape_id_hint);
RUBY_FUNC_EXPORTED bool rb_shape_obj_too_complex_p(VALUE obj);
bool rb_shape_too_complex_p(shape_id_t shape_id);
bool rb_shape_has_object_id(shape_id_t shape_id);

shape_id_t rb_shape_transition_frozen(VALUE obj);
shape_id_t rb_shape_transition_complex(VALUE obj);
shape_id_t rb_shape_transition_remove_ivar(VALUE obj, ID id, shape_id_t *removed_shape_id);
shape_id_t rb_shape_transition_add_ivar(VALUE obj, ID id);
shape_id_t rb_shape_transition_add_ivar_no_warnings(VALUE obj, ID id);
shape_id_t rb_shape_transition_object_id(VALUE obj);

void rb_shape_free_all(void);

shape_id_t rb_shape_rebuild(shape_id_t initial_shape_id, shape_id_t dest_shape_id);
void rb_shape_copy_fields(VALUE dest, VALUE *dest_buf, shape_id_t dest_shape_id, VALUE src, VALUE *src_buf, shape_id_t src_shape_id);
void rb_shape_copy_complex_ivars(VALUE dest, VALUE obj, shape_id_t src_shape_id, st_table *fields_table);

static inline bool
rb_shape_canonical_p(shape_id_t shape_id)
{
    return !RSHAPE(shape_id)->flags;
}

static inline shape_id_t
rb_shape_root(size_t heap_id)
{
    return (shape_id_t)(heap_id + FIRST_T_OBJECT_SHAPE_ID);
}

static inline bool
RSHAPE_TYPE_P(shape_id_t shape_id, enum shape_type type)
{
    return RSHAPE(shape_id)->type == type;
}

static inline attr_index_t
RSHAPE_CAPACITY(shape_id_t shape_id)
{
    return RSHAPE(shape_id)->capacity;
}

static inline attr_index_t
RSHAPE_LEN(shape_id_t shape_id)
{
    return RSHAPE(shape_id)->next_field_index;
}

static inline attr_index_t
RSHAPE_INDEX(shape_id_t shape_id)
{
    return RSHAPE_LEN(shape_id) - 1;
}

static inline ID
RSHAPE_EDGE_NAME(shape_id_t shape_id)
{
    return RSHAPE(shape_id)->edge_name;
}

static inline uint32_t
ROBJECT_FIELDS_CAPACITY(VALUE obj)
{
    RBIMPL_ASSERT_TYPE(obj, RUBY_T_OBJECT);
    // Asking for capacity doesn't make sense when the object is using
    // a hash table for storing instance variables
    RUBY_ASSERT(!rb_shape_obj_too_complex_p(obj));
    return RSHAPE(RBASIC_SHAPE_ID(obj))->capacity;
}

static inline st_table *
ROBJECT_FIELDS_HASH(VALUE obj)
{
    RBIMPL_ASSERT_TYPE(obj, RUBY_T_OBJECT);
    RUBY_ASSERT(rb_shape_obj_too_complex_p(obj));
    return (st_table *)ROBJECT(obj)->as.heap.fields;
}

static inline void
ROBJECT_SET_FIELDS_HASH(VALUE obj, const st_table *tbl)
{
    RBIMPL_ASSERT_TYPE(obj, RUBY_T_OBJECT);
    RUBY_ASSERT(rb_shape_obj_too_complex_p(obj));
    ROBJECT(obj)->as.heap.fields = (VALUE *)tbl;
}

static inline uint32_t
ROBJECT_FIELDS_COUNT(VALUE obj)
{
    if (rb_shape_obj_too_complex_p(obj)) {
        return (uint32_t)rb_st_table_size(ROBJECT_FIELDS_HASH(obj));
    }
    else {
        RBIMPL_ASSERT_TYPE(obj, RUBY_T_OBJECT);
        RUBY_ASSERT(!rb_shape_obj_too_complex_p(obj));
        return RSHAPE(RBASIC_SHAPE_ID(obj))->next_field_index;
    }
}

static inline uint32_t
RBASIC_FIELDS_COUNT(VALUE obj)
{
    return RSHAPE(rb_obj_shape_id(obj))->next_field_index;
}

shape_id_t rb_shape_traverse_from_new_root(shape_id_t initial_shape_id, shape_id_t orig_shape_id);

bool rb_obj_set_shape_id(VALUE obj, shape_id_t shape_id);

static inline bool
rb_shape_obj_has_id(VALUE obj)
{
    return rb_shape_has_object_id(RBASIC_SHAPE_ID(obj));
}

// For ext/objspace
RUBY_SYMBOL_EXPORT_BEGIN
typedef void each_shape_callback(shape_id_t shape_id, void *data);
void rb_shape_each_shape_id(each_shape_callback callback, void *data);
size_t rb_shape_memsize(shape_id_t shape);
size_t rb_shape_edges_count(shape_id_t shape_id);
size_t rb_shape_depth(shape_id_t shape_id);
RUBY_SYMBOL_EXPORT_END

#endif
