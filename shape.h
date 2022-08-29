#ifndef RUBY_SHAPE_H
#define RUBY_SHAPE_H
#define USE_SHAPE_CACHE_P (SIZEOF_UINT64_T == SIZEOF_VALUE)

#if RUBY_DEBUG
typedef uint16_t shape_id_t;
# define SHAPE_BITS 16
#else
typedef uint32_t shape_id_t;
# define SHAPE_BITS 32
#endif

# define SHAPE_MASK (((VALUE)1 << SHAPE_BITS) - 1)

# define SHAPE_FLAG_SHIFT ((SIZEOF_VALUE * 8) - SHAPE_BITS)
# define SHAPE_FLAG_MASK (((VALUE)-1) >> SHAPE_BITS)

# define MAX_SHAPE_ID (SHAPE_MASK - 1)
# define INVALID_SHAPE_ID SHAPE_MASK
# define ROOT_SHAPE_ID 0x0
# define FROZEN_ROOT_SHAPE_ID 0x1

#define SHAPE_ID(shape) ((((rb_shape_t *)shape)->flags >> SHAPE_BITS) & SHAPE_MASK)

struct rb_shape {
    VALUE flags; // Shape ID and frozen status encoded within flags
    struct rb_shape * parent; // Pointer to the parent
    struct rb_id_table * edges; // id_table from ID (ivar) to next shape
    ID edge_name; // ID (ivar) for transition from parent to rb_shape
    uint32_t iv_count;
};

typedef struct rb_shape rb_shape_t;


shape_id_t rb_generic_shape_id(VALUE obj);

static inline shape_id_t
IMEMO_CACHED_SHAPE_ID(VALUE cc)
{
    RBIMPL_ASSERT_TYPE((VALUE)cc, RUBY_T_IMEMO);
    return (shape_id_t)(SHAPE_MASK & (RBASIC(cc)->flags >> SHAPE_FLAG_SHIFT));
}

static inline void
IMEMO_SET_CACHED_SHAPE_ID(VALUE cc, shape_id_t shape_id)
{
    RBIMPL_ASSERT_TYPE((VALUE)cc, RUBY_T_IMEMO);
    RBASIC(cc)->flags &= SHAPE_FLAG_MASK;
    RBASIC(cc)->flags |= ((VALUE)(shape_id) << SHAPE_FLAG_SHIFT);
}

#if USE_SHAPE_CACHE_P
typedef uint32_t attr_index_t;

static inline shape_id_t
RBASIC_SHAPE_ID(VALUE obj)
{
    RUBY_ASSERT(!RB_SPECIAL_CONST_P(obj));
    return (shape_id_t)(SHAPE_MASK & ((RBASIC(obj)->flags) >> SHAPE_FLAG_SHIFT));
}

static inline void
RBASIC_SET_SHAPE_ID(VALUE obj, shape_id_t shape_id)
{
    // Ractors are occupying the upper 32 bits of flags, but only in debug mode
    // Object shapes are occupying top bits
    RBASIC(obj)->flags &= SHAPE_FLAG_MASK;
    RBASIC(obj)->flags |= ((VALUE)(shape_id) << SHAPE_FLAG_SHIFT);
}

static inline shape_id_t
ROBJECT_SHAPE_ID(VALUE obj)
{
    RBIMPL_ASSERT_TYPE(obj, RUBY_T_OBJECT);
    return RBASIC_SHAPE_ID(obj);
}

static inline void
ROBJECT_SET_SHAPE_ID(VALUE obj, shape_id_t shape_id)
{
    RBIMPL_ASSERT_TYPE(obj, RUBY_T_OBJECT);
    RBASIC_SET_SHAPE_ID(obj, shape_id);
}
#else
typedef uint16_t attr_index_t;

shape_id_t rb_generic_shape_id(VALUE obj);

static inline shape_id_t
ROBJECT_SHAPE_ID(VALUE obj)
{
    RBIMPL_ASSERT_TYPE(obj, RUBY_T_OBJECT);
    return (shape_id_t)(SHAPE_MASK & (RBASIC(obj)->flags >> SHAPE_FLAG_SHIFT));
}

static inline void
ROBJECT_SET_SHAPE_ID(VALUE obj, shape_id_t shape_id)
{
    RBASIC(obj)->flags &= SHAPE_FLAG_MASK;
    RBASIC(obj)->flags |= ((VALUE)(shape_id) << SHAPE_FLAG_SHIFT);
}
#endif

bool rb_shape_root_shape_p(rb_shape_t* shape);

rb_shape_t* rb_shape_get_shape_by_id_without_assertion(shape_id_t shape_id);

MJIT_SYMBOL_EXPORT_BEGIN
rb_shape_t* rb_shape_get_shape_by_id(shape_id_t shape_id);
void rb_shape_set_shape(VALUE obj, rb_shape_t* shape);
shape_id_t rb_shape_get_shape_id(VALUE obj);
rb_shape_t* rb_shape_get_shape(VALUE obj);
int rb_shape_frozen_shape_p(rb_shape_t* shape);
void rb_shape_transition_shape_frozen(VALUE obj);
void rb_shape_transition_shape(VALUE obj, ID id, rb_shape_t *shape);
rb_shape_t* rb_shape_get_next(rb_shape_t* shape, VALUE obj, ID id);
int rb_shape_get_iv_index(rb_shape_t * shape, ID id, VALUE * value);
MJIT_SYMBOL_EXPORT_END

rb_shape_t * rb_shape_alloc(shape_id_t shape_id, ID edge_name, rb_shape_t * parent);
struct rb_id_table * rb_shape_generate_iv_table(rb_shape_t* shape);

bool rb_shape_set_shape_id(VALUE obj, shape_id_t shape_id);
void rb_shape_set_shape_by_id(shape_id_t, rb_shape_t *);

VALUE rb_obj_debug_shape(VALUE self, VALUE obj);
#endif
