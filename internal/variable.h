#ifndef INTERNAL_VARIABLE_H                              /*-*-C-*-vi:se ft=c:*/
#define INTERNAL_VARIABLE_H
/**
 * @author     Ruby developers <ruby-core@ruby-lang.org>
 * @copyright  This  file  is   a  part  of  the   programming  language  Ruby.
 *             Permission  is hereby  granted,  to  either redistribute  and/or
 *             modify this file, provided that  the conditions mentioned in the
 *             file COPYING are met.  Consult the file for details.
 * @brief      Internal header for variables.
 */
#include "ruby/internal/config.h"
#include <stddef.h>             /* for size_t */
#include "constant.h"           /* for rb_const_entry_t */
#include "ruby/internal/stdbool.h"     /* for bool */
#include "ruby/ruby.h"          /* for VALUE */

/* global variable */

#define ROBJECT_TRANSIENT_FLAG    FL_USER2

/* variable.c */
void rb_gc_mark_global_tbl(void);
void rb_gc_update_global_tbl(void);
size_t rb_generic_ivar_memsize(VALUE);
VALUE rb_search_class_path(VALUE);
VALUE rb_attr_delete(VALUE, ID);
VALUE rb_ivar_lookup(VALUE obj, ID id, VALUE undef);
void rb_autoload_str(VALUE mod, ID id, VALUE file);
VALUE rb_autoload_at_p(VALUE, ID, int);
NORETURN(VALUE rb_mod_const_missing(VALUE,VALUE));
rb_gvar_getter_t *rb_gvar_getter_function_of(ID);
rb_gvar_setter_t *rb_gvar_setter_function_of(ID);
void rb_gvar_readonly_setter(VALUE v, ID id, VALUE *_);
void rb_gvar_ractor_local(const char *name);
static inline bool ROBJ_TRANSIENT_P(VALUE obj);
static inline void ROBJ_TRANSIENT_SET(VALUE obj);
static inline void ROBJ_TRANSIENT_UNSET(VALUE obj);
uint32_t rb_obj_ensure_iv_index_mapping(VALUE obj, ID id);

struct gen_ivtbl;
int rb_gen_ivtbl_get(VALUE obj, ID id, struct gen_ivtbl **ivtbl);

#ifndef shape_id_t
typedef uint16_t shape_id_t;
#define shape_id_t shape_id_t
#endif

struct rb_shape {
    VALUE flags; // Shape ID and frozen status encoded within flags
    struct rb_shape * parent; // Pointer to the parent
    struct rb_id_table * edges; // id_table from ID (ivar) to next shape
    ID edge_name; // ID (ivar) for transition from parent to rb_shape
    uint32_t iv_count;
};

#define SHAPE_ID(shape) rb_shape_get_shape_id((VALUE)shape)

#ifndef rb_shape_t
typedef struct rb_shape rb_shape_t;
#define rb_shape_t rb_shape_t
#endif

rb_shape_t* rb_shape_get_shape_by_id_without_assertion(shape_id_t shape_id);
rb_shape_t* rb_vm_get_root_shape();
bool rb_shape_root_shape_p(rb_shape_t* shape);
void rb_shape_set_shape_by_id(shape_id_t, rb_shape_t *);
rb_shape_t * rb_shape_alloc(shape_id_t shape_id, ID edge_name, rb_shape_t * parent);
struct rb_id_table * rb_shape_generate_iv_table(rb_shape_t* shape);
shape_id_t rb_generic_shape_id(VALUE obj);

# define MAX_SHAPE_ID 0xFFFE
# define NO_CACHE_SHAPE_ID (0x2)
# define INVALID_SHAPE_ID (MAX_SHAPE_ID + 1)
# define ROOT_SHAPE_ID 0x0
# define FROZEN_ROOT_SHAPE_ID 0x1

RUBY_SYMBOL_EXPORT_BEGIN
/* variable.c (export) */
void rb_mark_generic_ivar(VALUE);
void rb_mv_generic_ivar(VALUE src, VALUE dst);
VALUE rb_const_missing(VALUE klass, VALUE name);
int rb_class_ivar_set(VALUE klass, ID vid, VALUE value);
void rb_iv_tbl_copy(VALUE dst, VALUE src);
RUBY_SYMBOL_EXPORT_END

MJIT_SYMBOL_EXPORT_BEGIN
bool rb_no_cache_shape_p(rb_shape_t * shape);
int rb_shape_get_iv_index(rb_shape_t * shape, ID id, VALUE * value);
rb_shape_t* rb_shape_get_next(rb_shape_t* shape, VALUE obj, ID id);
rb_shape_t* rb_shape_get_shape(VALUE obj);
rb_shape_t* rb_shape_get_shape_by_id(shape_id_t shape_id);
shape_id_t rb_shape_get_shape_id(VALUE obj);
void rb_shape_set_shape(VALUE obj, rb_shape_t* shape);
VALUE rb_gvar_get(ID);
VALUE rb_gvar_set(ID, VALUE);
VALUE rb_gvar_defined(ID);
void rb_const_warn_if_deprecated(const rb_const_entry_t *, VALUE, ID);
void rb_init_iv_list(VALUE obj);
void rb_ensure_iv_list_size(VALUE obj, uint32_t len, uint32_t newsize);
struct gen_ivtbl * rb_ensure_generic_iv_list_size(VALUE obj, uint32_t newsize);
MJIT_SYMBOL_EXPORT_END

static inline bool
ROBJ_TRANSIENT_P(VALUE obj)
{
#if USE_TRANSIENT_HEAP
    return FL_TEST_RAW(obj, ROBJECT_TRANSIENT_FLAG);
#else
    return false;
#endif
}

static inline void
ROBJ_TRANSIENT_SET(VALUE obj)
{
#if USE_TRANSIENT_HEAP
    FL_SET_RAW(obj, ROBJECT_TRANSIENT_FLAG);
#endif
}

static inline void
ROBJ_TRANSIENT_UNSET(VALUE obj)
{
#if USE_TRANSIENT_HEAP
    FL_UNSET_RAW(obj, ROBJECT_TRANSIENT_FLAG);
#endif
}

#endif /* INTERNAL_VARIABLE_H */
