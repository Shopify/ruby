#ifndef RBIMPL_SHAPE_H
#define RBIMPL_SHAPE_H

typedef uint32_t shape_id_t;
# define SHAPE_ID_NUM_BITS (sizeof(shape_id_t) * CHAR_BIT)

#define SHAPE_IN_BASIC_FLAGS (SIZEOF_UINT64_T == SIZEOF_VALUE)

#endif
