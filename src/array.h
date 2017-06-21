// Basic dynamic array

#ifndef NAIVE_ARRAY_H_
#define NAIVE_ARRAY_H_

#include "misc.h"

typedef struct Array_
{
	u8 *elements;
	u32 size;
	u32 capacity;
} Array_;

// This is just used to make types self-documenting
#define Array(T) Array_

void _array_init(Array_ *array, u32 element_size, u32 initial_capacity);
void _array_ensure_room(Array_ *array, u32 element_size, u32 count);
void _array_append_elems(Array_ *array, u32 element_size, u32 size, void *elems);
void *_array_insert(Array_ *array, u32 element_size, u32 insertion_point);
void _array_remove(Array_ *array, u32 element_size, u32 removal_point);

void array_delete_last(Array_ *array);
void array_free(Array_ *array);
void array_clear(Array_ *array);

#define ARRAY_IS_VALID(array) ((array)->elements != NULL)
#define EMPTY_ARRAY ((Array_){ NULL, 0, 0 })
#define ARRAY_ENSURE_ROOM(array, element_type, extra_elems) \
	_array_ensure_room(array, sizeof(element_type), extra_elems)
#define ARRAY_INIT(array, element_type, initial_capacity) \
	_array_init((array), sizeof(element_type), (initial_capacity))
#define ARRAY_REF(array, element_type, i) \
	((element_type *)&(array)->elements[(i) * sizeof(element_type)])
#define ARRAY_LAST(array, element_type) \
	ARRAY_REF((array), element_type, (array)->size - 1)
#define ARRAY_APPEND(array, element_type) \
	(ARRAY_ENSURE_ROOM(array, element_type, 1), \
	 ARRAY_REF((array), element_type, (array)->size++))
#define ARRAY_APPEND_ELEMS(array, element_type, size, elems) \
	_array_append_elems(array, sizeof(element_type), size, elems)
#define ARRAY_POP(array, element_type) \
	ARRAY_REF((array), element_type, --(array)->size)
#define ARRAY_INSERT(array, element_type, insertion_point) \
	(element_type *)_array_insert((array), sizeof(element_type), (insertion_point))
#define ARRAY_REMOVE(array, element_type, removal_point) \
	_array_remove((array), sizeof(element_type), (removal_point))

#endif
