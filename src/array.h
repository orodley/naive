// Basic dynamic array

#ifndef NAIVE_ARRAY_H_
#define NAIVE_ARRAY_H_

#include "misc.h"

typedef struct Array_
{
	u8 *elements;

	// @PERF: This could quite possibly be removed, as we have the
	// element type (and therefore size) passed in.
	u32 element_size;
	u32 size;
	u32 capacity;
} Array_;

// This is just used to make types self-documenting
#define Array(T) Array_

void _array_init(Array_ *array, u32 element_size, u32 initial_capacity);
void array_ensure_room(Array_ *array);
void array_delete_last(Array_ *array);
void array_free(Array_ *array);

#define ARRAY_ZEROED ((Array_){ 0, 0, 0, 0 })
#define ARRAY_INIT(array, element_type, initial_capacity) \
	_array_init((array), sizeof(element_type), (initial_capacity))
#define ARRAY_REF(array, element_type, i) \
	((element_type *)&(array)->elements[(i) * sizeof(element_type)])
#define ARRAY_APPEND(array, element_type) \
	(array_ensure_room(array), \
	 ARRAY_REF((array), element_type, (array)->size++))

#endif
