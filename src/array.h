// Basic dynamic array

#ifndef NAIVE_ARRAY_H_
#define NAIVE_ARRAY_H_

#include "misc.h"

typedef struct Array
{
	u8 *elements;

	u32 element_size;
	u32 size;
	u32 capacity;
} Array;

void array_init(Array *array, u32 element_size, u32 initial_capacity);
void *array_ref(Array *array, u32 index);
void *array_append(Array *array);
void array_delete_last(Array *array);

#endif
