// Basic dynamic array

#include <stdlib.h>
#include "array.h"

void array_init(Array *array, u32 element_size, u32 initial_capacity)
{
	array->element_size = element_size;
	array->capacity = initial_capacity;
	array->size = 0;

	array->elements = malloc(element_size * initial_capacity);
}

void *array_ref(Array *array, u32 index)
{
	return array->elements + (index * array->element_size);
}

void *array_append(Array *array)
{
	if (array->size >= array->capacity) {
		u32 required_size = array->capacity * 2;
		array->elements = realloc(array->elements, required_size);
	}

	array->size++;

	return array_ref(array, array->size - 1);
}

void array_delete_last(Array *array)
{
	array->size--;
}
