// Basic dynamic array

#include <stdlib.h>
#include "array.h"

void _array_init(Array_ *array, u32 element_size, u32 initial_capacity)
{
	array->capacity = initial_capacity;
	array->size = 0;

	array->elements = malloc(element_size * initial_capacity);
}

void array_ensure_room(Array_ *array, u32 element_size)
{
	if (array->size >= array->capacity) {
		array->capacity *= 2;
		array->elements = realloc(array->elements, array->capacity * element_size);
	}
}

void array_delete_last(Array_ *array)
{
	array->size--;
}

void array_free(Array_ *array)
{
	free(array->elements);
}
