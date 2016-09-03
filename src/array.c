// Basic dynamic array

#include <stdlib.h>
#include <string.h>
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

void *_array_insert(Array_ *array, u32 element_size, u32 insertion_point)
{
	array_ensure_room(array, element_size);
	for (u32 i = array->size; i > insertion_point; i--) {
		memcpy(array->elements + i * element_size,
				array->elements + (i - 1) * element_size,
				element_size);
	}

	array->size++;
	return array->elements + insertion_point * element_size;
}

void _array_remove(Array_ *array, u32 element_size, u32 removal_point)
{
	for (u32 i = removal_point; i < array->size - 1; i++) {
		memcpy(array->elements + i * element_size,
				array->elements + (i + 1) * element_size,
				element_size);
	}

	array->size--;
}
