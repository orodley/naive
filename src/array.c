// Basic dynamic array

#include <stdlib.h>
#include <string.h>
#include "array.h"

void _array_init(Array_ *array, u32 element_size, u32 initial_capacity)
{
	array->capacity = initial_capacity;
	array->size = 0;

	if (initial_capacity == 0) {
		array->elements = NULL;
	} else {
		array->elements = malloc(element_size * initial_capacity);
	}
}

void array_ensure_room(Array_ *array, u32 element_size, u32 count)
{
	u32 new_size = array->size + count;
	if (new_size > array->capacity) {
		if (array->capacity == 0) {
			array->capacity = new_size;
		}

		while (new_size > array->capacity) {
			array->capacity *= 2;
		}
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

void _array_append_elems(Array_ *array, u32 element_size, u32 size, void *elems)
{
	array_ensure_room(array, element_size, size);
	u8 *end = array->elements + array->size;
	memcpy(end, elems, size * element_size);
	array->size += size;
}

void *_array_insert(Array_ *array, u32 element_size, u32 insertion_point)
{
	array_ensure_room(array, element_size, 1);
	memmove(array->elements + (insertion_point + 1) * element_size,
			array->elements + insertion_point * element_size,
			(array->size - insertion_point) * element_size);

	array->size++;
	return array->elements + insertion_point * element_size;
}

void _array_remove(Array_ *array, u32 element_size, u32 removal_point)
{
	if (removal_point != array->size - 1) {
		memmove(array->elements + removal_point * element_size,
				array->elements + (removal_point + 1) * element_size,
				(array->size - removal_point) * element_size);
	}

	array->size--;
}

void array_clear(Array_ *array)
{
	array->size = 0;
}
