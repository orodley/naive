#define _GNU_SOURCE
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include <sys/mman.h>

static size_t align_to(size_t n, size_t align)
{
	size_t x = align - 1;
	return (n + x) & ~x;
}

typedef struct HeapEntryHeader
{
	size_t size;
	struct HeapEntryHeader *next;
} HeapEntryHeader;

static HeapEntryHeader *heap_free_list = NULL;

static uint8_t *heap_start = NULL;
static size_t heap_size = 0;

uint8_t *ptr_for_header(HeapEntryHeader *header)
{
	return (uint8_t *)header + sizeof(HeapEntryHeader);
}

HeapEntryHeader *header_for_ptr(void *ptr)
{
	return (HeapEntryHeader *)((uint8_t *)ptr - sizeof(HeapEntryHeader));
}

void *malloc(size_t size)
{
	if (size == 0)
		return NULL;

	HeapEntryHeader *curr_entry = heap_free_list;
	HeapEntryHeader *prev_entry = NULL;
	while (curr_entry != NULL) {
		if (curr_entry->size >= size) {
			if (prev_entry == NULL) {
				heap_free_list = curr_entry->next;
			} else {
				prev_entry->next = curr_entry->next;
			}

			return ptr_for_header(curr_entry);
		}

		prev_entry = curr_entry;
		curr_entry = curr_entry->next;
	}

	size_t old_size = heap_size;
	heap_size += align_to(sizeof(HeapEntryHeader) + size, 8);

	if (heap_start == NULL) {
		heap_start = mmap(NULL, heap_size,
				PROT_READ | PROT_WRITE,
				MAP_PRIVATE | MAP_ANONYMOUS,
				-1, 0);

		if (heap_start == MAP_FAILED) {
			return NULL;
		}
	} else {
		if (mremap(heap_start, old_size, heap_size, 0) == MAP_FAILED) {
			return NULL;
		}
	}

	HeapEntryHeader *new_header = (HeapEntryHeader *)(heap_start + old_size);
	new_header->next = NULL;
	new_header->size = size;

	return ptr_for_header(new_header);
}

// We put this in the same TU because when do you ever use malloc without using
// free, and then we don't need a separate header for the implementation
// details.
void free(void *ptr)
{
	if (ptr == NULL)
		return;

	HeapEntryHeader *header = header_for_ptr(ptr);
	header->next = heap_free_list;
	heap_free_list = header;
}

void *realloc(void *ptr, size_t size)
{
	if (ptr == NULL)
		return malloc(size);

	if (size == 0)
		return NULL;

	HeapEntryHeader *header = header_for_ptr(ptr);
	if (header->size >= size)
		return ptr;

	void *new_ptr = malloc(size);
	memcpy(new_ptr, ptr, size);
	free(ptr);

	return new_ptr;
}

void *calloc(size_t nmemb, size_t size)
{
	if (nmemb == 0 || size == 0)
		return NULL;

	void *ptr = malloc(nmemb * size);
	memset(ptr, 0, nmemb * size);
	return ptr;
}
