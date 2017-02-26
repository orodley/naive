#define _GNU_SOURCE
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
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

void *malloc(size_t size)
{
	HeapEntryHeader *curr_entry = heap_free_list;
	HeapEntryHeader *prev_entry = NULL;
	while (curr_entry != NULL) {
		if (curr_entry->size >= size) {
			if (prev_entry == NULL) {
				heap_free_list = curr_entry->next;
			} else {
				prev_entry->next = curr_entry->next;
			}

			return (uint8_t *)curr_entry + sizeof(HeapEntryHeader);
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

	return (uint8_t *)new_header + sizeof(HeapEntryHeader);
}

// We put this in the same TU because when do you ever use malloc without using
// free, and then we don't need a separate header for the implementation
// details.
void free(void *ptr)
{
	HeapEntryHeader *header =
		(HeapEntryHeader *)((uint8_t *)ptr - sizeof(HeapEntryHeader));
	header->next = heap_free_list;
	heap_free_list = header;
}
