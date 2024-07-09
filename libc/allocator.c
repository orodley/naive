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

static uint8_t *heap_block = NULL;
static size_t heap_block_used = 0;
static size_t heap_block_size = 0;

static uint8_t *ptr_for_header(HeapEntryHeader *header)
{
  return (uint8_t *)header + sizeof(HeapEntryHeader);
}

static HeapEntryHeader *header_for_ptr(void *ptr)
{
  return (HeapEntryHeader *)((uint8_t *)ptr - sizeof(HeapEntryHeader));
}

void *malloc(size_t size)
{
  if (size == 0) return NULL;

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

  size_t extra = align_to(sizeof(HeapEntryHeader) + size, 8);
  if (heap_block_used + extra > heap_block_size) {
    if (heap_block_size == 0) heap_block_size = 32768;
    while (extra > heap_block_size) heap_block_size *= 2;

    heap_block_used = 0;
    void *new_heap_block = mmap(
        NULL, heap_block_size, PROT_READ | PROT_WRITE,
        MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);

    if (new_heap_block == MAP_FAILED) {
      return NULL;
    }

    heap_block = new_heap_block;
  }

  HeapEntryHeader *new_header =
      (HeapEntryHeader *)(heap_block + heap_block_used);
  heap_block_used += extra;

  new_header->next = NULL;
  new_header->size = size;

  return ptr_for_header(new_header);
}

// We put this in the same TU because when do you ever use malloc without using
// free, and then we don't need a separate header for the implementation
// details.
void free(void *ptr)
{
  if (ptr == NULL) return;

  HeapEntryHeader *header = header_for_ptr(ptr);
  header->next = heap_free_list;
  heap_free_list = header;
}

void *realloc(void *ptr, size_t size)
{
  if (ptr == NULL) return malloc(size);

  if (size == 0) return NULL;

  HeapEntryHeader *header = header_for_ptr(ptr);
  size_t old_size = header->size;
  if (old_size >= size) return ptr;

  void *new_ptr = malloc(size);
  if (new_ptr == NULL) return NULL;

  memcpy(new_ptr, ptr, old_size);
  free(ptr);

  return new_ptr;
}

void *calloc(size_t nmemb, size_t size)
{
  if (nmemb == 0 || size == 0) return NULL;

  void *ptr = malloc(nmemb * size);
  memset(ptr, 0, nmemb * size);
  return ptr;
}
