#include <stdlib.h>

#include "pool.h"

static inline void entry_init(PoolEntry *entry, size_t size)
{
	entry->used = 0;
	entry->memory = malloc(size);
	entry->next = NULL;
}

void pool_init(Pool *pool, size_t entry_size)
{
	pool->first_entry = malloc(sizeof *pool->first_entry);
	entry_init(pool->first_entry, entry_size);
	pool->first_entry_with_space = pool->first_entry;
	pool->entry_size = entry_size;
}

void *pool_alloc(Pool *pool, size_t size)
{
	PoolEntry *entry = pool->first_entry_with_space;
	if (entry->used + size > pool->entry_size) {
		PoolEntry *new_entry = malloc(sizeof *new_entry);
		entry_init(new_entry, pool->entry_size);
		pool->first_entry_with_space->next = new_entry;
		pool->first_entry_with_space = new_entry;

		entry = new_entry;
	}

	entry->used += size;

	return entry->memory + entry->used - size;
}

void pool_free(Pool *pool)
{
	PoolEntry *entry = pool->first_entry;

	while (entry != NULL) {
		free(entry->memory);
		PoolEntry *next_entry = entry->next;
		free(entry);

		entry = next_entry;
	}
}
