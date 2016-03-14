#ifndef NAIVE_POOL_H_
#define NAIVE_POOL_H_

#include "misc.h"

typedef struct PoolEntry
{
	size_t used;
	u8 *memory;

	struct PoolEntry *next;
} PoolEntry;

typedef struct Pool
{
	size_t entry_size;

	PoolEntry *first_entry;
	PoolEntry *first_entry_with_space;
} Pool;

void pool_init(Pool *pool, size_t entry_size);
void *pool_alloc(Pool *pool, size_t size);

#endif
