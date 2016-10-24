#include <stdlib.h>

#include "pool.h"

static inline void block_init(PoolBlock *block, size_t size)
{
	block->used = 0;
	block->memory = malloc(size);
	block->next = NULL;
}

void pool_init(Pool *pool, size_t block_size)
{
	pool->first_block = malloc(sizeof *pool->first_block);
	block_init(pool->first_block, block_size);
	pool->first_block_with_space = pool->first_block;
	pool->block_size = block_size;
}

void *pool_alloc(Pool *pool, size_t size)
{
	PoolBlock *block = pool->first_block_with_space;
	if (block->used + size > pool->block_size) {
		PoolBlock *new_block = malloc(sizeof *new_block);
		block_init(new_block, pool->block_size);
		pool->first_block_with_space->next = new_block;
		pool->first_block_with_space = new_block;

		block = new_block;
	}

	void *ptr = block->memory + block->used;
	block->used += size;

	return ptr;
}

void pool_free(Pool *pool)
{
	PoolBlock *block = pool->first_block;

	while (block != NULL) {
		PoolBlock *next_block = block->next;

		free(block->memory);
		free(block);

		block = next_block;
	}
}
