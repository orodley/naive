#ifndef NAIVE_POOL_H_
#define NAIVE_POOL_H_

#include "misc.h"

#if RUNNING_UNDER_SANITIZER

#include "array.h"

typedef struct Pool
{
	Array(void *) allocated_pointers;
} Pool;

#else

typedef struct PoolBlock
{
	size_t used;
	u8 *memory;

	struct PoolBlock *next;
} PoolBlock;

typedef struct Pool
{
	size_t block_size;

	PoolBlock *first_block;
	PoolBlock *first_block_with_space;
} Pool;

#endif

void pool_init(Pool *pool, size_t block_size);
void *pool_alloc(Pool *pool, size_t size);
void pool_free(Pool *pool);

#endif
