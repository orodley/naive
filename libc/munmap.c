#include <stddef.h>

#include "syscall.h"

int munmap(void *addr, size_t length)
{
	return __syscall(11, (uint64_t)addr, length, 0, 0, 0, 0);
}
