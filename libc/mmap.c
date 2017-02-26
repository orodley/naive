#include <stddef.h>
#include <sys/types.h>

#include "syscall.h"

void *mmap(void *addr, size_t length, int prot, int flags, int fd, off_t offset)
{
	uint64_t result =
		__syscall(9, (uint64_t)addr, length, prot, flags, fd, offset);
	if (result < 1) {
		result = -1;
	}

	return (void *)result;
}
