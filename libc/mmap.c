#include <errno.h>
#include <stddef.h>
#include <sys/mman.h>
#include <sys/types.h>

#include "err.h"
#include "syscall.h"

void *mmap(void *addr, size_t length, int prot, int flags, int fd, off_t offset)
{
	uint64_t ret =
		__syscall(9, (uint64_t)addr, length, prot, flags, fd, offset);
	if (PTR_IS_ERR(ret)) {
		errno = -ret;
		return MAP_FAILED;
	}

	return (void *)ret;
}
