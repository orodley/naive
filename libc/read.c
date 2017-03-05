#include <errno.h>
#include <stdint.h>
#include <unistd.h>

#include "syscall.h"

ssize_t read(int fd, void *buf, size_t count)
{
	int ret = __syscall(0, fd, (uint64_t)buf, count, 0, 0, 0);
	if (ret < 0) {
		errno = -ret;
		return -1;
	}

	return ret;
}
