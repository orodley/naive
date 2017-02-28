#include <errno.h>
#include <unistd.h>

#include "syscall.h"

ssize_t write(int fd, const void *buf, size_t count)
{
	int ret = __syscall(1, fd, (uint64_t)buf, count, 0, 0, 0);
	if (ret < 0) {
		errno = -ret;
		return -1;
	}

	return 0;
}
