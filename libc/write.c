#include <unistd.h>

#include "syscall.h"

ssize_t write(int fd, const void *buf, size_t count)
{
	return __syscall(1, fd, (uint64_t)buf, count, 0, 0, 0);
}
