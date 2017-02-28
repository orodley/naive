#include <errno.h>

#include "syscall.h"

int close(int fd)
{
	int ret = __syscall(6, fd, 0, 0, 0, 0, 0);
	if (ret < 0) {
		errno = -ret;
		return -1;
	}

	return 0;
}
