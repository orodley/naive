#include <errno.h>
#include <sys/types.h>

#include "syscall.h"

int fchmod(int fd, mode_t mode)
{
	int ret = __syscall(91, fd, mode, 0, 0, 0, 0);
	if (ret < 0) {
		errno = -ret;
		return -1;
	}

	return 0;
}
