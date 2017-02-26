#include "syscall.h"

int close(int fd)
{
	return __syscall(6, fd, 0, 0, 0, 0, 0);
}
