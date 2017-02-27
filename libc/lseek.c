#include <sys/types.h>

#include "syscall.h"

off_t lseek(int fd, off_t offset, int whence)
{
	return __syscall(8, fd, offset, whence, 0, 0, 0);
}
