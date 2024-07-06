#include <errno.h>
#include <sys/stat.h>

#include "syscall.h"

int fstat(int fd, struct stat *buf)
{
  int ret = __syscall(5, fd, (uint64_t)buf, 0, 0, 0, 0);
  if (ret != 0) {
    errno = -ret;
    return -1;
  }

  return 0;
}
