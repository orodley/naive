#include <errno.h>
#include <stddef.h>

#include "syscall.h"

int munmap(void *addr, size_t length)
{
  int ret = __syscall(11, (uint64_t)addr, length, 0, 0, 0, 0);
  if (ret < 0) {
    errno = -ret;
    return -1;
  }

  return 0;
}
