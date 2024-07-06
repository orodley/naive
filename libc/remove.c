#include <errno.h>

#include "syscall.h"

int remove(const char *pathname)
{
  // unlink
  int ret = __syscall(87, (uint64_t)pathname, 0, 0, 0, 0, 0);

  if (ret == -EISDIR) {
    // rmdir
    ret = __syscall(84, (uint64_t)pathname, 0, 0, 0, 0, 0);
  }

  if (ret < 0) {
    errno = -ret;
    return -1;
  }

  return 0;
}
