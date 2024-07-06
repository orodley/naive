#include <errno.h>
#include <fcntl.h>
#include <stdarg.h>
#include <stdint.h>

#include "syscall.h"

int open(const char *pathname, int flags, ...)
{
  va_list varargs;
  va_start(varargs, flags);

  int mode = 0;
  if ((flags & O_CREAT) != 0)
    mode = va_arg(varargs, int);

  va_end(varargs);

  int ret = __syscall(2, (uint64_t)pathname, flags, mode, 0, 0, 0);
  if (ret < 0) {
    errno = -ret;
    return -1;
  }

  return ret;
}
