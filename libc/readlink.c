#include <sys/types.h>

#include "syscall.h"

ssize_t readlink(const char *restrict path, char *restrict buf, size_t size)
{
  return __syscall(89, path, buf, size, 0, 0, 0);
}