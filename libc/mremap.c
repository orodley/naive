#include <errno.h>
#include <stdarg.h>
#include <stddef.h>
#define _GNU_SOURCE
#include <sys/mman.h>

#include "err.h"
#include "syscall.h"

void *mremap(
    void *old_address, size_t old_size, size_t new_size, int flags, ...)
{
  va_list varargs;
  va_start(varargs, flags);

  void *new_address = NULL;
  if (flags & MREMAP_FIXED) new_address = va_arg(varargs, void *);

  uint64_t ret = __syscall(
      25, (uint64_t)old_address, old_size, new_size, flags,
      (uint64_t)new_address, 0);
  if (PTR_IS_ERR(ret)) {
    errno = -ret;
    return MAP_FAILED;
  }

  va_end(varargs);
  return (void *)ret;
}
