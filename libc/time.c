#include <errno.h>
#include <time.h>

#include "syscall.h"

time_t time(time_t *t_ptr)
{
  time_t t = __syscall(201, (uint64_t)t_ptr, 0 ,0 ,0, 0, 0);
  if (t < 0) {
    errno = -t;
    return -1;
  }

  return t;
}
