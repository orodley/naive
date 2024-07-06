#include <unistd.h>

#include "syscall.h"

pid_t getpid(void)
{
  return __syscall(39, 0, 0, 0, 0, 0, 0);
}
