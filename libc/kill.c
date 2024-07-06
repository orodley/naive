#include <errno.h>
#include <signal.h>

#include "syscall.h"

int kill(pid_t pid, int sig)
{
  int ret = __syscall(62, (uint64_t)pid, (uint64_t)sig, 0, 0, 0, 0);
  if (ret < 0) {
    errno = -ret;
    return -1;
  }

  return 0;
}
