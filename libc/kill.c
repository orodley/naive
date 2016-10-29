#include <signal.h>

#include "syscall.h"

int kill(pid_t pid, int sig)
{
	return __syscall(62, (uint64_t)pid, (uint64_t)sig, 0);
}
