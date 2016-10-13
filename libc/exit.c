#include "syscall.h"

void exit(int status)
{
	__syscall(1, (unsigned)status & 0xFF, 0, 0);
}
