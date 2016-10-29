#include "syscall.h"

void exit(int status)
{
	__syscall(60, (unsigned)status & 0xFF, 0, 0);
}
