#ifndef _SYSCALL_H
#define _SYSCALL_H

#include <stdint.h>

uint64_t __syscall(
		uint32_t syscall_num, uint64_t arg0, uint64_t arg1, uint64_t arg2);

#endif
