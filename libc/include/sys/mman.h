#ifndef _SYS_MMAN
#define _SYS_MMAN

// @TODO: Incorrect include.
#include <sys/types.h>

#define PROT_READ 0x01
#define PROT_WRITE 0x02

#define MAP_PRIVATE 0x02
#define MAP_ANONYMOUS 0x20

#define MREMAP_FIXED 0x02

#define MAP_FAILED ((void *)-1)

void *mmap(void *addr, size_t length, int prot, int flags, int fd, off_t offset);
void *mremap(void *old_address, size_t old_size, size_t new_size, int flags, ...);
int munmap(void *addr, size_t length);

#endif
