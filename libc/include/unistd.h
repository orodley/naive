#ifndef _UNISTD_H
#define _UNISTD_H

// @TODO: Incorrect include.
#include <sys/types.h>

pid_t getpid(void);

ssize_t read(int fd, void *buf, size_t count);
ssize_t write(int fd, const void *buf, size_t count);
int close(int fd);

#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2

off_t lseek(int fd, off_t offset, int whence);

ssize_t readlink(const char *restrict path, char *restrict buf, size_t size);

#endif
