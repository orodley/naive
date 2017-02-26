#ifndef _UNISTD_H
#define _UNISTD_H

// @TODO: Incorrect include.
#include <sys/types.h>

// @TODO: void in arg list
pid_t getpid();

ssize_t write(int fd, const void *buf, size_t count);
int close(int fd);

#endif
