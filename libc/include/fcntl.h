#ifndef _FCNTL_H
#define _FCNTL_H

int open(const char *pathname, int flags, ...);

#define O_RDONLY 00000
#define O_WRONLY 00001
#define O_CREAT 00100
#define O_EXCL 00200
#define O_TRUNC 01000

#endif
