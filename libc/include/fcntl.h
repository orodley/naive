#ifndef _FCNTL_H
#define _FCNTL_H

int open(const char *pathname, int flags, ...);

#define O_RDONLY 0000
#define O_WRONLY 0001
#define O_CREAT  0100
#define O_EXCL   0200

#endif
