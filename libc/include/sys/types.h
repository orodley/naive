#ifndef _SYS_TYPES
#define _SYS_TYPES

#include <stddef.h>

typedef int pid_t;
typedef unsigned int mode_t;
typedef unsigned int uid_t;
typedef unsigned int gid_t;
typedef unsigned long dev_t;
typedef unsigned long ino_t;
typedef unsigned long nlink_t;
typedef long blksize_t;
typedef long blkcnt_t;

typedef long ssize_t;
typedef long off_t;  // @TODO: Is this where this is meant to be defined?

#endif
