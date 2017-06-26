#ifndef _STAT_H
#define _STAT_H

#include <sys/types.h>
#include <time.h>

struct stat
{
	dev_t st_dev;
	ino_t st_ino;
	nlink_t st_nlink;
	mode_t st_mode;
	uid_t st_uid;
	gid_t st_gid;
	int __pad0;
	dev_t st_rdev;
	off_t st_size;
	blksize_t st_blksize;
	blkcnt_t st_blocks;

	struct timespec st_atime;
	struct timespec st_mtime;
	struct timespec st_ctime;

	long __reserved[3];
};

int fstat(int fd, struct stat *buf);
int stat(const char *pathname, struct stat *buf);

int fchmod(int fd, mode_t mode);

#define S_IXUSR 0100

#endif
