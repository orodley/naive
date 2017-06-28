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

	struct timespec st_atim;
	struct timespec st_mtim;
	struct timespec st_ctim;

	long __reserved[3];
};

#define st_atime st_atim.tv_sec
#define st_mtime st_mtim.tv_sec
#define st_ctime st_ctim.tv_sec

int fstat(int fd, struct stat *buf);
int stat(const char *pathname, struct stat *buf);

int fchmod(int fd, mode_t mode);

#define S_IXUSR 0100

#endif
