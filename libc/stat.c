#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>

int stat(const char *pathname, struct stat *buf)
{
  int fd = open(pathname, O_RDONLY);
  if (fd == -1) return -1;

  int fstat_ret = fstat(fd, buf);
  int fstat_errno = errno;

  int close_ret = close(fd);

  if (fstat_ret == -1) {
    errno = fstat_errno;
    return -1;
  }

  if (close_ret == -1) {
    return -1;
  }

  return 0;
}
