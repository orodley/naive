#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/stat.h>
#include <unistd.h>

int main()
{
	int fd = open("in.c", O_RDONLY);
	assert (fd >= 0);

	struct stat s;
	int ret = fstat(fd, &s);
	assert (ret == 0);

	printf("%d\n", s.st_mode);

	close(fd);
	return 0;
}
