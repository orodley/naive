#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/stat.h>
#include <unistd.h>

int main()
{
	struct stat s;
	int ret = stat("in.c", &s);
	assert (ret == 0);

	printf("%d\n", s.st_mode);

	return 0;
}
