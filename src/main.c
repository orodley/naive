#include <stdio.h>
#include "misc.h"

// @PORT
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/mman.h> 

// @PORT
char *map_file_into_memory(const char *filename)
{
	int fd = open(filename, O_RDONLY);
	if (fd == -1)
		return NULL;

	off_t file_size = lseek(fd, 0, SEEK_END);

	if (file_size == -1)
		return NULL;

	char *buffer = mmap(NULL, file_size, PROT_READ, MAP_PRIVATE, fd, 0);
	if (buffer == MAP_FAILED)
		return NULL;

	return buffer;
}

int main(int argc, char *argv[])
{
	if (argc != 2) {
		fprintf(stderr, "Usage: %s <input file>\n", argv[0]);
		return 1;
	}

	char *input_filename = argv[1];
	char *buffer = map_file_into_memory(input_filename);

	if (buffer == NULL)
		return 1;

	printf("char 1 = '%c'\n", buffer[0]);

	return 0;
}
