#include <assert.h>
#include <stdio.h>

void perror(const char *s)
{
	// @TODO: Implement this properly.
	int ret = printf("%s: something went wrong!", s);
	assert(ret > 0);
}
