#include <assert.h>
#include <stdio.h>

// @NOTE: This is a hack to force the compiler not to generate a .rodata
// section which our linker can't handle right now. If it were an inline string
// literal, it would be put in .rodata
static char fmt[] = "%s: something went wrong!";

void perror(const char *s)
{
	// @TODO: Implement this properly.
	int ret = printf(fmt, s);
	assert(ret > 0);
}
