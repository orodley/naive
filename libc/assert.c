#include <stdio.h>
#include <stdlib.h>

// @NOTE: .rodata hack
static char fmt[] = "Assert failure at %s:%d\n";

// @TODO: Print stringified expr and function name.
void __assert_fail(const char *filename, int line)
{
	fprintf(stderr, fmt, filename, line);
	abort();
}
