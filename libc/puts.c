#include <stdio.h>

int puts(const char *s)
{
	if (fputs(s, stdout) < 1)
		return EOF;
	return putchar('\n');
}
