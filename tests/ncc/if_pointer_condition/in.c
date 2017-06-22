#include <assert.h>
#include <stddef.h>

int main()
{
	int foo;
	int *bar = &foo;
	if (bar) { }
	else { assert(0); }

	int *baz = NULL;
	if (baz) { assert(0); }

	return 0;
}
