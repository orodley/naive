#include <assert.h>
#include <stddef.h>

int *foo;

int main()
{
	struct { int a; int *b; } bar = { 1 };

	assert(foo == NULL);
	assert(bar.b == NULL);

	return 0;
}
