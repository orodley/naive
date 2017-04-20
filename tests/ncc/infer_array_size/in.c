#include <assert.h>

int foo[] = { 1, 2, 3, 4 };

int main()
{
	int bar[] = { 4, 3, 2 };
	char baz[] = "quux";

	assert(sizeof foo / sizeof *foo == 4);
	assert(sizeof bar / sizeof *bar == 3);
	assert(sizeof baz / sizeof *baz == 4);

	assert(foo[0] == 1);
	assert(foo[1] == 2);
	assert(foo[2] == 3);
	assert(foo[3] == 4);

	assert(bar[0] == 4);
	assert(bar[1] == 3);
	assert(bar[2] == 2);

	assert(baz[0] == 'q');
	assert(baz[1] == 'u');
	assert(baz[2] == 'u');
	assert(baz[3] == 'x');

	return 0;
}
