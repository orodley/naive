#include <assert.h>

extern int foo[];

int main()
{
	assert(foo[0] == 1);
	assert(foo[1] == 2);
	assert(foo[2] == 3);

	return 0;
}
