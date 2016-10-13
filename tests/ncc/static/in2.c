#include <assert.h>

int *bar();

int foo;

int main()
{
	assert(foo == 0);
	assert(*bar() == 0);

	foo = 1;
	assert(foo == 1);
	assert(*bar() == 0);

	*bar() = 2;
	assert(foo == 1);
	assert(*bar() == 2);

	return 0;
}
