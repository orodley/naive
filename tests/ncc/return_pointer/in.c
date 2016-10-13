#include <assert.h>

int foo;
int *bar()
{
	return &foo;
}

int main()
{
	*bar() = 3;
	assert(foo == 3);

	return 0;
}
