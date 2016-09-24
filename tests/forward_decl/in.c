#include <assert.h>

int foo(int a);

int bar(int a)
{
	return 2 * foo(a);
}

int foo(int a)
{
	return a + 5;
}

int main()
{
	assert(bar(5) == 20);
	return 0;
}
