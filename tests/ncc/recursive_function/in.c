#include <assert.h>

int foo(int a)
{
	if (a > 3)
		return a;

	return a + foo(a + 1);
}

int main()
{
	assert(foo(0) == 10);

	return 0;
}
