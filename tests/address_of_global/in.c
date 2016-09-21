#include <assert.h>

int foo(int *x)
{
	*x = *x + 3;
	return *x;
}

int a;

int main()
{
	foo(&a);
	assert(a == 3);

	return 0;
}
