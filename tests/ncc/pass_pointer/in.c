#include <assert.h>

int foo(int *x)
{
	*x = *x + 3;
	return *x;
}

int main()
{
	int a = 1;
	foo(&a);
	assert(a == 4);

	return 0;
}
