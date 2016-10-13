#include <assert.h>

int a;

void foo(int b)
{
	a = b;
	return;
	a = 2;
}

int main()
{
	foo(23);
	assert(a == 23);
	return 0;
}
