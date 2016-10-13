#include <assert.h>

extern int a;

int b;

int main()
{
	assert(a == 0);
	assert(b == 0);

	a = 1;
	assert(a == 1);
	assert(b == 0);

	b = 2;
	assert(a == 1);
	assert(b == 2);

	return 0;
}
