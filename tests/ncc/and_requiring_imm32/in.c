#include <assert.h>

int main()
{
	int a = 123;
	a <<= 20;
	a |= 93;
	a &= 0xFF;
	assert(a == 93);

	return 0;
}
