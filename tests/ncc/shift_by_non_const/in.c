#include <assert.h>

int main()
{
	unsigned a = 12;
	unsigned b = 2;

	assert(a << b == 48);
	assert(a >> b == 3);

	return 0;
}
