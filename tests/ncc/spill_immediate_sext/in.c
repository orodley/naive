#include <assert.h>

int main()
{
	unsigned long long a = 3;
	a <<= 33;
	a |= 6;
	assert((a & 4294967295U) == 6);

	return 0;
}
