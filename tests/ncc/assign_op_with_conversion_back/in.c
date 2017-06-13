#include <assert.h>

int main()
{
	unsigned char a = 1;
	a |= 2;

	assert(a == 3);

	return 0;
}
