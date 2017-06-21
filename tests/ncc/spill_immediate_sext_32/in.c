#include <assert.h>

int main()
{
	int a = 3;
	a &= 0xFFFFFFFF;
	assert(a == 3);

	return 0;
}
