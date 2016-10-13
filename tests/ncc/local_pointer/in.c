#include <assert.h>

int main()
{
	int x = 3;
	int *p = &x;
	assert(*p == 3);

	*p = 2;
	assert(x == 2);

	return 0;
}
