#include <assert.h>

int main()
{
	int x[2];
	*x = 1;
	*(x + 1) = 2;

	assert(*x == 1);
	assert(*(x + 1) == 2);

	return 0;
}
