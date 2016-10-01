#include <assert.h>

int main()
{
	int a = 1, b = 2;

	assert(b > a);
	assert(a < b);
	assert(b >= a);
	assert(b >= b);
	assert(a <= b);
	assert(b <= b);

	return 0;
}
