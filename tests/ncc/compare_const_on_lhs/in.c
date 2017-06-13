#include <assert.h>

int main()
{
	int a = 1;
	assert(0 < a);
	assert(1 <= a);
	assert(0 != a);
	assert(1 == a);

	return 0;
}
