#include <assert.h>

int main()
{
	int x = 0;
	assert(x++ == 0);
	assert(x == 1);
	assert(++x == 2);
	assert(x == 2);

	return 0;
}
