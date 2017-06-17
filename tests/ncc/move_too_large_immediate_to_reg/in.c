#include <assert.h>

int main()
{
	unsigned long long x = 1;
	x += 1099511627776;
	assert(x == 1099511627777);

	return 0;
}
