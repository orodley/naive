#include <assert.h>

int main()
{
	int x = -1, y = 3;
	assert(x < y);
	assert(x <= y);
	assert(-1 < 3);
	assert(-1 <= 3);

	unsigned w = 0xFFFFFFFF, z = 3;
	assert(w > z);
	assert(w >= z);
	assert(0xFFFFFFFFU > 3);
	assert(0xFFFFFFFFU >= 3);

	return 0;
}
