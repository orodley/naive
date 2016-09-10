#include <assert.h>

int main()
{
	int x = 0, y = 0;
	while (x != 5) {
		y = y + x;
		x = x + 1;
	}

	assert(y == 10);
}
