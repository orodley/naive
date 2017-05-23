#include <assert.h>

int main()
{
	unsigned x = 3;
	signed y = 4;

	assert(0 ? x : y == 4);
	assert(1 ? x : y == 3);

	return 0;
}
