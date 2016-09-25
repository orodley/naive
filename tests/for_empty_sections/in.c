#include <assert.h>

int main()
{
	int i = 0;
	for (; i != 10;)
		i = i + 1;
	assert(i == 10);

	// @TODO: Test for empty expression once we've implemented break.

	return 0;
}
