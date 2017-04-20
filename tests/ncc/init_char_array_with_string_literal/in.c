#include <assert.h>

char foo[3] = "foo";

int main()
{
	char bar[4] = "buzz";

	assert(foo[0] == 'f');
	assert(foo[1] == 'o');
	assert(foo[2] == 'o');

	assert(bar[0] == 'b');
	assert(bar[1] == 'u');
	assert(bar[2] == 'z');
	assert(bar[3] == 'z');

	return 0;
}
