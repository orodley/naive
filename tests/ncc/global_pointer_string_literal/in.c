#include <assert.h>

char *foo = "foo";

int main()
{
	assert(foo[0] == 'f');
	assert(foo[1] == 'o');
	assert(foo[2] == 'o');
	assert(foo[3] == '\0');

	return 0;
}
