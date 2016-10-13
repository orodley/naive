#include <assert.h>

int main()
{
	char *foo = "foobar";
	assert(foo[0] == 102);
	assert(foo[1] == 111);
	assert(foo[2] == 111);
	assert(foo[3] == 98);
	assert(foo[4] == 97);
	assert(foo[5] == 114);
	assert(foo[6] == 0);

	return 0;
}
