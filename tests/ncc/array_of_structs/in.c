#include <assert.h>

int main()
{
	struct { int a, b; } foo[2] = { { 1, 2 }, { 3, 4 } };

	assert(foo[0].a == 1);
	assert(foo[0].b == 2);
	assert(foo[1].a == 3);
	assert(foo[1].b == 4);

	return 0;
}
