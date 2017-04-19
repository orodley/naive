#include <assert.h>

int foo[5] = { 1, 2, 3, 4, 5 };
struct { int a, b; } bar = {
	.a = 1,
	.b = 2,
};

int main()
{
	assert(foo[0] == 1);
	assert(foo[1] == 2);
	assert(foo[2] == 3);
	assert(foo[3] == 4);
	assert(foo[4] == 5);

	assert(bar.a == 1);
	assert(bar.b == 2);

	return 0;
}
