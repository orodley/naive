#include <assert.h>

void foo(int a, int b, int c, int d, int e, int f, int g, int h)
{
	assert(a == 0);
	assert(b == 1);
	assert(c == 2);
	assert(d == 3);
	assert(e == 4);
	assert(f == 5);
	assert(g == 6);
	assert(h == 7);

	// @TODO: This shouldn't be necessary, but there's a bug at the moment with
	// functions without an explicit return. Remove once this is fixed.
	return;
}

int main()
{
	foo(0, 1, 2, 3, 4, 5, 6, 7);

	return 0;
}
