#include <assert.h>

int g_a;

int main()
{
	int a;
	int *b = &a;

	assert(!!b);
	assert(!!"foo");

	//assert(0 != b);
	assert(0 != "foo");
	assert(b != 0);
	assert("foo" != 0);

	assert(&g_a != 0);
	assert(0 != &g_a);

	return 0;
}
