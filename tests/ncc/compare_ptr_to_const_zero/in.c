#include <assert.h>

int main()
{
	int a;
	int *b = &a;

	assert(!!b);
	//assert(!!"foo");

	//assert(0 != b);
	//assert(0 != "foo");
	assert(b != 0);
	//assert("foo" != 0);

	return 0;
}
