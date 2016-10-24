#include <assert.h>

struct
{
	int a;
	int b;
} foo;

int main()
{
	assert(foo.a == 0);
	assert(foo.b == 0);

	return 0;
}
