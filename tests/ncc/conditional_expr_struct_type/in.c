#include <assert.h>

struct Foo { int a; int b; };

int main()
{
	struct Foo f1 = { 1, 2 };
	struct Foo f2 = { 3, 4 };

	int z = 0;
	int o = 1;
	struct Foo f3 = z ? f1 : f2;
	struct Foo f4 = o ? f1 : f2;

	assert(f3.a == 3);
	assert(f3.b == 4);
	assert(f4.a == 1);
	assert(f4.b == 2);

	return 0;
}
