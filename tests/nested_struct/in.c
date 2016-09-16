#include <assert.h>

struct Foo
{
	int a, b;
};

struct Bar
{
	int a;
	struct Foo foo;
	int b;
};

int main()
{
	struct Bar bar;
	bar.a = 1;
	bar.foo.a = 2;
	bar.foo.b = 3;
	bar.b = 4;
	assert(bar.a == 1);
	assert(bar.foo.a == 2);
	assert(bar.foo.b == 3);
	assert(bar.b == 4);

	int *p = &bar.foo.b;
	*p = 10;
	assert(bar.foo.b == 10);

	return 0;
}
