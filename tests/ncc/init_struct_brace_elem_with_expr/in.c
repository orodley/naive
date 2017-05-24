#include <assert.h>

struct Foo
{
	int a;
	int b;
};

struct Bar
{
	struct Foo foo;
	int c;
};

int main()
{
	struct Foo foo = { 9217, 172 };
	struct Bar bar = {
		.foo = foo,
		.c = 8272,
	};

	assert(bar.foo.a == 9217);
	assert(bar.foo.b == 172);
	assert(bar.c == 8272);

	return 0;
}
