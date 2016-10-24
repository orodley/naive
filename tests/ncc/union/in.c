#include <assert.h>

union Foo
{
	int a;
	int b;
	char c[5];
};

int main()
{
	union Foo foo;
	assert(sizeof(union Foo) == 8);

	foo.a = 1;
	assert(foo.a == 1);
	assert(foo.b == 1);

	foo.b = 2;
	assert(foo.a == 2);
	assert(foo.b == 2);

	foo.c[0] = 'b';
	foo.c[1] = 'a';
	foo.c[2] = 'r';
	foo.c[3] = '\0';

	// @PORT: This is dependent on endianness.
	assert(foo.a == 7496034);
	assert(foo.b == 7496034);
	assert(foo.c[0] == 'b');
	assert(foo.c[1] == 'a');
	assert(foo.c[2] == 'r');
	assert(foo.c[3] == '\0');

	return 0;
}
