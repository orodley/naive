#include <assert.h>

struct Foo
{
	int a, b, c;
};

int main()
{
	struct Foo f;
	f.a = 1;
	f.b = 2;
	f.c = 3;

	assert(f.a == 1);
	assert(f.b == 2);
	assert(f.c == 3);

	return 0;
}
