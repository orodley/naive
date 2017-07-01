#include <assert.h>

struct Foo { int a; };

void foo(struct Foo f)
{
	assert(f.a == 1923);
}

int main()
{
	foo((struct Foo) { 1923 });
	return 0;
}
