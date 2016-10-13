#include <assert.h>

struct Foo
{
	int a;
	struct Bar
	{
		int b;
		int c;
	} d;
	int e;
};

int main()
{
	assert(sizeof(struct Foo) == 16);
	return 0;
}
