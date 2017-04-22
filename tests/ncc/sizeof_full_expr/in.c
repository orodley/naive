#include <assert.h>

struct Foo
{
	int a;
};

int main()
{
	struct Foo foos[3];
	assert(sizeof foos[1].a == sizeof(int));

	return 0;
}
