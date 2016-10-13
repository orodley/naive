#include <assert.h>

struct Foo
{
	int a;
	int b;
};

int main()
{
	struct Foo f;
	int *pb = &f.b;
	*pb = 836;
	assert(f.b == 836);

	return 0;
}
