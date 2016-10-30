#include <assert.h>

int main()
{
	int x = 34;
	int *p = (int *)(void *)(struct Foo *)(char *)&x;
	assert(*p == 34);

	return 0;
}
