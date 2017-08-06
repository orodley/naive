#include <assert.h>

typedef unsigned long u64;

static u64 foo(u64 arg1, u64 arg2)
{
	return arg1 == arg2;
}

int main()
{
	assert(foo(0, 0) == 1);

	return 0;
}
