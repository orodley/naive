#include "assert.h"

int foo()
{
	char x = 3;
	return x;
}

int main()
{
	assert(foo() == 3);
	return 0;
}
