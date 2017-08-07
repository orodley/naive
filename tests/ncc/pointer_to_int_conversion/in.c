#include <assert.h>

int main()
{
	int foo[2];
	int *bar = foo;
	unsigned long a = (unsigned long)bar;
	unsigned long b = (unsigned long)(bar + 1);

	assert(b - a == sizeof(int));

	return 0;
}
