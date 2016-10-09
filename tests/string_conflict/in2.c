#include <assert.h>

char foo();

int main()
{
	char *x = "!";
	assert(x[0] == 33);
	assert(foo() == 32);
	return 0;
}
