#include <assert.h>

int foo[6];

int main()
{
	*foo = 3;
	assert(*foo == 3);

	return 0;
}
