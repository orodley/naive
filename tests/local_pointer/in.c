#include <assert.h>

int main()
{
	int x = 3;
	int *p = &x;
	assert(*p == 3);

	return 0;
}
