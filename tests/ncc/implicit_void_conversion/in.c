#include <assert.h>

int main()
{
	int x = 54;
	void *p = &x;
	int *p2 = p;
	assert(*p2 == 54);

	return 0;
}
