#include <assert.h>
#include <stddef.h>

int main()
{
	unsigned char a[] = {1, 2, 3};
	a[1] = 4;
	assert(a[0] == 1);
	assert(a[1] == 4);
	assert(a[2] == 3);

	unsigned short b[] = {1, 2, 3};
	b[1] = 4;
	assert(b[0] == 1);
	assert(b[1] == 4);
	assert(b[2] == 3);

	unsigned int c[] = {1, 2, 3};
	c[1] = 4;
	assert(c[0] == 1);
	assert(c[1] == 4);
	assert(c[2] == 3);

	unsigned long d[] = {1, 2, 3};
	d[1] = 4;
	assert(d[0] == 1);
	assert(d[1] == 4);
	assert(d[2] == 3);

	long x = 1;
	x <<= 40;

	x = 0;
	assert(x == 0);

	int *foo = (int *)x;
	foo = NULL;
	assert(foo == NULL);

	return 0;
}
