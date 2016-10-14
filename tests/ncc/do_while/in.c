#include <assert.h>

int main()
{
	int a = 0;
	do {
		a++;
	} while (a < 2);
	assert(a == 2);

	int b = 0;
	do {
		b++;
	} while (0);
	assert(b == 1);

	return 0;
}
