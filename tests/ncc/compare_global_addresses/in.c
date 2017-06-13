#include <assert.h>

int a;
int b;

int main()
{
	assert(&a != &b);
	return 0;
}
