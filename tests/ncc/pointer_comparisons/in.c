#include <assert.h>

int main()
{
	int foo[2];
	int *bar = foo;

	assert(bar < bar + 1);
	assert(bar <= bar + 1);
	assert(bar <= bar);
	assert(bar + 1 > bar);
	assert(bar + 1 >= bar);
	assert(bar >= bar);

	assert(!(bar > bar + 1));
	assert(!(bar >= bar + 1));
	assert(!(bar + 1 < bar));
	assert(!(bar + 1 <= bar));

	return 0;
}
