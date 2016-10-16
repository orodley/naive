#include <assert.h>

int x = 5634;

int main()
{
	assert(x == 5634);
	assert(++x == 5635);
	return 0;
}
