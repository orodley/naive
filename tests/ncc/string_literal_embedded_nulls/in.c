#include <assert.h>

int main()
{
	char a[] = "a\0b\0";

	assert(sizeof a == 5);
	assert(a[0] == 'a');
	assert(a[1] == '\0');
	assert(a[2] == 'b');
	assert(a[3] == '\0');
	assert(a[4] == '\0');

	return 0;
}
