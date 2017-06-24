#include <assert.h>

int main()
{
	struct
	{
		int a;
		int *b;
		int c;
	} a = { .c = 1};
	assert(sizeof a == 24);
	assert(a.c == 1);

	return 0;
}
