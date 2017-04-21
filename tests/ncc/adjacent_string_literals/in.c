#include <assert.h>

#define OOB "oob"

int main()
{
	char *x = "f" /* foo */ OOB
		// foo
		"ar";

	assert(x[0] == 'f');
	assert(x[1] == 'o');
	assert(x[2] == 'o');
	assert(x[3] == 'b');
	assert(x[4] == 'a');
	assert(x[5] == 'r');

	return 0;
}
