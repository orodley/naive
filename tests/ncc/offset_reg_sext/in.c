#include <assert.h>

void bar()
{
	// Allocate a bunch of locals - if foo is writing stuff under its stack
	// frame, we should overwrite it here.
	unsigned char foo[1024];
	for (unsigned i = 0; i < sizeof foo; i++) {
		foo[i] = 0;
	}
}

int main()
{
	// Check a bunch of offsets, including numbers with the 8th bit set and
	// some greater than 256.
	unsigned char foo[512];
	for (unsigned i = 0; i < sizeof foo; i++) {
		foo[i] = i & 0xFF;
	}
	bar();
	for (unsigned i = 0; i < sizeof foo; i++) {
		assert(foo[i] == (i & 0xFF));
	}

	return 0;
}
