#include <stdlib.h>

// @TODO: Call abort instead of just crashing.
// @TODO: Print filename, line number, and function.
void __assert_fail()
{
	abort();
}
