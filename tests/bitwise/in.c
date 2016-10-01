#include <assert.h>

int main()
{
	unsigned int x = 0xDEADBEEF;
	unsigned int y = 0xDABBAD00;

	assert((x | y) == 0xDEBFBFEF);
	assert((x & y) == 0xDAA9AC00);
	assert((x ^ y) == 0x041613EF);
	assert(~x == 0x21524110);

	return 0;
}
