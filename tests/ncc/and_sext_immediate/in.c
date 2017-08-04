#include <assert.h>

int main()
{
	unsigned long x = 'f';
	unsigned char byte = x & 0xFF;
	assert(byte == 'f');

	return 0;
}
