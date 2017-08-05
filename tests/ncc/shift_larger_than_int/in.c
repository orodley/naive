#include <assert.h>

int main()
{
	unsigned long imm = 0x17;
	unsigned imm_width = 32;

	unsigned long truncated = imm & ((1ULL << imm_width) - 1);

	assert(imm == truncated);

	return 0;
}
