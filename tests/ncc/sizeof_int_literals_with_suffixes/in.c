#include <assert.h>

int main()
{
	assert(sizeof(0) == sizeof(int));
	assert(sizeof(0U) == sizeof(unsigned));
	assert(sizeof(0L) == sizeof(long));
	assert(sizeof(0UL) == sizeof(unsigned long));
	assert(sizeof(0LL) == sizeof(long long));
	assert(sizeof(0ULL) == sizeof(unsigned long long));
	assert(sizeof(4294967296) == sizeof(long));

	return 0;
}
