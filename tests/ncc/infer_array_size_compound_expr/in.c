#include <assert.h>

int main()
{
	int size = sizeof (int[]) {1, 2};
	assert(size == 2 * sizeof(int));
	return 0;
}
