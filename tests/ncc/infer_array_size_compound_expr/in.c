#include <assert.h>

int main()
{
	int size = sizeof (int[]) {1, 2};
	assert(size == 2 * sizeof(int));

	int a = ((int[]) {1, 2})[1];
	assert(a == 2);

	return 0;
}
