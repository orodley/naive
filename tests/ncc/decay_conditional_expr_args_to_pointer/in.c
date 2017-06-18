#include <assert.h>

int main()
{
	assert((1 ? "foo" : "bar")[0] == 'f');
	assert((0 ? "foo" : "bar")[0] == 'b');

	return 0;
}
