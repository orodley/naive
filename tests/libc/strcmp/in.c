#include <assert.h>
#include <string.h>

int main()
{
	assert(strcmp("foo", "foob") < 0);
	assert(strcmp("foob", "foo") > 0);
	assert(strcmp("fop", "foo") > 0);
	assert(strcmp("foo", "fop") < 0);
	assert(strcmp("foo", "foo") == 0);

	return 0;
}
