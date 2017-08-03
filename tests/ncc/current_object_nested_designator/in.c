#include <assert.h>

struct A
{
	struct
	{
		int y;
		int z;
	} x;
};

int main()
{
	struct A a = {
		.x.y = 283,
		.x.z = 942,
	};
	assert(a.x.z == 942);

	return 0;
}
