#include <assert.h>

int main()
{
	int x = 0;
	{
		int x = 1;
		{
			int x = 2;
			{
				int x = 3;
				assert(x == 3);
			}
			assert(x == 2);
		}
		assert(x == 1);
	}
	assert(x == 0);

	return 0;
}
