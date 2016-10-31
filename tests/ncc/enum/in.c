#include <assert.h>

enum Foo { FOO1, FOO2, FOO3 };

int main()
{
	assert(FOO1 == 0);
	assert(FOO2 == 1);
	assert(FOO3 == 2);

	// @TODO: Re-enable the below when we have proper scoping for types.
	/*
	{
		enum Foo { FOO0, FOO1, FOO2, FOO3 };
		assert(FOO0 == 0);
		assert(FOO1 == 1);
		assert(FOO2 == 2);
		assert(FOO3 == 3);
	}
	*/

	assert(FOO1 == 0);
	assert(FOO2 == 1);
	assert(FOO3 == 2);

	assert(sizeof(enum Foo) == 4);

	return 0;
}
