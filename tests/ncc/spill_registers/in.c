#include <assert.h>

void foo(int a, int b, int c, int d, int e, int f, int g)
{
	assert(a == 1);
	assert(b == 1);
	assert(c == 1);
	assert(d == 1);
	assert(e == 1);
	assert(f == 1);
	assert(g == 1);
}

int main()
{
	int a = 1;

	switch (a) {
	case 0: assert(0);
	case 1: foo(a, a, a, a, a, a, a); break;
	case 2: assert(0);
	}

	return 0;
}
