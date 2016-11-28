#include <assert.h>
#include <stdarg.h>

void foo(int a, ...)
{
	va_list args;
	va_start(args, a);

	assert(a == 0);
	assert(va_arg(args, int) == 1);
	assert(va_arg(args, int) == 2);
	assert(va_arg(args, int) == 3);
	assert(va_arg(args, int) == 4);

	va_end(args);
}

int main()
{
	foo(0, 1, 2, 3, 4);
	return 0;
}
