#include <stdio.h>

int main()
{
	char foo[10];
	fread(foo, 1, 9, stdin);
	foo[9] = '\0';

	puts(foo);

	return 0;
}
