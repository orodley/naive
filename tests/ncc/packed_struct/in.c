#include <assert.h>

struct Foo
{
	int a;
	char b;
	int c;
	char d;
} __attribute__((packed));

int main()
{
	struct Foo foo;
	assert((char *)&foo.b - (char *)&foo.a == 4);
	assert((char *)&foo.c - (char *)&foo.b == 1);
	assert((char *)&foo.d - (char *)&foo.c == 4);
	
	assert(sizeof(struct Foo) == 10);
	assert(sizeof(struct Foo[2]) == 20);

	return 0;
}
