typedef int innt, *pint;
typedef struct Foo { int a; } TFoo;
typedef struct { int b; } Bar;

int main()
{
	innt x;
	pint y = &x;
	*y = 1;
	struct Foo f;
	f.a = 2;
	TFoo t;
	t.a = 3;
	Bar b;
	b.b = 4;

	return 0;
}
