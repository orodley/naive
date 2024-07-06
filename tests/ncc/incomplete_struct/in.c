#include <assert.h>

struct Foo
{
  struct Foo *foo;
  int a;
};

struct Bar;

void set_a(struct Bar **bar, int a);
int get_a(struct Bar *bar);

int main()
{
  struct Foo foo;
  foo.a = 3;
  foo.foo = &foo;

  assert(foo.foo->foo->foo->a == 3);

  struct Bar *bar;
  set_a(&bar, 25);
  assert(get_a(bar) == 25);

  return 0;
}

struct Bar
{
  int a;
};

struct Bar global_bar;

void set_a(struct Bar **bar, int a)
{
  *bar = &global_bar;
  (*bar)->a = a;
}

int get_a(struct Bar *bar) { return bar->a; }
