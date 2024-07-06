#include <assert.h>

struct Foo
{
  long a;
  long b;
};

struct Foo bar(int c)
{
  long a = 0;
  long b = 0;
  for (int i = 0;; i++) {
    a += i * 3;
    b += i * 4;

    if (i >= c) return (struct Foo){a, b};
  }
}

int main()
{
  struct Foo f = bar(4);
  assert(f.a == 30);
  assert(f.b == 40);

  return 0;
}
