#include <assert.h>

typedef struct
{
  int t;
  struct
  {
    char a;
    int b;
    struct
    {
      int c1;
    } c;
    struct
    {
      int d1;
    } d;
  } u;
} Foo;

int main()
{
  Foo a[] = {(Foo){}, (Foo){.u.a = 2}};
  assert(a[0].t == 0);
  assert(a[1].u.a == 2);
  return 0;
}
