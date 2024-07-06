#include <assert.h>

int main()
{
  struct
  {
    struct { int *a; int b; } a[2];
  } b = { .a[1].b = 1 };
  assert(sizeof b.a == 32);
  assert(sizeof b.a[0] == 16);
  assert(b.a[1].b == 1);

  return 0;
}
