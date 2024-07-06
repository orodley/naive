#include <assert.h>

struct Foo
{
  struct Bar
  {
    int a;
    int b[3][5];
  } bar;
};

int main()
{
  struct Foo foos[5][5] = {
      [3][2] =
          {
              .bar.a = 3,
              .bar.b[2][4] = 4,
          },
  };

  assert(foos[3][2].bar.b[2][4] == 4);

  return 0;
}
