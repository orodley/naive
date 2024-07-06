#include <assert.h>
#include <string.h>

int main()
{
  {
    int foo[5] = {0, 1, 2, 3, 4};
    memmove(foo + 2, foo, sizeof(int) * 3);

    assert(foo[0] == 0);
    assert(foo[1] == 1);
    assert(foo[2] == 0);
    assert(foo[3] == 1);
    assert(foo[4] == 2);
  }

  {
    int foo[5] = {0, 1, 2, 3, 4};
    memmove(foo, foo + 2, sizeof(int) * 3);

    assert(foo[0] == 2);
    assert(foo[1] == 3);
    assert(foo[2] == 4);
    assert(foo[3] == 3);
    assert(foo[4] == 4);
  }

  {
    int foo[5] = {0, 1, 2, 3, 4};
    memmove(foo, foo, sizeof(int) * 5);

    assert(foo[0] == 0);
    assert(foo[1] == 1);
    assert(foo[2] == 2);
    assert(foo[3] == 3);
    assert(foo[4] == 4);
  }

  {
    int foo[5] = {0, 1, 2, 3, 4};
    memmove(foo + 2, foo, sizeof(int) * 2);

    assert(foo[0] == 0);
    assert(foo[1] == 1);
    assert(foo[2] == 0);
    assert(foo[3] == 1);
    assert(foo[4] == 4);
  }

  return 0;
}
