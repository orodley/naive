#include <assert.h>

enum Foo { A, B };

enum Foo a = A;
enum Foo b = B;

int main()
{
  assert(a == A);
  assert(b == B);

  switch (a) {
  case A: break;
  case B: assert(0);
  }

  switch (b) {
  case A: assert(0);
  case B: break;
  }

  return 0;
}
