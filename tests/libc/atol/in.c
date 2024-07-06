#include <assert.h>
#include <stdlib.h>

int main()
{
  assert(atol("0") == 0);
  assert(atol("1") == 1);
  assert(atol("2") == 2);
  assert(atol("3") == 3);
  assert(atol("4") == 4);
  assert(atol("5") == 5);
  assert(atol("6") == 6);
  assert(atol("7") == 7);
  assert(atol("8") == 8);
  assert(atol("9") == 9);
  assert(atol("10") == 10);
  assert(atol("11159") == 11159);
  assert(atol("421416") == 421416);
  assert(atol("552132") == 552132);
  assert(atol("247092") == 247092);
  assert(atol("811689") == 811689);
  assert(atol("237595") == 237595);
  assert(atol("889530") == 889530);
  assert(atol("466655") == 466655);
  assert(atol("519753") == 519753);
  assert(atol("396245") == 396245);

  return 0;
}
