#include <assert.h>

int main()
{
  // @PORT: This test is System V x86-64 specific.

  assert(sizeof(char) == 1);
  assert(sizeof(signed char) == 1);
  assert(sizeof(unsigned char) == 1);

  assert(sizeof(short) == 2);
  assert(sizeof(signed short) == 2);
  assert(sizeof(short int) == 2);
  assert(sizeof(signed short int) == 2);
  assert(sizeof(unsigned short) == 2);
  assert(sizeof(unsigned short int) == 2);

  assert(sizeof(int) == 4);
  assert(sizeof(signed) == 4);
  assert(sizeof(signed int) == 4);
  assert(sizeof(unsigned) == 4);
  assert(sizeof(unsigned int) == 4);

  assert(sizeof(long) == 8);
  assert(sizeof(signed long) == 8);
  assert(sizeof(long int) == 8);
  assert(sizeof(signed long int) == 8);
  assert(sizeof(unsigned long) == 8);
  assert(sizeof(unsigned long int) == 8);

  assert(sizeof(long long) == 8);
  assert(sizeof(signed long long) == 8);
  assert(sizeof(long long int) == 8);
  assert(sizeof(signed long long int) == 8);
  assert(sizeof(unsigned long long) == 8);
  assert(sizeof(unsigned long long int) == 8);

  return 0;
}
