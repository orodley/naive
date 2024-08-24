// Misc. mathematical functions

#ifndef NAIVE_MATHS_H_
#define NAIVE_MATHS_H_

#include "assertions.h"
#include "types.h"

inline u32 max(u32 a, u32 b) { return (a < b) ? b : a; }

inline u32 lowest_set_bit(u64 x)
{
  PRECONDITION(x != 0);

  u32 i = 0;
  while ((x & 1UL) != 1) {
    x >>= 1;
    i++;
  }

  return i;
}

#define HAS_CLZL 0
#ifdef __has_builtin
#if __has_builtin(__builtin_clzl)
#undef HAS_CLZL
#define HAS_CLZL 1
#endif
#endif
inline u32 highest_set_bit(u64 x)
{
  PRECONDITION(x != 0);

#if HAS_CLZL
  return 63 - __builtin_clzl(x);
#else
  // The following code is adapted from the section "Find the log base 2 of
  // an N-bit integer in O(lg(N)) operations" from the famous "Bit Twiddling
  // Hacks" page:
  // https://graphics.stanford.edu/~seander/bithacks.html#IntegerLog
  const uint64_t b[] = {0x2,    0xC,        0xF0,
                        0xFF00, 0xFFFF0000, 0xFFFFFFFF00000000ULL};
  const unsigned s[] = {1, 2, 4, 8, 16, 32};

  uint64_t r = 0;
  for (int i = 5; i >= 0; i--) {
    if ((x & b[i]) != 0) {
      x >>= s[i];
      r |= s[i];
    }
  }

  return r;
#endif
}
#undef HAS_CLZL

inline u32 bit_count(u32 x)
{
  u32 n = 0;
  for (; x != 0; n++) {
    x &= x - 1;  // clear the least significant bit set
  }

  return n;
}

// @NOTE: align must be a power of two
inline u32 align_to(u32 n, u32 align)
{
  u32 x = align - 1;
  return (n + x) & ~x;
}

inline u32 float_to_raw_bits(float f)
{
  union
  {
    float f;
    u32 x;
  } u = {.f = f};
  return u.x;
}

inline u64 double_to_raw_bits(double f)
{
  union
  {
    double f;
    u64 x;
  } u = {.f = f};
  return u.x;
}

#endif
