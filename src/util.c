#include "util.h"

extern inline u32 max(u32 a, u32 b);

extern inline u32 lowest_set_bit(u64 x);
extern inline u32 highest_set_bit(u64 x);
extern inline u32 bit_count(u32 x);

extern inline u32 align_to(u32 n, u32 align);

extern inline u32 float_to_raw_bits(float f);
extern inline u64 double_to_raw_bits(double f);