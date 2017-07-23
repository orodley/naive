#include <stdlib.h>

#include "util.h"

extern inline u32 max(u32 a, u32 b);

extern inline bool is_valid(String str);

extern inline bool streq(char *a, char *b);
extern inline bool strneq(char *a, char *b, u32 length);

extern inline u32 lowest_set_bit(u64 x);
extern inline u32 highest_set_bit(u64 x);
extern inline u32 bit_count(u32 x);

extern inline u32 align_to(u32 n, u32 align);
