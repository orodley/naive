#ifndef NAIVE_BIT_SET_H_
#define NAIVE_BIT_SET_H_

#include <assert.h>
#include "misc.h"
#include "util.h"

typedef struct BitSet
{
	u64 *bits;
	u64 inline_bits;
	u32 size_in_bits;
} BitSet;

#define SIZE_IN_U64S(bit_set) (((bit_set)->size_in_bits + 63) / 64)

void bit_set_init(BitSet *bit_set, u32 size_in_bits);
void bit_set_free(BitSet *bit_set);
void bit_set_copy(BitSet *copy, BitSet *original);
void bit_set_set_all(BitSet *bit_set);
void bit_set_clear_all(BitSet *bit_set);

inline bool bit_set_get_bit(BitSet *bit_set, u32 index)
{
	return (bit_set->bits[index / 64] >> (index % 64)) & 1;
}

inline void bit_set_set_bit(BitSet *bit_set, u32 index, bool value)
{
	if (value) {
		bit_set->bits[index / 64] |= (1UL << (index % 64));
	} else {
		bit_set->bits[index / 64] &= ~(1UL << (index % 64));
	}
}

inline u32 bit_set_bit_count(BitSet *bit_set)
{
	u32 total = 0;
	for (u32 i = 0; i < SIZE_IN_U64S(bit_set); i++) {
		total += bit_count(bit_set->bits[i]);
	}

	return total;
}

inline i32 bit_set_lowest_set_bit(BitSet *bit_set)
{
	for (u32 i = 0; i < SIZE_IN_U64S(bit_set); i++) {
		if (bit_set->bits[i] != 0) {
			return 64 * i + lowest_set_bit(bit_set->bits[i]);
		}
	}

	return -1;
}

inline i32 bit_set_highest_set_bit(BitSet *bit_set)
{
	for (i32 i = SIZE_IN_U64S(bit_set) - 1; i >= 0; i--) {
		if (bit_set->bits[i] != 0) {
			return 64 * i + highest_set_bit(bit_set->bits[i]);
		}
	}

	return -1;
}

inline bool bit_set_is_empty(BitSet *bit_set)
{
	for (u32 i = 0; i < SIZE_IN_U64S(bit_set); i++) {
		if (bit_set->bits[i] != 0) {
			return false;
		}
	}

	return true;
}

#endif
