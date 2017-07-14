#include <stdlib.h>

#include "bit_set.h"

void bit_set_init(BitSet *bit_set, u32 size_in_bits)
{
	bit_set->size_in_bits = size_in_bits;
	if (size_in_bits <= 64) {
		bit_set->bits = &bit_set->inline_bits;
	} else {
		bit_set->bits = malloc(8 * SIZE_IN_U64S(bit_set));
	}
}

void bit_set_free(BitSet *bit_set)
{
	if (bit_set->bits != &bit_set->inline_bits)
		free(bit_set->bits);
}

void bit_set_set_all(BitSet *bit_set)
{
	for (u32 i = 0; i < SIZE_IN_U64S(bit_set); i++) {
		bit_set->bits[i] = 0xFFFFFFFFFFFFFFFFULL;

		// We have to make sure we don't set the top bits so that
		// bit_set_highest_set_bit, bit_set_is_empty, etc. work correctly.
		if (i == SIZE_IN_U64S(bit_set) - 1) {
			u32 top_bits = bit_set->size_in_bits % 64;

			if (top_bits != 0) {
				bit_set->bits[i] = (1UL << top_bits) - 1;
			}
		}
	}
}

void bit_set_clear_all(BitSet *bit_set)
{
	for (u32 i = 0; i < SIZE_IN_U64S(bit_set); i++) {
		bit_set->bits[i] = 0;
	}
}

extern inline bool bit_set_get_bit(BitSet *bit_set, u32 index);
extern inline void bit_set_set_bit(BitSet *bit_set, u32 index, bool value);
extern inline void bit_set_clear_bit(BitSet *bit_set, u32 index);
extern inline i32 bit_set_lowest_set_bit(BitSet *bit_set);
extern inline i32 bit_set_highest_set_bit(BitSet *bit_set);
extern inline bool bit_set_is_empty(BitSet *bit_set);
