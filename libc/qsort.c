#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

// @NOTE: We do this so we only have to allocate constant space for doing
// swaps, rather than allocating at every recursive call.
static void qsort_aux(void *base, size_t nmemb, size_t size,
		int (*compar)(const void *, const void *), void *swap_space);

void qsort(void *base, size_t nmemb, size_t size,
		int (*compar)(const void *, const void *))
{
	void *swap_space = malloc(size);
	qsort_aux(base, nmemb, size, compar, swap_space);
	free(swap_space);
}

static void swap_elems(void *a, void *b, size_t size, void *swap_space)
{
	if (a != b) {
		memcpy(swap_space, a, size);
		memcpy(a, b, size);
		memcpy(b, swap_space, size);
	}
}

static void qsort_aux(void *base, size_t nmemb, size_t size,
		int (*compar)(const void *, const void *), void *swap_space)
{
#define NTH_ELEM(n) ((void *)((uint8_t *)base + ((n) * size)))

	if (nmemb <= 1) {
		return;
	}

	void *pivot = NTH_ELEM(nmemb - 1);
	size_t lower_end = 0;
	size_t higher_end = 0;

	while (higher_end != nmemb - 1) {
		void *curr_elem = NTH_ELEM(higher_end);
		if (compar(curr_elem, pivot) < 0) {
			swap_elems(curr_elem, NTH_ELEM(lower_end), size, swap_space);
			lower_end++;
		}

		higher_end++;
	}

	swap_elems(pivot, NTH_ELEM(lower_end), size, swap_space);

	size_t num_lower = lower_end;
	size_t num_higher = nmemb - num_lower - 1;

	void *lower_start = base;
	void *higher_start = NTH_ELEM(lower_end + 1);

	qsort_aux(lower_start, num_lower, size, compar, swap_space);
	qsort_aux(higher_start, num_higher, size, compar, swap_space);

#undef NTH_ELEM
}
