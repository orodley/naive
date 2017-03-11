#ifndef _STDLIB_H
#define _STDLIB_H

#include <stddef.h>

void *malloc(size_t size);
void free(void *ptr);
void *calloc(size_t nmemb, size_t size);
void *realloc(void *ptr, size_t size);

void exit(int status);
// @TODO: void in arg list
void abort();

long atol(const char *nptr);

// @TODO: void in arg list
int rand();
void srand(unsigned int seed);

void qsort(void *base, size_t nmemb, size_t size,
		int (*compar)(const void *, const void *));

#endif
