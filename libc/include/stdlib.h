#ifndef _STDLIB_H
#define _STDLIB_H

#include <stddef.h>

void *malloc(size_t size);
void free(void *ptr);

void exit(int status);
// @TODO: void in arg list
void abort();

#endif
