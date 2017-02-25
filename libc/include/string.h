#ifndef _STRING_H
#define _STRING_H

// @TODO: Are we allowed to include this here?
#include <stddef.h>

void *memcpy(void *dest, const void *src, size_t n);
void *memset(void *s, int c, size_t n);
size_t strlen(const char *s);

#endif
