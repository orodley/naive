#ifndef _STRING_H
#define _STRING_H

// @TODO: Are we allowed to include this here?
#include <stddef.h>

void *memcpy(void *dest, const void *src, size_t n);
void *memmove(void *dest, const void *src, size_t n);
void *memset(void *s, int c, size_t n);
size_t strlen(const char *s);
int strcmp(const char *s1, const char *s2);
int strncmp(const char *s1, const char *s2, size_t n);
char *strncpy(char *dest, const char *src, size_t count);

// @TODO: These should be hidden behind feature test macros.
char *strndup(const char *s, size_t n);
char *strdup(const char *s);

#endif
