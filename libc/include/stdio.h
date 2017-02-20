#ifndef _STDIO_H
#define _STDIO_H

#include <stddef.h>

struct _IO_FILE;
typedef struct _IO_FILE FILE;

extern FILE *stdout;

size_t fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream);

#endif
