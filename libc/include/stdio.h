#ifndef _STDIO_H
#define _STDIO_H

#include <stddef.h>

#define EOF -1

struct _IO_FILE;
typedef struct _IO_FILE FILE;

extern FILE *stdout;

size_t fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream);
FILE *fopen(const char *path, const char *mode);
int fclose(FILE *fp);

int fputc(int c, FILE *stream);
int fputs(const char *s, FILE *stream);
int putc(int c, FILE *stream);
int putchar(int c);
int puts(const char *s);

#endif
