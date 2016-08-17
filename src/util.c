#include <stdlib.h>

#include "util.h"

char *strndup(char *str, u32 length)
{
	char *result = malloc(length + 1);
	strncpy(result, str, length);
	result[length] = '\0';

	return result;
}

extern inline bool streq(char *a, char *b);
extern inline bool strneq(char *a, char *b, u32 length);

extern inline long checked_ftell(FILE *file);
extern inline void checked_fseek(FILE *file, long offset, int whence);
extern inline void checked_fread(void *ptr, size_t size, size_t nmemb, FILE *stream);
extern inline void checked_fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream);
