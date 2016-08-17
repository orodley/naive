// Misc. utility functions

#include <assert.h>
#include <string.h>
#include <stdbool.h>
#include <stdio.h>

#include "misc.h"

#define ZERO_STRUCT(s) memset(s, 0, sizeof *s);

char *strndup(char *str, u32 length);
char *strdup(char *str);

inline bool streq(char *a, char *b)
{
	return strcmp(a, b) == 0;
}

inline bool strneq(char *a, char *b, u32 length)
{
	return strncmp(a, b, length) == 0;
}


inline long checked_ftell(FILE *file)
{
	long ret = ftell(file);
	assert(ret != -1);

	return ret;
}

inline void checked_fseek(FILE *stream, long offset, int whence)
{
	int ret = fseek(stream, offset, whence);
	assert(ret != -1);
}

inline void checked_fread(void *ptr, size_t size, size_t nmemb, FILE *stream)
{
	size_t entries_read = fread(ptr, size, nmemb, stream);
	assert(entries_read == nmemb);
}

inline void checked_fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream)
{
	size_t entries_written = fwrite(ptr, size, nmemb, stream);
	assert(entries_written == nmemb);
}
