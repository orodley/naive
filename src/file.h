#ifndef NAIVE_FILE_H_
#define NAIVE_FILE_H_

#include <assert.h>
#include <stdio.h>

#include "misc.h"

typedef enum FileType
{
	ELF_FILE_TYPE,
	AR_FILE_TYPE,
	UNKNOWN_FILE_TYPE,
} FileType;

FileType file_type_of_bytes(u8 *bytes, u32 length);
FileType file_type(FILE *file);

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

#endif
