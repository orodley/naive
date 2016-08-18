#include <stdio.h>

#include "file.h"
#include "misc.h"
#include "util.h"

FileType file_type_of_bytes(u8 *bytes, u32 length)
{
	if (length >= 4 && strneq((char *)bytes, "\x7F" "ELF", 4)) {
		return ELF_FILE_TYPE;
	}
	if (length >= 8 && strneq((char *)bytes, "!<arch>\n", 8)) {
		return AR_FILE_TYPE;
	}

	return UNKNOWN_FILE_TYPE;
}

FileType file_type(FILE *file)
{
	u8 magic[8];
	size_t items_read = fread(magic, 1, sizeof magic, file);
	return file_type_of_bytes(magic, items_read);
}

extern inline long checked_ftell(FILE *file);
extern inline void checked_fseek(FILE *file, long offset, int whence);
extern inline void checked_fread(void *ptr, size_t size, size_t nmemb, FILE *stream);
extern inline void checked_fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream);
