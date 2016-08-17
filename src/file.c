#include <stdio.h>

#include "file.h"
#include "misc.h"
#include "util.h"

FileType file_type_of_bytes(u8 *bytes, u32 length)
{
	if (length >= 4 && bytes[0] == 0x7F &&
		bytes[1] == 'E' && bytes[2] == 'L' && bytes[3] == 'F') {
		return ELF_FILE_TYPE;
	}
	if (length == 7 && streq((char *)bytes, "!<arch>")) {
		return AR_FILE_TYPE;
	}

	return UNKNOWN_FILE_TYPE;
}

FileType file_type(FILE *file)
{
	u8 magic[8] = { [7] = 0 };
	size_t items_read = fread(magic, 1, 7, file);
	return file_type_of_bytes(magic, items_read);
}

extern inline long checked_ftell(FILE *file);
extern inline void checked_fseek(FILE *file, long offset, int whence);
extern inline void checked_fread(void *ptr, size_t size, size_t nmemb, FILE *stream);
extern inline void checked_fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream);
