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

#define AR_GLOBAL_HEADER "!<arch>\n"

typedef struct ArFileHeader
{
  char name[16];
  char modification_timestamp_decimal[12];
  char owner_id_decimal[6];
  char group_id_decimal[6];
  char mode_octal[8];
  char size_bytes_decimal[10];
  char magic[2];
} __attribute__((packed)) ArFileHeader;

inline long checked_ftell(FILE *file)
{
  long ret = ftell(file);
  if (ret == -1) {
    perror("checked_ftell");
    assert(false);
  }

  return ret;
}

inline void checked_fseek(FILE *stream, long offset, int whence)
{
  int ret = fseek(stream, offset, whence);
  if (ret == -1) {
    perror("checked_fseek");
    assert(false);
  }
}

inline void checked_fread(void *ptr, size_t size, size_t nmemb, FILE *stream)
{
  size_t entries_read = fread(ptr, size, nmemb, stream);
  if (entries_read != nmemb) {
    perror("checked_fread");
    assert(false);
  }
}

inline void checked_fwrite(
    const void *ptr, size_t size, size_t nmemb, FILE *stream)
{
  size_t entries_written = fwrite(ptr, size, nmemb, stream);
  if (entries_written != nmemb) {
    perror("checked_fwrite");
    assert(false);
  }
}

#endif
