#include <stdlib.h>
#include <string.h>

// @PORT
#include <fcntl.h>
#include <sys/mman.h>
#include <unistd.h>

#include "assertions.h"
#include "util.h"

extern inline u32 max(u32 a, u32 b);

extern inline bool is_valid(String str);

extern inline bool streq(char *a, char *b);
extern inline bool strneq(char *a, char *b, u32 length);

extern inline u32 lowest_set_bit(u64 x);
extern inline u32 highest_set_bit(u64 x);
extern inline u32 bit_count(u32 x);

extern inline u32 align_to(u32 n, u32 align);

extern inline u32 float_to_raw_bits(float f);
extern inline u64 double_to_raw_bits(double f);

char *nconcat(char *str_a, u32 len_a, char *str_b, u32 len_b)
{
  u32 result_length = len_a + len_b;
  char *result = malloc(result_length + 1);
  memcpy(result, str_a, len_a);
  memcpy(result + len_a, str_b, len_b);
  result[result_length] = '\0';

  return result;
}

char *concat(char *str_a, char *str_b)
{
  return nconcat(str_a, strlen(str_a), str_b, strlen(str_b));
}

// @PORT
String map_file_into_memory(char *filename)
{
  int fd = open(filename, O_RDONLY);
  if (fd == -1) return INVALID_STRING;

  off_t file_size = lseek(fd, 0, SEEK_END);

  if (file_size == -1) return INVALID_STRING;

  if (file_size == 0) return EMPTY_STRING;

  char *buffer = mmap(NULL, file_size, PROT_READ, MAP_PRIVATE, fd, 0);
  if (buffer == MAP_FAILED) return INVALID_STRING;

  close(fd);

  return (String){buffer, file_size};
}

// @PORT
void unmap_file(String buffer)
{
  int ret = munmap(buffer.chars, buffer.len);
  if (ret != 0) {
    exit_with_code(EXIT_CODE_IO_ERROR);
  }
}
