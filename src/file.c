// @PORT
#define _DEFAULT_SOURCE
#define _POSIX_SOURCE

#include "file.h"

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
// @PORT
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "types.h"

extern inline long checked_ftell(FILE *file);
extern inline void checked_fseek(FILE *file, long offset, int whence);
extern inline void checked_fread(
    void *ptr, size_t size, size_t nmemb, FILE *stream);
extern inline void checked_fwrite(
    const void *ptr, size_t size, size_t nmemb, FILE *stream);

FileType file_type_of_bytes(u8 *bytes, u32 length)
{
  if (length >= 4
      && strneq(
          (char *)bytes,
          "\x7F"
          "ELF",
          4)) {
    return ELF_FILE_TYPE;
  }
  if (length >= sizeof AR_GLOBAL_HEADER - 1
      && strneq((char *)bytes, AR_GLOBAL_HEADER, sizeof AR_GLOBAL_HEADER - 1)) {
    return AR_FILE_TYPE;
  }

  return UNKNOWN_FILE_TYPE;
}

FileType file_type(FILE *file)
{
  long initial_pos = checked_ftell(file);

  u8 magic[8];
  size_t items_read = fread(magic, 1, sizeof magic, file);
  FileType type = file_type_of_bytes(magic, items_read);

  checked_fseek(file, initial_pos, SEEK_SET);
  return type;
}

// @PORT
String make_temp_file(void)
{
  for (;;) {
    int rand_suffix = rand();
    String filename = string_printf("/tmp/ncc_temp_%x.o", rand_suffix);

    int fd = open(filename.chars, O_CREAT | O_WRONLY | O_EXCL, 0600);
    if (fd != -1) {
      close(fd);
      return filename;
    } else if (errno != EEXIST) {
      perror("Unable to create temporary file");
      exit_with_code(EXIT_CODE_IO_ERROR);
    }
  }
}

// @PORT
ExitCode make_file_executable(String filename)
{
  // @LEAK
  int fd = open(string_to_c_string(filename), O_RDONLY);
  if (fd == -1) {
    perror("Unable to open file to make executable");
    return EXIT_CODE_IO_ERROR;
  }

  struct stat status;
  if (fstat(fd, &status) == -1) {
    perror("Unable to stat output file");
    close(fd);
    return EXIT_CODE_IO_ERROR;
  }

  mode_t new_mode = (status.st_mode & 07777) | S_IXUSR;
  if (fchmod(fd, new_mode) == -1) {
    perror("Unable to change output file to executable");
    close(fd);
    return EXIT_CODE_IO_ERROR;
  }

  close(fd);
  return EXIT_CODE_SUCCESS;
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
