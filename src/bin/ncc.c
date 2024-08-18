// @PORT
#define _POSIX_SOURCE
#define _DEFAULT_SOURCE
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

// @PORT
#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "array.h"
#include "asm.h"
#include "asm_gen.h"
#include "elf.h"
#include "exit_code.h"
#include "file.h"
#include "ir_gen/ir_gen.h"
#include "misc.h"
#include "syntax/lex.h"
#include "syntax/parse.h"
#include "syntax/preprocess.h"
#include "util.h"

int __lsan_is_turned_off(void) { return 1; }

static bool flag_dump_tokens = false;
static bool flag_dump_ast = false;
static bool flag_dump_ir = false;
static bool flag_dump_asm = false;
bool flag_dump_live_ranges = false;
bool flag_dump_register_assignments = false;
bool flag_print_pre_regalloc_stats = false;

static char *make_temp_file(void);
static int compile_file(
    char *input_filename, char *output_filename, Array(char *) *include_dirs,
    bool syntax_only, bool preprocess_only);
static ExitCode make_file_executable(char *filename);
static char *directory_of_executable(void);

int main(int argc, char *argv[])
{
  // @NOTE: We only use this for generating temp file names. We take extra
  // precautions to make sure the names don't conflict, so it doesn't really
  // matter how we seed this.
  srand(time(NULL));

  Array(char *) source_input_filenames;
  Array(char *) linker_input_filenames;
  ARRAY_INIT(&source_input_filenames, char *, 10);
  ARRAY_INIT(&linker_input_filenames, char *, 10);

  Array(char *) include_dirs = EMPTY_ARRAY;

  bool do_link = true;
  bool syntax_only = false;
  bool preprocess_only = false;
  bool freestanding = false;
  char *output_filename = NULL;

  for (i32 i = 1; i < argc; i++) {
    char *arg = argv[i];
    if (arg[0] == '-') {
      if (streq(arg, "--version")) {
        puts("Naive C Compiler version 1.0");
        return EXIT_CODE_SUCCESS;
      } else if (streq(arg, "-E")) {
        preprocess_only = true;
      } else if (streq(arg, "-dump-tokens")) {
        flag_dump_tokens = true;
      } else if (streq(arg, "-dump-ast")) {
        flag_dump_ast = true;
      } else if (streq(arg, "-dump-ir")) {
        flag_dump_ir = true;
      } else if (streq(arg, "-dump-asm")) {
        flag_dump_asm = true;
      } else if (streq(arg, "-dump-live-ranges")) {
        flag_dump_live_ranges = true;
      } else if (streq(arg, "-dump-register-assignments")) {
        flag_dump_register_assignments = true;
      } else if (streq(arg, "-print-pre-regalloc-stats")) {
        flag_print_pre_regalloc_stats = true;
      } else if (streq(arg, "-fsyntax-only")) {
        syntax_only = true;
      } else if (streq(arg, "-ffreestanding")) {
        freestanding = true;
      } else if (streq(arg, "-c")) {
        do_link = false;
      } else if (strneq(arg, "-I", 2)) {
        char *include_dir = arg + 2;

        u32 len = strlen(include_dir);
        if (include_dir[len - 1] != '/') {
          // @LEAK
          char *with_slash = malloc(len + 2);
          memcpy(with_slash, include_dir, len);
          with_slash[len] = '/';
          with_slash[len + 1] = '\0';

          include_dir = with_slash;
        }

        *ARRAY_APPEND(&include_dirs, char *) = include_dir;
      } else if (strneq(arg, "-std=", 5)) {
        char *standard = arg + 5;
        if (!streq(standard, "c99")) {
          fprintf(stderr, "Error: unsupported C standard '%s'\n", standard);
          return EXIT_CODE_UNIMPLEMENTED;
        }
      } else if (streq(arg, "-o")) {
        if (i == argc - 1) {
          fputs("Error: No filename after '-o'\n", stderr);
          return EXIT_CODE_BAD_CLI;
        }
        output_filename = argv[++i];
      } else if (strneq(arg, "-W", 2)) {
        // Do nothing. We don't support any warnings, so for self-host
        // purposes we'll just ignore these flags, as they shouldn't
        // affect anything if our build is clean anyway.
      } else if (streq(arg, "-g")) {
        // Similarly we ignore "-g". Regardless of whether debug info
        // is present the binary should behave the same, so this should
        // be fine for self hosting.
      } else if (streq(arg, "-fcolor-diagnostics")) {
        // Again, ignore this flag. Not required for correctness, just
        // pretty output.
      } else if (streq(arg, "-fno-asynchronous-unwind-tables")) {
        // Sure thing! We won't generate any unwind tables. We weren't
        // going to anyway.
      } else if (streq(arg, "-fno-common")) {
        // Again, this is our default.
      } else {
        fprintf(stderr, "Error: Unknown command-line argument: %s\n", arg);
        return EXIT_CODE_BAD_CLI;
      }
    } else {
      char *input_filename = arg;

      FILE *input_file = fopen(input_filename, "rb");
      if (input_file == NULL) {
        perror("Unable to open input file");
        return EXIT_CODE_IO_ERROR;
      }

      FileType type = file_type(input_file);

      if (type == ELF_FILE_TYPE || type == AR_FILE_TYPE) {
        *ARRAY_APPEND(&linker_input_filenames, char *) = input_filename;
      } else {
        *ARRAY_APPEND(&source_input_filenames, char *) = input_filename;
      }

      fclose(input_file);
    }
  }

  if (source_input_filenames.size == 0 && linker_input_filenames.size == 0) {
    fputs("Error: no input files given\n", stderr);
    return EXIT_CODE_BAD_CLI;
  }
  if (!do_link && output_filename != NULL && source_input_filenames.size > 1) {
    fputs(
        "Cannot specify output filename"
        " when generating multiple output files\n",
        stderr);
    return EXIT_CODE_BAD_CLI;
  }

  // @LEAK
  char *toolchain_dir = directory_of_executable();

  // Put system headers at the start, so they take precedence.
  if (!freestanding) {
    // @LEAK: concat
    *ARRAY_INSERT(&include_dirs, char *, 0) =
        concat(toolchain_dir, "/include/");
  }
  // @LEAK: concat
  *ARRAY_INSERT(&include_dirs, char *, 0) =
      concat(toolchain_dir, "/freestanding/");

  Array(char *) temp_filenames;
  ARRAY_INIT(
      &temp_filenames, char *, do_link ? source_input_filenames.size : 0);

  for (u32 i = 0; i < source_input_filenames.size; i++) {
    char *source_input_filename =
        *ARRAY_REF(&source_input_filenames, char *, i);
    char *object_filename = NULL;
    if (preprocess_only) {
      object_filename = output_filename;
    } else if (!syntax_only) {
      if (do_link) {
        // In this mode we compile all given sources files to temporary
        // object files, link the result with libc and any other object
        // files passed on the command line, and then delete the
        // temporary object files we created.
        object_filename = make_temp_file();
        *ARRAY_APPEND(&temp_filenames, char *) = object_filename;
        *ARRAY_APPEND(&linker_input_filenames, char *) = object_filename;
      } else {
        // In this mode we compile all the given source files to object
        // files, and leave it at that.
        if (output_filename != NULL) {
          object_filename = output_filename;
        } else {
          u32 input_filename_length = strlen(source_input_filename);
          object_filename = malloc(input_filename_length + 1);  // @LEAK
          memcpy(
              object_filename, source_input_filename,
              input_filename_length + 1);
          object_filename[input_filename_length - 1] = 'o';
          object_filename[input_filename_length] = '\0';

          u32 last_slash = input_filename_length - 1;
          for (; last_slash != 0; last_slash--) {
            if (object_filename[last_slash] == '/') break;
          }

          if (last_slash != 0) {
            object_filename[last_slash - 1] = '.';
            object_filename += last_slash - 1;
          }
        }
      }
    }

    int result = compile_file(
        source_input_filename, object_filename, &include_dirs, syntax_only,
        preprocess_only);
    if (result != 0) return result;
  }

  array_free(&source_input_filenames);

  if (do_link && !syntax_only && !preprocess_only) {
    // Implicitly link in the standard library. We have to put this after
    // the rest of the inputs because it's an archive.
    if (!freestanding) {
      *ARRAY_APPEND(&linker_input_filenames, char *) =
          concat(toolchain_dir, "/libc.a");
    }

    char *executable_filename;
    if (output_filename == NULL)
      executable_filename = "a.out";
    else
      executable_filename = output_filename;

    // @NOTE: Needs to be changed if we support different object file
    // formats.
    ExitCode linker_result =
        link_elf_executable(executable_filename, &linker_input_filenames);
    if (linker_result != EXIT_CODE_SUCCESS) {
      fputs("Linker error, terminating", stderr);
      return linker_result;
    }

    ExitCode result = 0;
    for (u32 i = 0; i < temp_filenames.size; i++) {
      char *temp_filename = *ARRAY_REF(&temp_filenames, char *, i);
      int ret = remove(temp_filename);
      if (ret != 0) {
        perror("Failed to remove temporary object file");
        result = EXIT_CODE_IO_ERROR;
      }
    }

    if (result != EXIT_CODE_SUCCESS) return result;

    result = make_file_executable(executable_filename);
    if (result != EXIT_CODE_SUCCESS) return result;
  }

  array_free(&linker_input_filenames);

  return EXIT_CODE_SUCCESS;
}

static int compile_file(
    char *input_filename, char *output_filename, Array(char *) *include_dirs,
    bool syntax_only, bool preprocess_only)
{
  Array(char) preprocessed;
  Array(Adjustment) adjustments;
  if (!preprocess(input_filename, include_dirs, &preprocessed, &adjustments))
    return EXIT_CODE_INVALID_SOURCE;

  if (preprocess_only) {
    *ARRAY_APPEND(&preprocessed, char) = '\0';
    if (output_filename == NULL || streq(output_filename, "-")) {
      fputs((char *)preprocessed.elements, stdout);
    } else {
      FILE *output = fopen(output_filename, "w");
      if (output == NULL) {
        perror("Unable to open output file");
        return EXIT_CODE_IO_ERROR;
      }

      fputs((char *)preprocessed.elements, output);
      fclose(output);
    }

#if 0
    for (u32 i = 0; i < adjustments.size; i++) {
      Adjustment *adjustment = ARRAY_REF(&adjustments, Adjustment, i);

      char *type;
      switch (adjustment->type) {
      case NORMAL_ADJUSTMENT: type = "normal"; break;
      case BEGIN_MACRO_ADJUSTMENT: type = "begin_macro"; break;
      case END_MACRO_ADJUSTMENT: type = "end_macro"; break;
      }
      u32 location = adjustment->location;
      char *filename = adjustment->new_source_loc.filename;
      u32 line = adjustment->new_source_loc.line;
      u32 column = adjustment->new_source_loc.column;
      printf("%u, %s -> %s:%u:%u\n", location, type, filename, line, column);
    }
#endif

    array_free(&preprocessed);
    array_free(&adjustments);
    return EXIT_CODE_SUCCESS;
  }

  Array(SourceToken) tokens;
  if (!lex(
          &tokens,
          (String){
              .chars = (char *)preprocessed.elements, .len = preprocessed.size},
          &adjustments))
    return EXIT_CODE_INVALID_SOURCE;

  array_free(&preprocessed);
  array_free(&adjustments);

  if (flag_dump_tokens) {
    for (u32 i = 0; i < tokens.size; i++) {
      SourceToken *source_token = ARRAY_REF(&tokens, SourceToken, i);
      u32 line = source_token->source_loc.line;
      u32 column = source_token->source_loc.column;

      printf("%d:%d, ", line, column);
      dump_token((Token *)source_token);
      putchar('\n');
    }
  }

  Pool ast_pool;
  pool_init(&ast_pool, 1024);
  ASTToplevel *ast;
  if (!parse_toplevel(&tokens, &ast_pool, &ast)) {
    return EXIT_CODE_INVALID_SOURCE;
  }

  if (flag_dump_ast) {
    if (flag_dump_tokens) puts("\n");
    dump_toplevel(ast);
  }

  if (syntax_only) {
    array_free(&tokens);
    pool_free(&ast_pool);

    return EXIT_CODE_SUCCESS;
  }

  IrModule ir_module = ir_gen(ast);

  array_free(&tokens);
  pool_free(&ast_pool);

  if (flag_dump_ir) {
    if (flag_dump_tokens || flag_dump_ast) puts("\n");
    dump_ir_module(&ir_module);
  }

  AsmBuilder asm_builder;
  init_asm_builder(&asm_builder, input_filename);
  generate_asm_module(&asm_builder, &ir_module);

  ir_module_free(&ir_module);

  if (flag_dump_asm) {
    if (flag_dump_tokens || flag_dump_ast || flag_dump_ir) puts("\n");
    dump_asm_module(&asm_builder.asm_module);
  }

  assemble(&asm_builder.asm_module);

  ExitCode result =
      write_elf_object_file(output_filename, &asm_builder.asm_module);
  if (result != EXIT_CODE_SUCCESS) return result;

  free_asm_builder(&asm_builder);

  return EXIT_CODE_SUCCESS;
}

// @PORT
static char *make_temp_file(void)
{
  char fmt[] = "/tmp/ncc_temp_%x.o";

  // - 2 adjusts down for the "%x" which isn't present in the output
  // sizeof(int) * 2 is the max length of rand_suffix in hex
  // + 1 for the null terminator
  u32 filename_max_length = sizeof fmt - 2 + sizeof(int) * 2 + 1;
  char *filename = calloc(filename_max_length, 1);

  for (;;) {
    int rand_suffix = rand();
    snprintf(filename, filename_max_length, fmt, rand_suffix);

    int fd = open(filename, O_CREAT | O_WRONLY | O_EXCL, 0600);
    if (fd != -1) {
      close(fd);
      return filename;
    } else {
      assert(errno == EEXIST);
    }
  }
}

// @PORT
static ExitCode make_file_executable(char *filename)
{
  int fd = open(filename, O_RDONLY);
  assert(fd != -1);

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
// /proc/self/exe isn't a standard POSIX thing. On Windows there's
// GetModuleFileName. On some other Unixes there are slightly different
// /proc-based things. On other systems without /proc there isn't a great
// alternative unfortunately.
static char *directory_of_executable(void)
{
  char buf[1024];
  ssize_t size = readlink("/proc/self/exe", buf, sizeof buf);
  assert(size > 0);
  assert(size != 1024);

  while (buf[size - 1] != '/') size--;

  char *ret = malloc(size + 1);
  memcpy(ret, buf, size);
  ret[size] = '\0';

  return ret;
}
