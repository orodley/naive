// @PORT
#define _POSIX_SOURCE

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

// @PORT
#include <errno.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "array.h"
#include "asm.h"
#include "asm_gen.h"
#include "elf.h"
#include "file.h"
#include "ir_gen.h"
#include "misc.h"
#include "tokenise.h"
#include "parse.h"
#include "util.h"

static bool flag_dump_tokens = false;
static bool flag_dump_ast = false;
static bool flag_dump_ir = false;
static bool flag_dump_asm = false;
bool flag_dump_live_ranges = false;

static char *make_temp_file();
static int compile_file(char *input_filename, char *output_filename, bool syntax_only);
static int make_file_executable(char *filename);

int main(int argc, char *argv[])
{
	// @NOTE: We only use this for generating temp file names. We go to extra
	// precautions to make sure the names don't conflict, so it doesn't really
	// matter how we seed this.
	srand(time(NULL));

	Array(char *) source_input_filenames;
	Array(char *) linker_input_filenames;
	ARRAY_INIT(&source_input_filenames, char *, 10);
	ARRAY_INIT(&linker_input_filenames, char *, 10);

	bool do_link = true;
	bool syntax_only = false;
	char *output_filename = NULL;

	for (i32 i = 1; i < argc; i++) {
		char *arg = argv[i];
		if (arg[0] == '-') {
			if (streq(arg, "-fdump-tokens")) {
				flag_dump_tokens = true;
			} else if (streq(arg, "-fdump-ast")) {
				flag_dump_ast = true;
			} else if (streq(arg, "-dump-ir")) {
				flag_dump_ir = true;
			} else if (streq(arg, "-dump-asm")) {
				flag_dump_asm = true;
			} else if (streq(arg, "-dump-live-ranges")) {
				flag_dump_live_ranges = true;
			} else if (streq(arg, "-fsyntax-only")) {
				syntax_only = true;
			} else if (streq(arg, "-c")) {
				do_link = false;
			} else if (streq(arg, "-o")) {
				output_filename = argv[++i];
			} else {
				fprintf(stderr, "Error: Unknown command-line argument: %s\n", arg);
				return 1;
			}
		} else {
			char *input_filename = arg;

			FILE *input_file = fopen(input_filename, "rb");
			if (input_file == NULL) {
				perror("Unable to open input file");
				return 7;
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
		return 2;
	}
	if (!do_link && output_filename != NULL && source_input_filenames.size > 1) {
		fputs("Cannot specify output filename"
				" when generating multiple output files\n", stderr);
		return 12;
	}

	Array(char *) temp_filenames;
	ARRAY_INIT(&temp_filenames, char *, do_link ? source_input_filenames.size : 0);

	for (u32 i = 0; i < source_input_filenames.size; i++) {
		char *source_input_filename = *ARRAY_REF(&source_input_filenames, char *, i);
		char *object_filename = NULL;
		if (do_link && !syntax_only) {
			// In this mode we compile all given sources files to temporary
			// object files, link the result with libc and any other object
			// files passed on the command line, and then delete the temporary
			// object files we created.
			object_filename = make_temp_file();
			*ARRAY_APPEND(&temp_filenames, char *) = object_filename;
			*ARRAY_APPEND(&linker_input_filenames, char *) = object_filename;
		} else if (!syntax_only) {
			// In this mode we compile all the given source files to object
			// files, and leave it at that.
			if (output_filename != NULL) {
				object_filename = output_filename;
			} else {
				u32 input_filename_length = strlen(source_input_filename);
				object_filename = malloc(input_filename_length + 1); // @LEAK
				memcpy(object_filename, source_input_filename, input_filename_length + 1);
				object_filename[input_filename_length - 1] = 'o';
				object_filename[input_filename_length] = '\0';

				u32 last_slash = input_filename_length - 1;
				for (; last_slash != 0; last_slash--) {
					if (object_filename[last_slash] == '/')
						break;
				}

				if (last_slash != 0) {
					object_filename[last_slash - 1] = '.';
					object_filename += last_slash - 1;
				}
			}
		}

		int result = compile_file(source_input_filename, object_filename, syntax_only);
		if (result != 0)
			return result;
	}

	array_free(&source_input_filenames);

	if (do_link && !syntax_only) {
		// Implicitly link in the standard library. We have to put this after
		// the rest of the inputs because it's an archive.
		*ARRAY_APPEND(&linker_input_filenames, char *) = "libc.a";

		char *executable_filename;
		if (output_filename == NULL)
			executable_filename = "a.out";
		else
			executable_filename = output_filename;

		// @NOTE: Needs to be changed if we support different object file
		// formats.
		if (!link_elf_executable(executable_filename, &linker_input_filenames)) {
			puts("Linker error, terminating");
			return 10;
		}

		int result = 0;
		for (u32 i = 0; i < temp_filenames.size; i++) {
			char *temp_filename = *ARRAY_REF(&temp_filenames, char *, i);
			int result = remove(temp_filename);
			if (result != 0) {
				perror("Failed to remove temporary object file");
				result = 9;
			}
		}

		if (result != 0)
			return result;

		result = make_file_executable(executable_filename);
		if (result != 0)
			return result;
	}

	array_free(&linker_input_filenames);

	return 0;
}

static int compile_file(char *input_filename, char *output_filename, bool syntax_only) {
	Array(SourceToken) tokens;
	if (!tokenise(&tokens, input_filename))
		return 11;

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
	ASTToplevel *ast = parse_toplevel(&tokens, &ast_pool);
	if (ast == NULL)
		return 3;

	if (flag_dump_ast) {
		if (flag_dump_tokens)
			puts("\n");
		dump_toplevel(ast);
	}

	if (syntax_only) {
		array_free(&tokens);
		pool_free(&ast_pool);

		return 0;
	}

	TransUnit tu;
	trans_unit_init(&tu);
	IrBuilder builder;
	builder_init(&builder, &tu);

	ir_gen_toplevel(&builder, ast);

	array_free(&tokens);
	pool_free(&ast_pool);

	if (flag_dump_ir) {
		if (flag_dump_tokens || flag_dump_ast)
			puts("\n");
		dump_trans_unit(&tu);
	}

	AsmBuilder asm_builder;
	init_asm_builder(&asm_builder, input_filename);
	generate_asm_module(&asm_builder, &tu); 

	trans_unit_free(&tu);

	if (flag_dump_asm) {
		if (flag_dump_tokens || flag_dump_ast || flag_dump_ir)
			puts("\n");
		dump_asm_module(&asm_builder.asm_module);
	}

	// @NOTE: Needs to be changed if we support different object file
	// formats.
	if (!write_elf_object_file(output_filename, &asm_builder.asm_module))
		return 4;

	free_asm_builder(&asm_builder);

	return 0;
}

// @PORT
static char *make_temp_file()
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
static int make_file_executable(char *filename)
{
	int fd = open(filename, O_RDONLY);
	assert(fd != -1);

	struct stat status;
	if (fstat(fd, &status) == -1) {
		perror("Unable to stat output file");
		close(fd);
		return 5;
	}

	mode_t new_mode = (status.st_mode & 07777) | S_IXUSR;
	if (fchmod(fd, new_mode) == -1) {
		perror("Unable to change output file to executable");
		close(fd);
		return 6;
	}

	close(fd);
	return 0;
}
