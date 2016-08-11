// @PORT
#define _POSIX_SOURCE

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

// @PORT
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#include "array.h"
#include "asm.h"
#include "asm_gen.h"
#include "elf.h"
#include "ir_gen.h"
#include "misc.h"
#include "tokenise.h"
#include "parse.h"
#include "util.h"

static bool flag_dump_tokens = false;
static bool flag_dump_ast = false;
static bool flag_dump_ir = false;
static bool flag_dump_asm = false;

static char *make_temp_file();
static int compile_file(char *input_filename, char *output_filename);
static int make_file_executable(char *filename);

int main(int argc, char *argv[])
{
	Array(char *) input_filenames;
	ARRAY_INIT(&input_filenames, char *, 10);
	bool do_link = true;

	for (i32 i = 1; i < argc; i++) {
		char *arg = argv[i];
		if (arg[0] == '-') {
			if (streq(arg, "-fdump-tokens")) {
				flag_dump_tokens = true;
			} else if (streq(arg, "-fdump-ast")) {
				flag_dump_ast = true;
			} else if (streq(arg, "-fdump-ir")) {
				flag_dump_ir = true;
			} else if (streq(arg, "-fdump-asm")) {
				flag_dump_asm = true;
			} else if (streq(arg, "-c")) {
				do_link = false;
			} else {
				fprintf(stderr, "Error: Unknown command-line argument: %s\n", arg);
				return 1;
			}
		} else {
			*ARRAY_APPEND(&input_filenames, char *) = arg;
		}
	}

	if (input_filenames.size == 0) {
		fputs("Error: no input files given\n", stderr);
		return 2;
	}

	Array(char *) source_input_filenames;
	Array(char *) linker_input_filenames;
	ARRAY_INIT(&source_input_filenames, char *, input_filenames.size);
	ARRAY_INIT(&linker_input_filenames, char *, input_filenames.size);

	for (u32 i = 0; i < input_filenames.size; i++) {
		char *input_filename = *ARRAY_REF(&input_filenames, char *, i);
		FILE *input_file = fopen(input_filename, "rb");
		if (input_file == NULL) {
			perror("Unable to open input file");
			return 7;
		}

		u8 magic[4];
		size_t items_read = fread(magic, 1, sizeof magic, input_file);
		if (items_read != sizeof magic) {
			perror("Failed to determine type of input file");
			return 8;
		}

		// @NOTE: Needs to be changed if we support different object file
		// formats.
		if (magic[0] == 0x7F &&
				magic[1] == 'E' && magic[2] == 'L' && magic[3] == 'F') {
			*ARRAY_APPEND(&linker_input_filenames, char *) = input_filename;
		} else {
			*ARRAY_APPEND(&source_input_filenames, char *) = input_filename;
		} 

		fclose(input_file);
	}

	Array(char *) temp_filenames;
	ARRAY_INIT(&temp_filenames, char *, do_link ? source_input_filenames.size : 0);

	for (u32 i = 0; i < source_input_filenames.size; i++) {
		char *source_input_filename = *ARRAY_REF(&source_input_filenames, char *, i);
		char *output_filename;
		if (do_link) {
			// In this mode we compile all given sources files to temporary
			// object files, link the result with the stdlib and any other
			// object files passed on the command line, and then delete the
			// temporary object files we created.
			output_filename = make_temp_file();
			*ARRAY_APPEND(&temp_filenames, char *) = output_filename;
			*ARRAY_APPEND(&linker_input_filenames, char *) = output_filename;
		} else {
			// In this mode we compile all the given source files to object
			// files, and leave it at that.
			// @TODO: Support "-o" flag.
			u32 input_filename_length = strlen(source_input_filename);
			output_filename = malloc(input_filename_length); // @LEAK
			strcpy(output_filename, source_input_filename);
			output_filename[input_filename_length - 1] = 'o';

			u32 last_slash = input_filename_length - 1;
			for (; last_slash != 0; last_slash--) {
				if (output_filename[last_slash] == '/')
					break;
			}

			if (last_slash != 0) {
				output_filename[last_slash - 1] = '.';
				output_filename += last_slash - 1;
			}
		}

		int result = compile_file(source_input_filename, output_filename);
		if (result != 0)
			return result;
	}

	if (do_link) {
		// @TODO: Support "-o" flag.
		char *executable_filename = "a.out";

		// @NOTE: Needs to be changed if we support different object file
		// formats.
		link_elf_executable(executable_filename, &linker_input_filenames);

		int ret = 0;

		for (u32 i = 0; i < temp_filenames.size;i ++) {
			char *temp_filename = *ARRAY_REF(&temp_filenames, char *, i);
			int result = remove(temp_filename);
			if (result != 0) {
				perror("Failed to remove temporary object file");
				ret = 9;
			}

			if (ret != 0)
				return ret;
		}

		int result = make_file_executable(executable_filename);
		if (result != 0)
			return result;
	}

	return 0;
}

static int compile_file(char *input_filename, char *output_filename) {
	Array(SourceToken) tokens;
	tokenise(&tokens, input_filename);

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

	TransUnit tu;
	trans_unit_init(&tu);
	IrBuilder builder;
	builder_init(&builder, &tu);

	ir_gen_toplevel(&tu, &builder, ast);

	array_free(&tokens);
	pool_free(&ast_pool);

	if (flag_dump_ir) {
		if (flag_dump_tokens || flag_dump_ast)
			puts("\n");
		dump_trans_unit(&tu);
	}

	AsmBuilder asm_builder;
	init_asm_builder(&asm_builder);
	generate_asm_module(&asm_builder, &tu); 

	trans_unit_free(&tu);

	if (flag_dump_asm) {
		if (flag_dump_tokens || flag_dump_ast || flag_dump_ir)
			puts("\n");
		dump_asm_module(&asm_builder.asm_module);
	}

	FILE *output_file = fopen(output_filename, "wb");
	if (output_file == NULL) {
		perror("Unable to open output file");
		return 4;
	}
	// @NOTE: Needs to be changed if we support different object file
	// formats.
	write_elf_file(output_file, &asm_builder.asm_module);

	free_asm_builder(&asm_builder);

	fclose(output_file);

	return 0;
}

// @PORT
static char *make_temp_file()
{
	UNIMPLEMENTED;
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
