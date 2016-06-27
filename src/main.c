// @PORT
#define _POSIX_SOURCE

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

// @PORT
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "array.h"
#include "asm.h"
#include "asm_gen.h"
#include "elf.h"
#include "ir_gen.h"
#include "misc.h"
#include "tokenise.h"
#include "parse.h"

int main(int argc, char *argv[])
{
	if (argc == 1) {
		fputs("Error: no input file given\n", stderr);
		return 1;
	}

	char *input_filename = NULL;
	bool dump_tokens = false;
	bool dump_ast = false;
	bool dump_ir = false;
	bool dump_asm = false;
	bool do_link = true;

	for (i32 i = 1; i < argc; i++) {
		char *arg = argv[i];
		if (arg[0] == '-') {
			if (streq(arg, "-fdump-tokens")) {
				dump_tokens = true;
			} else if (streq(arg, "-fdump-ast")) {
				dump_ast = true;
			} else if (streq(arg, "-fdump-ir")) {
				dump_ir = true;
			} else if (streq(arg, "-fdump-asm")) {
				dump_asm = true;
			} else if (streq(arg, "-c")) {
				do_link = false;
			} else {
				fprintf(stderr, "Error: Unknown command-line argument: %s\n", arg);
				return 1;
			}
		} else {
			if (input_filename == NULL) {
				input_filename = arg;
			} else {
				fputs("Error: no input file given\n", stderr);
				return 2;
			}
		}
	}

	Array(SourceToken) tokens;
	tokenise(&tokens, input_filename);

	if (dump_tokens) {
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

	if (dump_ast) {
		if (dump_tokens)
			puts("\n");
		dump_toplevel(ast);
	}

	TransUnit tu;
	trans_unit_init(&tu);
	Builder builder;
	builder_init(&builder);

	ir_gen_toplevel(&tu, &builder, ast);

	pool_free(&ast_pool);

	if (dump_ir) {
		if (dump_tokens || dump_ast)
			puts("\n");
		dump_trans_unit(&tu);
	}

	AsmBuilder asm_builder;
	init_asm_builder(&asm_builder);
	generate_asm_module(&asm_builder, &tu); 
	if (dump_asm) {
		if (dump_tokens || dump_ast || dump_ir)
			puts("\n");
		dump_asm_module(&asm_builder.asm_module);
	}


	char *output_filename;
	if (do_link) {
		output_filename = "a.out";
	} else {
		u32 input_filename_length = strlen(input_filename);
		output_filename = malloc(input_filename_length);
		strcpy(output_filename, input_filename);
		output_filename[input_filename_length - 1] = 'o';
	}

	FILE *output_file = fopen(output_filename, "wb");
	if (output_file == NULL) {
		perror("Unable to open output file");
		return 4;
	}
	write_elf_file(output_file, &asm_builder.asm_module, do_link);

	free_asm_builder(&asm_builder);

	if (do_link) {
		// @PORT
		int fd = fileno(output_file);
		assert(fd != -1);

		struct stat status;
		if (fstat(fd, &status) == -1) {
			perror("Unable to stat output file");
			fclose(output_file);
			return 5;
		}

		mode_t new_mode = (status.st_mode & 07777) | S_IXUSR;
		if (fchmod(fd, new_mode) == -1) {
			perror("Unable to change output file to executable");
			fclose(output_file);
			return 6;
		}
	}

	fclose(output_file);

	return 0;
}
