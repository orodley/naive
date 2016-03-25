// @PORT
#define _POSIX_SOURCE

#include <assert.h>
#include <stdio.h>

// @PORT
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "array.h"
#include "asm.h"
#include "elf.h"
#include "ir_gen.h"
#include "misc.h"
#include "tokenise.h"
#include "parse.h"

int main(int argc, char *argv[])
{
	if (argc != 2) {
		fprintf(stderr, "Usage: %s <input file>\n", argv[0]);
		return 1;
	}

	char *input_filename = argv[1];

	Array(SourceToken) tokens;
	tokenise(&tokens, input_filename);

	for (u32 i = 0; i < tokens.size; i++) {
		SourceToken *source_token = ARRAY_REF(&tokens, SourceToken, i);
		u32 line = source_token->source_loc.line;
		u32 column = source_token->source_loc.column;

		printf("%d:%d, ", line, column);
		dump_token((Token *)source_token);
		putchar('\n');
	}
	puts("\n");

	Pool ast_pool;
	pool_init(&ast_pool, 1024);
	ASTToplevel *ast = parse_toplevel(&tokens, &ast_pool);
	if (ast == NULL)
		return 2;

	dump_toplevel(ast);
	puts("\n");


	TransUnit tu;
	trans_unit_init(&tu);
	Builder builder;
	builder_init(&builder);

	ir_gen_function(&tu, &builder, ast);

	pool_free(&ast_pool);
	dump_trans_unit(&tu);
	puts("\n");


	AsmModule asm_module;
	init_asm_module(&asm_module);
	generate_asm_module(&tu, &asm_module);

	dump_asm_module(&asm_module);


	FILE *output_file = fopen("a.out", "wb");
	if (output_file == NULL) {
		perror("Unable to open output file");
		return 3;
	}
	write_elf_file(output_file, &asm_module);

	// @PORT
	int fd = fileno(output_file);
	assert(fd != -1);

	struct stat status;
	if (fstat(fd, &status) == -1) {
		perror("Unable to stat output file");
		fclose(output_file);
		return 4;
	}

	mode_t new_mode = (status.st_mode & 07777) | S_IXUSR;
	if (fchmod(fd, new_mode) == -1) {
		perror("Unable to change output file to executable");
		fclose(output_file);
		return 5;
	}

	fclose(output_file);

	return 0;
}
