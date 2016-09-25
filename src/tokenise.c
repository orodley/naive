#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "array.h"
#include "diagnostics.h"
#include "misc.h"
#include "tokenise.h"
#include "util.h"

// @PORT
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/mman.h>

typedef struct InputBuffer
{
	char *buffer;
	size_t length;
} InputBuffer;

#define INVALID_INPUT_BUFFER ((InputBuffer) { NULL, -1 })

static inline bool is_valid(InputBuffer ib)
{
	return !((ib.buffer == INVALID_INPUT_BUFFER.buffer) &&
		(ib.length == INVALID_INPUT_BUFFER.length));
}

// @PORT
static InputBuffer map_file_into_memory(char *filename)
{
	int fd = open(filename, O_RDONLY);
	if (fd == -1)
		return INVALID_INPUT_BUFFER;

	off_t file_size = lseek(fd, 0, SEEK_END);

	if (file_size == -1)
		return INVALID_INPUT_BUFFER;

	if (file_size == 0)
		return (InputBuffer) { NULL, 0 };

	char *buffer = mmap(NULL, file_size, PROT_READ, MAP_PRIVATE, fd, 0);
	if (buffer == MAP_FAILED)
		return INVALID_INPUT_BUFFER;

	close(fd);

	return (InputBuffer) { buffer, file_size };
}

// @PORT
static void unmap_file(InputBuffer buffer)
{
	int ret = munmap(buffer.buffer, buffer.length);
	assert(ret == 0);
}


typedef struct Macro
{
	char *name;
	char *value;
	Array(char *) arg_names;
} Macro;

static Macro *look_up_macro(Array(Macro) *macro_env, char *name)
{
	// Iterate in reverse order so that macro params have priority over a
	// macro with the same name.
	for (i32 i = macro_env->size - 1; i >= 0; i--) {
		Macro *m = ARRAY_REF(macro_env, Macro, i);
		if (streq(m->name, name)) {
			return m;
		}
	}

	return NULL;
}

typedef struct Reader
{
	Array(SourceToken) *tokens;

	InputBuffer buffer;
	u32 position;

	SourceLoc source_loc;
	u32 last_line_length;
	bool first_token_of_line;

	Array(Macro) macro_env;
} Reader;

static inline bool at_end(Reader *reader) {
	return reader->position >= reader->buffer.length;
}

static inline char peek_char(Reader *reader)
{
	return reader->buffer.buffer[reader->position];
}

static inline void next_char(Reader *reader)
{
	reader->position++;

	if (peek_char(reader) == '\n') {
		reader->last_line_length = reader->source_loc.column;
		reader->source_loc.line++;
		reader->source_loc.column = 0;
	} else {
		reader->source_loc.column++;
	}
}

static inline void back_up(Reader *reader)
{
	if (peek_char(reader) == '\n') {
		assert(reader->source_loc.line != 1);

		reader->source_loc.line--;
		reader->source_loc.column = reader->last_line_length;
	} else {
		reader->source_loc.column--;
	}

	reader->position--;
}

static void advance(Reader *reader)
{
	for (;;) {
		next_char(reader);
		if (peek_char(reader) == '\\') {
			next_char(reader);
			if (peek_char(reader) == '\n') {
				continue;
			} else {
				back_up(reader);
				break;
			}
		} else {
			break;
		}
	}
}

static inline char read_char(Reader *reader)
{
	char c = peek_char(reader);
	advance(reader);

	return c;
}

static void skip_whitespace_and_comments(Reader *reader, bool skip_newline)
{
	while (!at_end(reader)) {
		switch (peek_char(reader)) {
		case '\n':
			reader->first_token_of_line = true;
			if (skip_newline)
				break;
			else
				return;
		case ' ': case '\t':
			break;
		case '/':
			advance(reader);
			switch (peek_char(reader)) {
			case '/':
				while (peek_char(reader) != '\n' && !at_end(reader)) {
					advance(reader);
				}
				break;
			case '*':
				advance(reader);
				while (!at_end(reader)) {
					if (read_char(reader) == '*') {
						if (read_char(reader) == '/')
							break;

						back_up(reader);
					}
				}

				if (at_end(reader))
					issue_error(&reader->source_loc, "Unterminated /* comment");
				break;
			default:
				back_up(reader);
				return;
			}
			break;
		default:
			return;
		}

		advance(reader);
	}
}


static inline bool initial_ident_char(char c)
{
	return  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c == '_');
}

static inline bool ident_char(char c)
{
	return initial_ident_char(c) || (c >= '0' && c <= '9');
}


static Token *append_token(Reader *reader, SourceLoc source_loc, TokenType type)
{
	SourceToken *source_token = ARRAY_APPEND(reader->tokens, SourceToken);
	source_token->token.type = type;
	source_token->source_loc = source_loc;

	return (Token *)source_token;
}

static char *read_symbol(Reader *reader)
{
	u32 start_index = reader->position - 1;
	for (;;) {
		char c = peek_char(reader);
		if (!ident_char(c))
			break;

		advance(reader);
	}

	u32 length = reader->position - start_index;
	return strndup(reader->buffer.buffer + start_index, length);
}

static bool handle_pp_directive(Reader *reader);
static bool tokenise_file(Reader *reader, char *input_filename,
		SourceLoc blame_source_loc);
static bool tokenise_aux(Reader *reader);

// @IMPROVE: Replace with a finite state machine
bool tokenise(Array(SourceToken) *tokens, char *input_filename)
{
	ARRAY_INIT(tokens, SourceToken, 500);

	Reader reader;
	reader.tokens = tokens;
	reader.source_loc = (SourceLoc) { NULL, 0, 0 };
	ARRAY_INIT(&reader.macro_env, Macro, 10);

	bool ret = tokenise_file(&reader, input_filename, reader.source_loc);

	for (u32 i = 0; i < reader.macro_env.size; i++) {
		Macro *macro = ARRAY_REF(&reader.macro_env, Macro, i);
		free(macro->name);
		free(macro->value);
		array_free(&macro->arg_names);
	}
	array_free(&reader.macro_env);

	return ret;
}


static bool tokenise_string(Reader *reader, char *string)
{
	u32 old_position = reader->position;
	SourceLoc old_source_loc = reader->source_loc;
	InputBuffer old_buffer = reader->buffer;

	InputBuffer buffer;
	buffer.buffer = string;
	buffer.length = strlen(string);
	reader->buffer = buffer;
	reader->position = 0;
	reader->source_loc = (SourceLoc) { "<macro>", 1, 1 };

	if (!tokenise_aux(reader))
		return false;

	reader->position = old_position;
	reader->buffer = old_buffer;
	reader->source_loc = old_source_loc;

	return true;
}

static bool tokenise_file(Reader *reader, char *input_filename,
		SourceLoc blame_source_loc)
{
	u32 old_position = reader->position;
	SourceLoc old_source_loc = reader->source_loc;
	InputBuffer old_buffer = reader->buffer;

	InputBuffer buffer = map_file_into_memory(input_filename);
	if (!is_valid(buffer)) {
		if (blame_source_loc.filename == NULL) {
			fprintf(stderr, "Failed to open input file: '%s'\n", input_filename);
		} else {
			issue_error(&blame_source_loc, "File not found: '%s'\n", input_filename);
		}

		return false;
	}

	reader->buffer = buffer;
	reader->position = 0;
	reader->source_loc = (SourceLoc) { input_filename, 1, 1, };
	reader->first_token_of_line = true;

	if (buffer.buffer == NULL)
		return false;

	if (!tokenise_aux(reader))
		return false;

	unmap_file(buffer);

	reader->position = old_position;
	reader->buffer = old_buffer;
	reader->source_loc = old_source_loc;

	return true;
}


static void read_int_literal_suffix(Reader *reader)
{
	// @TODO: Assign the type based on the suffix and the table in 6.4.4.1.5
	bool read_length_suffix = false;
	bool read_unsigned_suffix = false;

	for (;;) {
		char c = read_char(reader);
		switch (c) {
		case 'u': case 'U':
			if (read_unsigned_suffix) {
				issue_error(&reader->source_loc,
						"Multiple 'u' suffixes on integer literal");
			}

			read_unsigned_suffix = true;
			break;
		case 'l': case 'L':
			if (read_length_suffix) {
				issue_error(&reader->source_loc,
						"Multiple 'l'/'ll' suffixes on integer literal");
			}

			read_length_suffix = true;
			if (peek_char(reader) == c) {
				advance(reader);
			}
			break;
		default:
			back_up(reader);
			return;
		}
	}
}

static bool substitute_macro_params(Reader *reader, Macro *macro)
{
	bool ret;

	Array(char) arg_chars;
	ARRAY_INIT(&arg_chars, char, 20);
	u32 args_processed = 0;
	for (;;) {
		char c = read_char(reader);
		if (c == '(') {
			u32 bracket_depth = 1;
			*ARRAY_APPEND(&arg_chars, char) = c;

			while (bracket_depth != 0) {
				char c = read_char(reader);
				switch (c) {
				case '(': bracket_depth++; break;
				case ')': bracket_depth--; break;
				case '\n':
					issue_error(&reader->source_loc,
							"Unexpected newline in macro argument list");
					ret = false;
					goto cleanup;
				}

				*ARRAY_APPEND(&arg_chars, char) = c;
			}
		} else if (c == ')' || c == ',') {
			if (args_processed == macro->arg_names.size) {
				issue_error(&reader->source_loc,
						"Too many parameters to function-like macro"
						" (expected %u)",
						macro->arg_names.size);
				ret = false;
				goto cleanup;
			}
			Macro *arg_macro = ARRAY_APPEND(&reader->macro_env, Macro);
			arg_macro->name = *ARRAY_REF(&macro->arg_names, char *, args_processed);
			arg_macro->value = strndup((char *)arg_chars.elements, arg_chars.size); 
			array_clear(&arg_chars);
			args_processed++;

			if (c == ')') {
				ret = true;
				if (args_processed != macro->arg_names.size) {
					issue_error(&reader->source_loc,
							"Not enough parameters to function-like macro"
							" (expected %u, got %u)",
							macro->arg_names.size,
							args_processed);
					ret = false;
				}

				goto cleanup;
			}
		} else {
			*ARRAY_APPEND(&arg_chars, char) = c;
		}
	}

cleanup:
	array_free(&arg_chars);
	return ret;
}

static bool tokenise_aux(Reader *reader)
{
	while (!at_end(reader)) {
		skip_whitespace_and_comments(reader, true);
		if (at_end(reader))
			break;

		SourceLoc start_source_loc = reader->source_loc;

		switch (read_char(reader)) {
		case '0': {
			char c = peek_char(reader);
			u64 value = 0;
			if (c == 'x') {
				bool at_least_one_digit = false;
				for (;;) {
					advance(reader);
					char c = peek_char(reader);
					if (!((c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
								|| (c >= '0' && c <= '9')))
						break;

					at_least_one_digit = true;
					value *= 16;

					if (c >= 'a' && c <= 'f')
						value += c - 'a' + 10;
					else if (c >= 'A' && c <= 'F')
						value += c - 'A' + 10;
					else
						value += c - '0';
				}

				if (!at_least_one_digit) {
					issue_error(&reader->source_loc,
							"Hexadecimal literal must have at least one digit");
				}
			} else {
				while (c >= '0' && c <= '9') {
					if (c == '8' || c == '9') {
						// @TODO: Skip past all numeric characters to resync?
						issue_error(&reader->source_loc,
								"Invalid digit '%c' in octal literal", c);
						break;
					} else {
						value *= 8;
						value += c - '0';

						advance(reader);
						c = peek_char(reader);
					}
				}
			}

			read_int_literal_suffix(reader);

			Token *token = append_token(reader, start_source_loc, TOK_INT_LITERAL);
			token->val.int_literal = value;
			break;
		}
		case '1': case '2': case '3': case '4': case '5': case '6':
		case '7': case '8': case '9': {
			back_up(reader);
			u64 value = 0;

			for (;;) {
				char c = peek_char(reader);
				if (!(c >= '0' && c <= '9'))
					break;

				value *= 10;
				value += c - '0';
				advance(reader);
			}

			read_int_literal_suffix(reader);

			Token *token = append_token(reader, start_source_loc, TOK_INT_LITERAL);
			token->val.int_literal = value;

			break;
		}
		case '"': {
			u32 start_index = reader->position;
			for (;;) {
				char c = read_char(reader);

				if (c == '\\')
					advance(reader);
				else if (c == '"')
					break;
			}

			u32 length = ((reader->position - 1) - start_index);

			Token *token = append_token(reader, start_source_loc, TOK_STRING_LITERAL);
			token->val.string_literal = strndup(
					reader->buffer.buffer + start_index, length);

			break;
		}
		case '+':
			switch (read_char(reader)) {
			case '+':
				append_token(reader, start_source_loc, TOK_INCREMENT);
				break;
			case '=':
				append_token(reader, start_source_loc, TOK_PLUS_ASSIGN);
				break;
			default:
				back_up(reader);
				append_token(reader, start_source_loc, TOK_PLUS);
			}

			break;

		case '-':
			switch (read_char(reader)) {
			case '-':
				append_token(reader, start_source_loc, TOK_DECREMENT);
				break;
			case '=':
				append_token(reader, start_source_loc, TOK_MINUS_ASSIGN);
				break;
			case '>':
				append_token(reader, start_source_loc, TOK_ARROW);
				break;
			default:
				back_up(reader);
				append_token(reader, start_source_loc, TOK_MINUS);
			}

			break;

		case '*':
			if (read_char(reader) == '=') {
				append_token(reader, start_source_loc, TOK_MULT_ASSIGN);
			} else {
				back_up(reader);
				append_token(reader, start_source_loc, TOK_ASTERISK);
			}

			break;

		case '/':
			if (read_char(reader) == '=') {
				append_token(reader, start_source_loc, TOK_DIVIDE_ASSIGN);
			} else {
				back_up(reader);
				append_token(reader, start_source_loc, TOK_DIVIDE);
			}

			break;

		case '%':
			if (read_char(reader) == '=') {
				append_token(reader, start_source_loc, TOK_MODULO_ASSIGN);
			} else {
				back_up(reader);
				append_token(reader, start_source_loc, TOK_MODULO);
			}

			break;

		case '&':
			switch (read_char(reader)) {
			case '&':
				append_token(reader, start_source_loc, TOK_LOGICAL_AND);
				break;
			case '=':
				append_token(reader, start_source_loc, TOK_BIT_AND_ASSIGN);
				break;
			default:
				back_up(reader);
				append_token(reader, start_source_loc, TOK_AMPERSAND);
			}

			break;

		case '|':
			switch (read_char(reader)) {
			case '|':
				append_token(reader, start_source_loc, TOK_LOGICAL_OR);
				break;
			case '=':
				append_token(reader, start_source_loc, TOK_BIT_OR_ASSIGN);
				break;
			default:
				back_up(reader);
				append_token(reader, start_source_loc, TOK_BIT_OR);
			}

			break;

		case '^':
			if (read_char(reader) == '=') {
				append_token(reader, start_source_loc, TOK_BIT_XOR_ASSIGN);
			} else {
				back_up(reader);
				append_token(reader, start_source_loc, TOK_BIT_XOR);
			}

			break;

		case '=':
			if (read_char(reader) == '=') {
				append_token(reader, start_source_loc, TOK_EQUAL);
			} else {
				back_up(reader);
				append_token(reader, start_source_loc, TOK_ASSIGN);
			}

			break;

		case '!':
			if (read_char(reader) == '=') {
				append_token(reader, start_source_loc, TOK_NOT_EQUAL);
			} else {
				back_up(reader);
				append_token(reader, start_source_loc, TOK_LOGICAL_NOT);
			}

			break;

		case '<':
			switch (read_char(reader)) {
			case '=':
				append_token(reader, start_source_loc, TOK_LESS_THAN_OR_EQUAL);
				break;
			case '<':
				if (read_char(reader) == '=') {
					append_token(reader, start_source_loc, TOK_LEFT_SHIFT_ASSIGN);
				} else {
					back_up(reader);
					append_token(reader, start_source_loc, TOK_LEFT_SHIFT);
				}
				break;
			default:
				back_up(reader);
				append_token(reader, start_source_loc, TOK_LESS_THAN);
			}

			break;

		case '>':
			switch (read_char(reader)) {
			case '=':
				append_token(reader, start_source_loc, TOK_GREATER_THAN_OR_EQUAL);
				break;
			case '>':
				if (read_char(reader) == '=') {
					append_token(reader, start_source_loc, TOK_RIGHT_SHIFT_ASSIGN);
				} else {
					back_up(reader);
					append_token(reader, start_source_loc, TOK_RIGHT_SHIFT);
				}
				break;
			default:
				back_up(reader);
				append_token(reader, start_source_loc, TOK_GREATER_THAN);
			}

			break;

		case '#':
			if (read_char(reader) == '#') {
				append_token(reader, start_source_loc, TOK_DOUBLE_HASH);
			} else {
				back_up(reader);

				if (!reader->first_token_of_line) {
					issue_error(&reader->source_loc,
							"Unexpected preprocessor directive");
					return false;
				} else if (!handle_pp_directive(reader)) {
					return false;
				}
			}

			break;

		case '.':
			if (read_char(reader) == '.') {
				if (read_char(reader) == '.') {
					append_token(reader, start_source_loc, TOK_ELLIPSIS);
				} else {
					back_up(reader);
					back_up(reader);
					append_token(reader, start_source_loc, TOK_DOT);
				}
			} else {
				back_up(reader);
				append_token(reader, start_source_loc, TOK_DOT);
			}

			break;

		case '~': append_token(reader, start_source_loc, TOK_BIT_NOT); break;
		case '?': append_token(reader, start_source_loc, TOK_QUESTION_MARK); break;
		case ':': append_token(reader, start_source_loc, TOK_COLON); break;
		case ';': append_token(reader, start_source_loc, TOK_SEMICOLON); break;
		case ',': append_token(reader, start_source_loc, TOK_COMMA); break;

		case '{': append_token(reader, start_source_loc, TOK_LCURLY); break;
		case '}': append_token(reader, start_source_loc, TOK_RCURLY); break;
		case '(': append_token(reader, start_source_loc, TOK_LROUND); break;
		case ')': append_token(reader, start_source_loc, TOK_RROUND); break;
		case '[': append_token(reader, start_source_loc, TOK_LSQUARE); break;
		case ']': append_token(reader, start_source_loc, TOK_RSQUARE); break;

		case '\n': case ' ': case '\t':
			// skip_whitespace_and_comments should have moved us past these
			UNREACHABLE;

		default: {
			char *symbol = read_symbol(reader);
			if (streq(symbol, "__LINE__")) {
				Token *line_number =
					append_token(reader, start_source_loc, TOK_INT_LITERAL);
				line_number->val.int_literal = reader->source_loc.line;
			} else if (streq(symbol, "__FILE__")) {
				Token *file_name =
					append_token(reader, start_source_loc, TOK_STRING_LITERAL);
				file_name->val.string_literal = reader->source_loc.filename;
			} else {
				Macro *macro = look_up_macro(&reader->macro_env, symbol);

				if (macro == NULL) {
					Token *token =
						append_token(reader, start_source_loc, TOK_SYMBOL);
					token->val.symbol = symbol;
				} else {
					if (macro->arg_names.size == 0) {
						if (!tokenise_string(reader, macro->value))
							return false;
					} else {
						skip_whitespace_and_comments(reader, true);
						if (peek_char(reader) != '(') {
							// This identifier names a function-like macro, but
							// it appears here without arguments. Therefore, we
							// leave it as is.
							Token *token =
								append_token(reader, start_source_loc, TOK_SYMBOL);
							token->val.symbol = symbol;
						} else {
							read_char(reader);
							substitute_macro_params(reader, macro);
							if (!tokenise_string(reader, macro->value))
								return false;

							// Remove the temporary macro values inserted for
							// the macro parameters.
							reader->macro_env.size -= macro->arg_names.size;
						}
					}
				}
			}

			break;
		}
		}

		reader->first_token_of_line = false;
	}

	return true;
}

// @TODO: This is probably too conservative - fopen can fail for other reasons.
static bool file_exists(char *path)
{
	FILE *f = fopen(path, "r");
	if (f != NULL)
		fclose(f);

	return f != NULL;
}

static char *concat(char *str_a, u32 len_a, char *str_b, u32 len_b)
{
	u32 result_length = len_a + len_b;
	char *result = malloc(result_length + 1);
	strncpy(result, str_a, len_a);
	strncpy(result + len_a, str_b, len_b);
	result[result_length] = '\0';

	return result;
}

static char *look_up_include_path(char *including_file, char *include_path)
{
	// If absolute, just try the exact path.
	if (include_path[0] == '/') {
		if (file_exists(include_path))
			return include_path;
		else
			return NULL;
	}

	// Try relative to the including file
	u32 including_file_length = strlen(including_file);
	i32 i = including_file_length - 1;
	for (; i >= 0 && including_file[i] != '/'; i--)
		;

	char *base_path;
	u32 base_length;
	// Path without any slashes
	if (i == -1) {
		base_path = "./";
		base_length = 2;
	} else {
		base_path = including_file;
		base_length = i + 1;
	}

	u32 include_path_length = strlen(include_path);
	char *potential_path = concat(
			base_path, base_length, include_path, include_path_length);
	if (file_exists(potential_path))
		return potential_path;
	free(potential_path);

	// Try system headers

	// @TODO: Here we assume that libc inclues live in "./libc". This obviously
	// doesn't work in general. We need to install our libc somewhere.
	base_path = "./libc/";
	base_length = 7;

	include_path_length = strlen(include_path);
	potential_path = concat(
			base_path, base_length, include_path, include_path_length);
	if (file_exists(potential_path))
		return potential_path;
	free(potential_path);

	// @TODO: Support -I flag.

	return NULL;
}

static bool handle_pp_directive(Reader *reader)
{
	IGNORE(reader);
	skip_whitespace_and_comments(reader, false);
	char c = read_char(reader);
	// Empty directive
	if (c == '\n')
		return true;

	if (!initial_ident_char(c)) {
		// @TODO: Sync to the next newline?
		issue_error(&reader->source_loc, "Expected preprocessor directive");
		return false;
	}

	char *directive = read_symbol(reader);
	if (streq(directive, "if")) {
		UNIMPLEMENTED;
	} else if (streq(directive, "ifdef")) {
		UNIMPLEMENTED;
	} else if (streq(directive, "ifndef")) {
		UNIMPLEMENTED;
	} else if (streq(directive, "elif")) {
		UNIMPLEMENTED;
	} else if (streq(directive, "else")) {
		UNIMPLEMENTED;
	} else if (streq(directive, "endif")) {
		UNIMPLEMENTED;
	} else if (streq(directive, "include")) {
		skip_whitespace_and_comments(reader, false);
		SourceLoc include_path_source_loc = reader->source_loc;

		char c = read_char(reader);
		if (c != '<' && c != '"') {
			// @TODO: Resync to newline?
			issue_error(&reader->source_loc, "Expected filename after #include");
			return false;
		}

		char terminator = c == '<' ? '>' : '"';
		u32 start_index = reader->position;
		while (read_char(reader) != terminator)
			;
		u32 end_index = reader->position - 1;
		u32 length = end_index - start_index;

		char *include_path = strndup(reader->buffer.buffer + start_index, length);
		char *includee_path = look_up_include_path(reader->source_loc.filename, include_path);

		if (includee_path == NULL) {
			issue_error(&include_path_source_loc,
					"File not found: '%s'", include_path);
			return false;
		}

		if (!tokenise_file(reader, includee_path, include_path_source_loc))
			return false;
		
		free(include_path);
		free(includee_path);

		skip_whitespace_and_comments(reader, false);
		if (read_char(reader) != '\n') {
			// @TODO: Resync to newline?
			issue_error(&reader->source_loc, "Extraneous text after include path");
		}
	} else if (streq(directive, "define")) {
		skip_whitespace_and_comments(reader, false);

		char c = read_char(reader);
		if (!initial_ident_char(c)) {
			issue_error(&reader->source_loc, "Expected identifier after #define");
			return false;
		}

		char *macro_name = read_symbol(reader);

		Array(char *) arg_names;
		ARRAY_INIT(&arg_names, char *, 0);
		if (peek_char(reader) == '(') {
			read_char(reader);
			for (;;) {
				skip_whitespace_and_comments(reader, false);
				char c = read_char(reader);
				if (c == '\n') {
					issue_error(&reader->source_loc,
							"Unexpected newline in macro argument list");
					return false;
				} else if (!initial_ident_char(c)) {
					issue_error(&reader->source_loc,
							"Unexpected charater while processing macro argument list");
					return false;
				} else {
					*ARRAY_APPEND(&arg_names, char *) = read_symbol(reader);
					skip_whitespace_and_comments(reader, false);
					char next = read_char(reader);
					if (next == ')') {
						break;
					} else if (next != ',') {
						issue_error(&reader->source_loc,
								"Expected comma after macro argument name");
						return false;
					}
				}
			}
		}

		skip_whitespace_and_comments(reader, false);
		Array(char) macro_value_chars;
		ARRAY_INIT(&macro_value_chars, char, 10);
		while ((c = read_char(reader)) != '\n')
			*ARRAY_APPEND(&macro_value_chars, char) = c;

		char *macro_value = strndup((char *)macro_value_chars.elements, macro_value_chars.size);
		array_free(&macro_value_chars);

		Macro *macro = look_up_macro(&reader->macro_env, macro_name);
		if (macro != NULL) {
			// @TODO: Proper checks as per C99 6.10.3.2
			assert(macro->arg_names.size == arg_names.size);
		} else {
			macro = ARRAY_APPEND(&reader->macro_env, Macro);
		}

		macro->name = macro_name;
		macro->value = macro_value;
		macro->arg_names = arg_names;
	} else if (streq(directive, "undef")) {
		UNIMPLEMENTED;
	} else if (streq(directive, "line")) {
		UNIMPLEMENTED;
	} else if (streq(directive, "error")) {
		UNIMPLEMENTED;
	} else if (streq(directive, "pragma")) {
		UNIMPLEMENTED;
	} else {
		issue_error(&reader->source_loc, "Invalid preprocessor directive: %s", directive);
		return false;
	}

	return true;
}


#define X(x) #x
char *token_type_names[] = {
	TOKEN_TYPES
};
#undef X

void dump_token(Token *token)
{
	fputs(token_type_names[token->type], stdout);
	switch (token->type) {
	case TOK_INT_LITERAL:
		printf("(%" PRIu64 ")", token->val.int_literal);
		break;
	case TOK_FLOAT_LITERAL:
		printf("(%lf)", token->val.float_literal);
		break;
	case TOK_STRING_LITERAL:
		// @TODO: Escape the resulting string
		printf("(\"%s\")", token->val.string_literal);
		break;
	case TOK_SYMBOL:
		printf("(%s)", token->val.symbol);
		break;
	default:
		break;
	}
}
