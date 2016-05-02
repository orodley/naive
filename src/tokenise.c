#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "array.h"
#include "diagnostics.h"
#include "misc.h"
#include "tokenise.h"

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

#define INVALID_INPUT_BUFFER ((InputBuffer) { NULL, 0 })

// @PORT
static InputBuffer map_file_into_memory(char *filename)
{
	int fd = open(filename, O_RDONLY);
	if (fd == -1)
		return INVALID_INPUT_BUFFER;

	off_t file_size = lseek(fd, 0, SEEK_END);

	if (file_size == -1)
		return INVALID_INPUT_BUFFER;

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

static inline bool is_valid(InputBuffer ib)
{
	return !((ib.buffer == INVALID_INPUT_BUFFER.buffer) &&
		(ib.length == INVALID_INPUT_BUFFER.length));
}


typedef struct Macro
{
	char *name;
	char *value;
} Macro;

static Macro *look_up_macro(Array(Macro) *macro_env, char *name)
{
	for (u32 i = 0; i < macro_env->size; i++) {
		Macro *m = ARRAY_REF(macro_env, Macro, i);
		if (strcmp(m->name, name) == 0) {
			return m;
		}
	}

	return NULL;
}

static void define_macro(Array(Macro) *macro_env, char *name, char *value)
{
	Macro *pre_existing = look_up_macro(macro_env, name);
	if (pre_existing != NULL) {
		pre_existing->value = value;
	} else {
		*ARRAY_APPEND(macro_env, Macro) = (Macro) { .name = name, .value = value};
	}
}

typedef struct Reader
{
	Array(SourceToken) *tokens;

	InputBuffer buffer;
	u32 position;

	SourceLoc source_loc;
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
	if (peek_char(reader) == '\n') {
		reader->source_loc.line++;
		reader->source_loc.column = 1;
	} else {
		reader->source_loc.column++;
	}

	reader->position++;
}

static inline void back_up(Reader *reader)
{
	// We should never need to back up over a newline. We assert that we don't
	// because if we did we'd need to keep track of the previous lines length
	// to preserve source information.
	assert(peek_char(reader) != '\n');

	reader->source_loc.column--;
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


static Token *append_token(Reader *reader, TokenType type)
{
	SourceToken *source_token = ARRAY_APPEND(reader->tokens, SourceToken);
	source_token->token.type = TOK_INVALID;
	source_token->source_loc = reader->source_loc;

	Token *token = (Token *)source_token;
	token->type = type;
	return token;
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

static void handle_pp_directive(Reader *reader);
static void tokenise_file(Reader *reader, char *input_filename);
static void tokenise_reader(Reader *reader);

// @IMPROVE: Replace with a finite state machine
void tokenise(Array(SourceToken) *tokens, char *input_filename)
{
	ARRAY_INIT(tokens, SourceToken, 500);

	Reader reader;
	reader.tokens = tokens;
	ARRAY_INIT(&reader.macro_env, Macro, 10);

	tokenise_file(&reader, input_filename);

	for (u32 i = 0; i < reader.macro_env.size; i++) {
		free(ARRAY_REF(&reader.macro_env, Macro, i)->name);
		free(ARRAY_REF(&reader.macro_env, Macro, i)->value);
	}
	array_free(&reader.macro_env);
}


static void tokenise_string(Reader *reader, char *string)
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

	tokenise_reader(reader);

	reader->position = old_position;
	reader->buffer = old_buffer;
	reader->source_loc = old_source_loc;
}

static void tokenise_file(Reader *reader, char *input_filename)
{
	u32 old_position = reader->position;
	SourceLoc old_source_loc = reader->source_loc;
	InputBuffer old_buffer = reader->buffer;

	InputBuffer buffer = map_file_into_memory(input_filename);
	if (!is_valid(buffer)) {
		issue_error(&reader->source_loc, "Failed to open input file: '%s'", input_filename);
		return;
	}

	reader->buffer = buffer;
	reader->position = 0;
	reader->source_loc = (SourceLoc) { input_filename, 1, 1, };
	reader->first_token_of_line = true;

	if (buffer.buffer == NULL)
		return;

	tokenise_reader(reader);

	unmap_file(buffer);

	reader->position = old_position;
	reader->buffer = old_buffer;
	reader->source_loc = old_source_loc;
}

static void tokenise_reader(Reader *reader)
{
	while (!at_end(reader)) {
		skip_whitespace_and_comments(reader, true);
		if (at_end(reader))
			break;

		switch (read_char(reader)) {
		case '0': case '1': case '2': case '3': case '4': case '5': case '6':
		case '7': case '8': case '9': {
			back_up(reader);
			i64 value = 0;

			for (;;) {
				char c = peek_char(reader);
				if (!(c >= '0' && c <= '9'))
					break;

				value *= 10;
				value += c - '0';
				advance(reader);
			}

			Token *token = append_token(reader, TOK_INT_LITERAL);
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

			Token *token = append_token(reader, TOK_STRING_LITERAL);
			token->val.symbol_or_string_literal = strndup(
					reader->buffer.buffer + start_index, length);

			break;
		}
		case '+':
			switch (read_char(reader)) {
			case '+':
				append_token(reader, TOK_INCREMENT);
				break;
			case '=':
				append_token(reader, TOK_PLUS_ASSIGN);
				break;
			default:
				back_up(reader);
				append_token(reader, TOK_PLUS);
			}

			break;

		case '-':
			switch (read_char(reader)) {
			case '-':
				append_token(reader, TOK_DECREMENT);
				break;
			case '=':
				append_token(reader, TOK_MINUS_ASSIGN);
				break;
			case '>':
				append_token(reader, TOK_ARROW);
				break;
			default:
				back_up(reader);
				append_token(reader, TOK_MINUS);
			}

			break;

		case '*':
			if (read_char(reader) == '=') {
				append_token(reader, TOK_MULT_ASSIGN);
			} else {
				back_up(reader);
				append_token(reader, TOK_ASTERISK);
			}

			break;

		case '/':
			if (read_char(reader) == '=') {
				append_token(reader, TOK_DIVIDE_ASSIGN);
			} else {
				back_up(reader);
				append_token(reader, TOK_DIVIDE);
			}

			break;

		case '%':
			if (read_char(reader) == '=') {
				append_token(reader, TOK_MODULO_ASSIGN);
			} else {
				back_up(reader);
				append_token(reader, TOK_MODULO);
			}

			break;

		case '&':
			switch (read_char(reader)) {
			case '&':
				append_token(reader, TOK_LOGICAL_AND);
				break;
			case '=':
				append_token(reader, TOK_BIT_AND_ASSIGN);
				break;
			default:
				back_up(reader);
				append_token(reader, TOK_AMPERSAND);
			}

			break;

		case '|':
			switch (read_char(reader)) {
			case '|':
				append_token(reader, TOK_LOGICAL_OR);
				break;
			case '=':
				append_token(reader, TOK_BIT_OR_ASSIGN);
				break;
			default:
				back_up(reader);
				append_token(reader, TOK_BIT_OR);
			}

			break;

		case '^':
			if (read_char(reader) == '=') {
				append_token(reader, TOK_BIT_XOR_ASSIGN);
			} else {
				back_up(reader);
				append_token(reader, TOK_BIT_XOR);
			}

			break;

		case '=':
			if (read_char(reader) == '=') {
				append_token(reader, TOK_EQUAL);
			} else {
				back_up(reader);
				append_token(reader, TOK_ASSIGN);
			}

			break;

		case '!':
			if (read_char(reader) == '=') {
				append_token(reader, TOK_NOT_EQUAL);
			} else {
				back_up(reader);
				append_token(reader, TOK_LOGICAL_NOT);
			}

			break;

		case '<':
			switch (read_char(reader)) {
			case '=':
				append_token(reader, TOK_LESS_THAN_OR_EQUAL);
				break;
			case '<':
				if (read_char(reader) == '=') {
					append_token(reader, TOK_LEFT_SHIFT_ASSIGN);
				} else {
					back_up(reader);
					append_token(reader, TOK_LEFT_SHIFT);
				}
				break;
			default:
				back_up(reader);
				append_token(reader, TOK_LESS_THAN);
			}

			break;

		case '>':
			switch (read_char(reader)) {
			case '=':
				append_token(reader, TOK_GREATER_THAN_OR_EQUAL);
				break;
			case '>':
				if (read_char(reader) == '=') {
					append_token(reader, TOK_RIGHT_SHIFT_ASSIGN);
				} else {
					back_up(reader);
					append_token(reader, TOK_RIGHT_SHIFT);
				}
				break;
			default:
				back_up(reader);
				append_token(reader, TOK_GREATER_THAN);
			}

			break;

		case '#':
			if (read_char(reader) == '#') {
				append_token(reader, TOK_DOUBLE_HASH);
			} else {
				back_up(reader);

				if (!reader->first_token_of_line) {
					issue_error(&reader->source_loc,
							"Unexpected preprocessor directive");
				} else {
					handle_pp_directive(reader);
				}
			}

			break;

		case '.':
			if (read_char(reader) == '.') {
				if (read_char(reader) == '.') {
					append_token(reader, TOK_ELLIPSIS);
				} else {
					back_up(reader);
					back_up(reader);
					append_token(reader, TOK_DOT);
				}
			} else {
				back_up(reader);
				append_token(reader, TOK_DOT);
			}

			break;

		case '~': append_token(reader, TOK_BIT_NOT); break;
		case '?': append_token(reader, TOK_QUESTION_MARK); break;
		case ':': append_token(reader, TOK_COLON); break;
		case ';': append_token(reader, TOK_SEMICOLON); break;
		case ',': append_token(reader, TOK_COMMA); break;

		case '{': append_token(reader, TOK_LCURLY); break;
		case '}': append_token(reader, TOK_RCURLY); break;
		case '(': append_token(reader, TOK_LROUND); break;
		case ')': append_token(reader, TOK_RROUND); break;
		case '[': append_token(reader, TOK_LSQUARE); break;
		case ']': append_token(reader, TOK_RSQUARE); break;

		case '\n': case ' ': case '\t':
			// skip_whitespace_and_comments should have moved us past these
			UNREACHABLE;

		default: {
			char *symbol = read_symbol(reader);
			Macro *macro = look_up_macro(&reader->macro_env, symbol);

			if (macro == NULL) {
				Token *token = append_token(reader, TOK_SYMBOL);
				token->val.symbol_or_string_literal = symbol;
			} else {
				tokenise_string(reader, macro->value);
			}
			break;
		}
		}

		reader->first_token_of_line = false;
	}
}


static char *look_up_include_path(char *including_file, char *include_path)
{
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
	u32 result_length = base_length + include_path_length;
	char *result = malloc(result_length + 1);
	strncpy(result, base_path, base_length);
	strncpy(result + base_length, include_path, include_path_length);
	result[result_length] = '\0';

	return result;
}

static void handle_pp_directive(Reader *reader)
{
	IGNORE(reader);
	skip_whitespace_and_comments(reader, false);
	char c = read_char(reader);
	// Empty directive
	if (c == '\n')
		return;

	if (!initial_ident_char(c)) {
		// @TODO: Sync to the next newline?
		issue_error(&reader->source_loc, "Expected preprocessor directive");
		return;
	}

	char *directive = read_symbol(reader);
	if (strcmp(directive, "if") == 0) {
		UNIMPLEMENTED;
	} else if (strcmp(directive, "ifdef") == 0) {
		UNIMPLEMENTED;
	} else if (strcmp(directive, "ifndef") == 0) {
		UNIMPLEMENTED;
	} else if (strcmp(directive, "elif") == 0) {
		UNIMPLEMENTED;
	} else if (strcmp(directive, "else") == 0) {
		UNIMPLEMENTED;
	} else if (strcmp(directive, "endif") == 0) {
		UNIMPLEMENTED;
	} else if (strcmp(directive, "include") == 0) {
		skip_whitespace_and_comments(reader, false);

		char c = read_char(reader);
		if (c != '<' && c != '"') {
			// @TODO: Resync to newline?
			issue_error(&reader->source_loc, "Expected filename after #include");
			return;
		}

		char terminator = c == '<' ? '>' : '"';
		u32 start_index = reader->position;
		while (read_char(reader) != terminator)
			;
		u32 end_index = reader->position - 1;
		u32 length = end_index - start_index;

		char *include_path = strndup(reader->buffer.buffer + start_index, length);
		char *includee_path = look_up_include_path(reader->source_loc.filename, include_path);

		tokenise_file(reader, includee_path);
		
		free(include_path);
		free(includee_path);

		skip_whitespace_and_comments(reader, false);
		if (read_char(reader) != '\n') {
			// @TODO: Resync to newline?
			issue_error(&reader->source_loc, "Extraneous text after include path");
		}
	} else if (strcmp(directive, "define") == 0) {
		skip_whitespace_and_comments(reader, false);

		char c = read_char(reader);
		if (!initial_ident_char(c)) {
			issue_error(&reader->source_loc, "Expected identifier after #define");
			return;
		}

		char *macro_name = read_symbol(reader);

		skip_whitespace_and_comments(reader, false);
		Array(char) macro_value_chars;
		ARRAY_INIT(&macro_value_chars, char, 10);
		while ((c = read_char(reader)) != '\n')
			*ARRAY_APPEND(&macro_value_chars, char) = c;

		char *macro_value = strndup((char *)macro_value_chars.elements, macro_value_chars.size);
		array_free(&macro_value_chars);

		define_macro(&reader->macro_env, macro_name, macro_value);
	} else if (strcmp(directive, "undef") == 0) {
		UNIMPLEMENTED;
	} else if (strcmp(directive, "line") == 0) {
		UNIMPLEMENTED;
	} else if (strcmp(directive, "error") == 0) {
		UNIMPLEMENTED;
	} else if (strcmp(directive, "pragma") == 0) {
		UNIMPLEMENTED;
	} else {
		issue_error(&reader->source_loc, "Invalid preprocessor directive: %s", directive);
	}
}


#define X(x) #x
const char *token_type_names[] = {
	TOKEN_TYPES
};
#undef X

void dump_token(Token *token)
{
	fputs(token_type_names[token->type], stdout);
	switch (token->type) {
	case TOK_INT_LITERAL:
		printf("(%" PRId64 ")", token->val.int_literal);
		break;
	case TOK_FLOAT_LITERAL:
		printf("(%lf)", token->val.float_literal);
		break;
	case TOK_STRING_LITERAL:
		// @TODO: Escape the resulting string
		printf("(\"%s\")", token->val.symbol_or_string_literal);
		break;
	case TOK_SYMBOL:
		printf("(%s)", token->val.symbol_or_string_literal);
		break;
	default:
		break;
	}
}
