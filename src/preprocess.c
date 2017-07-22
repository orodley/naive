#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>

// @PORT
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/mman.h>

#include "array.h"
#include "diagnostics.h"
#include "reader.h"
#include "util.h"

// @PORT
static String map_file_into_memory(char *filename)
{
	int fd = open(filename, O_RDONLY);
	if (fd == -1)
		return INVALID_STRING;

	off_t file_size = lseek(fd, 0, SEEK_END);

	if (file_size == -1)
		return INVALID_STRING;

	if (file_size == 0)
		return EMPTY_STRING;

	char *buffer = mmap(NULL, file_size, PROT_READ, MAP_PRIVATE, fd, 0);
	if (buffer == MAP_FAILED)
		return INVALID_STRING;

	close(fd);

	return (String) { buffer, file_size };
}

// @PORT
static void unmap_file(String buffer)
{
	int ret = munmap(buffer.chars, buffer.len);
	assert(ret == 0);
}

typedef struct Macro
{
	String name;
	char *value;
	Array(String) arg_names;
} Macro;

static Macro *look_up_macro(Array(Macro) *macro_env, String name)
{
	for (u32 i = 0; i < macro_env->size; i++) {
		Macro *m = ARRAY_REF(macro_env, Macro, i);
		if (m->name.len == name.len
				&& strneq(m->name.chars, name.chars, name.len)) {
			return m;
		}
	}

	return NULL;
}

typedef struct PPCondScope
{
	bool condition;
	enum { THEN, ELSE } position;
} PPCondScope;

typedef struct PP
{
	Reader reader;

	Array(char) out_chars;
	Array(Adjustment) out_adjustments;
	u32 macro_depth;

	Array(InputBuffer) mapped_files;

	Array(char *) *include_dirs;
	Array(PPCondScope) pp_scope_stack;
	Array(Macro) macro_env;
	Array(Macro) curr_macro_params;
} PP;

static bool preprocess_string(PP *pp, char *string);
static char *macroexpand(PP *pp, char *string);
static bool preprocess_aux(PP *pp);

static void start_pp_if(PP *pp, bool condition)
{
	Array(PPCondScope) *stack = &pp->pp_scope_stack;
	if (stack->size >= 1 && !ARRAY_LAST(stack, PPCondScope)->condition) {
		condition = false;
	}

	*ARRAY_APPEND(stack, PPCondScope) = (PPCondScope) {
		.condition = condition,
		.position = THEN,
	};
}

static bool ignoring_chars(PP *pp)
{
	Array(PPCondScope) *pp_scope_stack = &pp->pp_scope_stack;
	if (pp_scope_stack->size == 0)
		return false;

	return !ARRAY_LAST(pp_scope_stack, PPCondScope)->condition;
}

static void add_adjustment_to(PP *pp, AdjustmentType type, SourceLoc source_loc)
{
	u32 location = pp->out_chars.size;
	Adjustment *adjustment;

	// Don't bother tracking nested BEGIN_MACRO's - we just want to report all
	// tokens as being at the location of the top-level macro. Just keep track
	// of the depth so we know when we're back to the top-level one.
	switch (type) {
	case BEGIN_MACRO_ADJUSTMENT:
		pp->macro_depth++;
		if (pp->macro_depth != 1)
			return;

		break;
	case END_MACRO_ADJUSTMENT:
		assert(pp->macro_depth != 0);
		pp->macro_depth--;
		if (pp->macro_depth != 0)
			return;

		break;
	case NORMAL_ADJUSTMENT:
		break;
	}

	if (pp->out_adjustments.size == 0) {
		adjustment = ARRAY_APPEND(&pp->out_adjustments, Adjustment);
	} else {
		Adjustment *prev = ARRAY_LAST(&pp->out_adjustments, Adjustment);

		// Again, we don't bother with any adjustments inside macros since
		// everything is reported at the top-level macro location.
		if (prev->type == BEGIN_MACRO_ADJUSTMENT && type == NORMAL_ADJUSTMENT) {
			return;
		}

		if (prev->location == location) {
			adjustment = prev;

			// If we have a normal adjustment directly after an END_MACRO, we
			// don't want to remove the END_MACRO, even if they're at the same
			// location.
			if (prev->type == END_MACRO_ADJUSTMENT && type == NORMAL_ADJUSTMENT)
				type = END_MACRO_ADJUSTMENT;
		} else {
			adjustment = ARRAY_APPEND(&pp->out_adjustments, Adjustment);
		}
	}

	*adjustment = (Adjustment) {
		.type = type,
		.location = location,
		.new_source_loc = source_loc,
	};
}

static void add_adjustment(PP *pp, AdjustmentType type)
{
	add_adjustment_to(pp, type, pp->reader.source_loc);
}

static void skip_whitespace_and_comments(PP *pp, bool skip_newline)
{
	Reader *reader = &pp->reader;
	while (!at_end(reader)) {
		switch (peek_char(reader)) {
		case '\n':
			if (!skip_newline)
				return;

			advance(reader);

			// Retain any newlines - we need them in the output so that the
			// tokeniser can correctly track source location without needing
			// an adjustment for every single line in the source.
			*ARRAY_APPEND(&pp->out_chars, char) = '\n';

			break;
		case ' ': case '\t':
			advance(reader);
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
	}
}

static bool append_string_or_char_literal(Reader *reader, char start_char,
		Array(char) *out_chars)
{
	u32 literal_start = reader->position - 1;
	SourceLoc literal_start_source_loc = reader->source_loc;
	while (peek_char(reader) != start_char) {
		if (at_end(reader)) {
			issue_error(&literal_start_source_loc,
					start_char == '"'
						? "Unterminated string literal"
						: "Unterminated character literal");
			return false;
		}
		if (peek_char(reader) == '\\') {
			advance(reader);
		}

		advance(reader);
	}
	advance(reader);

	ARRAY_APPEND_ELEMS(out_chars, char,
			reader->position - literal_start,
			reader->buffer.chars + literal_start);
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
	memcpy(result, str_a, len_a);
	memcpy(result + len_a, str_b, len_b);
	result[result_length] = '\0';

	return result;
}

static char *look_up_include_path(Array(char *) *include_dirs,
		char *including_file, char *include_path)
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

	// Try include dirs
	for (u32 i = 0; i < include_dirs->size; i++) {
		base_path = *ARRAY_REF(include_dirs, char *, i);
		base_length = strlen(base_path);

		include_path_length = strlen(include_path);
		potential_path = concat(
				base_path, base_length, include_path, include_path_length);
		if (file_exists(potential_path))
			return potential_path;
		free(potential_path);
	}

	// @TODO: Support -I flag.

	return NULL;
}

static bool preprocess_file(PP *pp, char *input_filename,
		SourceLoc blame_source_loc);

static bool handle_pp_directive(PP *pp)
{
	Reader *reader = &pp->reader;

	skip_whitespace_and_comments(pp, false);
	// Empty directive
	if (peek_char(reader) == '\n') {
		advance(reader);
		return true;
	}

	SourceLoc directive_start = reader->source_loc;

	String directive = read_symbol(reader);
	if (!is_valid(directive)) {
		// @TODO: Sync to the next newline?
		issue_error(&reader->source_loc, "Expected preprocessor directive");
		return false;
	}

	// Process #if and friends even if we're currently ignoring tokens.
	if (strneq(directive.chars, "if", directive.len)) {
		skip_whitespace_and_comments(pp, false);

		Array(char) condition_chars;
		ARRAY_INIT(&condition_chars, char, 10);
		while (peek_char(reader) != '\n')
			*ARRAY_APPEND(&condition_chars, char) = read_char(reader);
		*ARRAY_APPEND(&condition_chars, char) = '\0';

		char *condition_str = macroexpand(pp, (char *)condition_chars.elements);
		if (condition_str == NULL)
			return false;

		array_free(&condition_chars);

		bool cond;
		// We do this so that we can skip stuff like #if __has_feature(...)
		// when it's guarded by #ifdef __has_feature.
		// @TODO: We don't even need to allocate condition_chars in this case.
		if (ignoring_chars(pp)) {
			cond = false;
		} else {
			// For now we only handle #if 0 and #if 1
			if (streq(condition_str, "0")) {
				cond = false;
			} else if (streq(condition_str, "1")) {
				cond = true;
			} else {
				UNIMPLEMENTED;
			}
		}

		start_pp_if(pp, cond);
	} else if (strneq(directive.chars, "ifdef", directive.len)) {
		skip_whitespace_and_comments(pp, false);
		String macro_name = read_symbol(reader);
		if (!is_valid(macro_name)) {
			issue_error(&reader->source_loc, "Expected identifier after #ifdef");
			return false;
		}

		bool condition = look_up_macro(&pp->macro_env, macro_name) != NULL;
		start_pp_if(pp, condition);
	} else if (strneq(directive.chars, "ifndef", directive.len)) {
		skip_whitespace_and_comments(pp, false);
		String macro_name = read_symbol(reader);
		if (!is_valid(macro_name)) {
			issue_error(&reader->source_loc, "Expected identifier after #ifndef");
			return false;
		}

		bool condition = look_up_macro(&pp->macro_env, macro_name) == NULL;
		start_pp_if(pp, condition);
	} else if (strneq(directive.chars, "elif", directive.len)) {
		UNIMPLEMENTED;
	} else if (strneq(directive.chars, "else", directive.len)) {
		PPCondScope *scope = ARRAY_LAST(&pp->pp_scope_stack, PPCondScope);
		if (scope->position == ELSE) {
			issue_error(&directive_start,
					"Duplicate #else clause for preprocessor conditional");
			return false;
		}

		scope->position = ELSE;
		scope->condition = !scope->condition;
		if (pp->pp_scope_stack.size >= 2) {
			PPCondScope *parent_scope = ARRAY_REF(
					&pp->pp_scope_stack,
					PPCondScope,
					pp->pp_scope_stack.size - 2);
			if (!parent_scope->condition)
				scope->condition = false;
		}
	} else if (strneq(directive.chars, "endif", directive.len)) {
		if (pp->pp_scope_stack.size == 0) {
			issue_error(&directive_start, "Unmatched #endif");
			return false;
		}
		pp->pp_scope_stack.size--;
	} else if (!ignoring_chars(pp)) {
		if (strneq(directive.chars, "include", directive.len)) {
			skip_whitespace_and_comments(pp, false);
			SourceLoc include_path_source_loc = reader->source_loc;

			char c = read_char(reader);
			if (c != '<' && c != '"') {
				// @TODO: Resync to newline?
				issue_error(&reader->source_loc,
						"Expected filename after #include");
				return false;
			}

			char terminator = c == '<' ? '>' : '"';
			u32 start_index = reader->position;
			while (!at_end(reader) && read_char(reader) != terminator)
				;

			if (at_end(reader)) {
				issue_error(&include_path_source_loc, "Unterminated include path");
				return false;
			}

			u32 end_index = reader->position - 1;
			u32 length = end_index - start_index;

			char *include_path =
				strndup(reader->buffer.chars + start_index, length);
			char *includee_path = look_up_include_path(pp->include_dirs,
					reader->source_loc.filename, include_path);

			if (includee_path == NULL) {
				issue_error(&include_path_source_loc,
						"File not found: '%s'", include_path);
				return false;
			}

			// @LEAK: We leak includee_path because it gets attached to
			// SourceLoc's on tokens created by tokenise_file. Given how little
			// data this should take up in a given compilation this is probably
			// fine.

			bool success =
				preprocess_file(pp, includee_path, include_path_source_loc);

			if (include_path != includee_path)
				free(include_path);

			if (!success)
				return false;

			skip_whitespace_and_comments(pp, false);
		} else if (strneq(directive.chars, "define", directive.len)) {
			skip_whitespace_and_comments(pp, false);

			String macro_name = read_symbol(reader);
			if (!is_valid(macro_name)) {
				issue_error(&reader->source_loc,
						"Expected identifier after #define");
				return false;
			}

			Array(String) arg_names;
			ARRAY_INIT(&arg_names, String, 0);
			if (peek_char(reader) == '(') {
				advance(reader);
				for (;;) {
					skip_whitespace_and_comments(pp, false);
					char c = peek_char(reader);
					if (c == '\n') {
						issue_error(&reader->source_loc,
								"Unexpected newline in macro definition argument list");
						return false;
					} else {
						String arg_name = read_symbol(reader);
						if (!is_valid(arg_name)) {
							issue_error(&reader->source_loc,
									"Unexpected charater while processing "
									"macro argument list");
							return false;
						}

						*ARRAY_APPEND(&arg_names, String) = arg_name;
						skip_whitespace_and_comments(pp, false);
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

			skip_whitespace_and_comments(pp, false);
			Array(char) macro_value_chars;
			ARRAY_INIT(&macro_value_chars, char, 10);
			while (peek_char(reader) != '\n')
				*ARRAY_APPEND(&macro_value_chars, char) = read_char(reader);

			char *macro_value = strndup((char *)macro_value_chars.elements,
					macro_value_chars.size);
			array_free(&macro_value_chars);

			Macro *macro = look_up_macro(&pp->macro_env, macro_name);
			if (macro != NULL) {
				// @TODO: Proper checks as per C99 6.10.3.2
				assert(macro->arg_names.size == arg_names.size);
			} else {
				macro = ARRAY_APPEND(&pp->macro_env, Macro);
			}

			macro->name = macro_name;
			macro->value = macro_value;
			macro->arg_names = arg_names;
		} else if (strneq(directive.chars, "undef", directive.len)) {
			skip_whitespace_and_comments(pp, false);

			String macro_name = read_symbol(reader);
			if (!is_valid(macro_name)) {
				issue_error(&reader->source_loc,
						"Expected identifier after #undef");
				return false;
			}

			// @NOTE: #undef on an undefined macro is allowed (C99 6.10.3.5.2)
			for (u32 i = 0; i < pp->macro_env.size; i++) {
				Macro *m = ARRAY_REF(&pp->macro_env, Macro, i);
				if (m->name.len == macro_name.len
						&& strneq(m->name.chars, macro_name.chars, macro_name.len)) {
					ARRAY_REMOVE(&pp->macro_env, Macro, i);
					break;
				}
			}

			skip_whitespace_and_comments(pp, false);
		} else if (strneq(directive.chars, "line", directive.len)) {
			UNIMPLEMENTED;
		} else if (strneq(directive.chars, "error", directive.len)) {
			advance(reader);

			u32 start = reader->position;
			while (peek_char(reader) != '\n')
				advance(reader);

			u32 length = reader->position - start;
			char *error = strndup(reader->buffer.chars + start, length);
			issue_error(&directive_start, error);
			return false;
		} else if (strneq(directive.chars, "pragma", directive.len)) {
			UNIMPLEMENTED;
		} else {
			issue_error(&reader->source_loc,
					"Invalid preprocessor directive: %s", directive);
			return false;
		}
	}

	skip_whitespace_and_comments(pp, false);

	if (!ignoring_chars(pp) && peek_char(reader) != '\n') {
		issue_error(&reader->source_loc,
				"Extraneous text after preprocessing directive");
		return false;
	}

	// Advance past the newline, otherwise it will get added to out_chars by
	// skip_whitespace_and_comments.
	advance(reader);
	add_adjustment(pp, NORMAL_ADJUSTMENT);

	return true;
}

static bool substitute_macro_params(PP *pp, Macro *macro)
{
	bool ret;
	Reader *reader = &pp->reader;
	SourceLoc start_source_loc = pp->reader.source_loc;

	Array(Macro) new_macro_params = EMPTY_ARRAY;

	Array(char) arg_chars;
	ARRAY_INIT(&arg_chars, char, 20);
	u32 args_processed = 0;
	while (!at_end(reader)) {
		char c = read_char(reader);
		switch (c) {
		case '(': {
			u32 bracket_depth = 1;
			*ARRAY_APPEND(&arg_chars, char) = c;

			while (bracket_depth != 0) {
				char c = read_char(reader);
				switch (c) {
				case '(': bracket_depth++; break;
				case ')': bracket_depth--; break;
				}

				// @TODO: Append all at once with ARRAY_APPEND_ELEMS instead.
				*ARRAY_APPEND(&arg_chars, char) = c;
			}
			break;
		}
		case '"': case '\'':
			if (!append_string_or_char_literal(reader, c, &arg_chars))
				return false;

			break;
		case ')': case ',': {
			if (args_processed == macro->arg_names.size) {
				issue_error(&reader->source_loc,
						"Too many parameters to function-like macro"
						" (expected %u)",
						macro->arg_names.size);
				ret = false;
				goto cleanup;
			}
			Macro *arg_macro = ARRAY_APPEND(&new_macro_params, Macro);
			arg_macro->name = *ARRAY_REF(&macro->arg_names, String, args_processed);

			char *macro_value = strndup((char *)arg_chars.elements, arg_chars.size); 
			arg_macro->value = macroexpand(pp, macro_value);
			free(macro_value);

			if (arg_macro->value == NULL) {
				ret = false;
				goto cleanup;
			}

			arg_macro->arg_names = EMPTY_ARRAY;
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
			} else {
				skip_whitespace_and_comments(pp, false);
			}
			break;
		}
		default:
			*ARRAY_APPEND(&arg_chars, char) = c;
		}
	}

	issue_error(&start_source_loc, "Unterminated macro-like function invocation");
	ret = false;

cleanup:
	if (ret)
		pp->curr_macro_params = new_macro_params;

	array_free(&arg_chars);
	return ret;
}

static bool preprocess_file(PP *pp, char *input_filename,
		SourceLoc blame_source_loc)
{
	String buffer = map_file_into_memory(input_filename);
	if (!is_valid(buffer)) {
		if (blame_source_loc.filename == NULL) {
			fprintf(stderr, "Failed to open input file: '%s'\n", input_filename);
		} else {
			issue_error(&blame_source_loc, "File not found: '%s'\n", input_filename);
		}

		return false;
	}
	*ARRAY_APPEND(&pp->mapped_files, String) = buffer;

	Reader old_reader = pp->reader;

	reader_init(&pp->reader, buffer, EMPTY_ARRAY, true, input_filename);
	add_adjustment(pp, NORMAL_ADJUSTMENT);

	bool ret = preprocess_aux(pp);

	pp->reader = old_reader;
	return ret;
}

static bool preprocess_string(PP *pp, char *string)
{
	Reader old_reader = pp->reader;
	Array(Adjustment) old_adjustments = pp->out_adjustments;
	pp->out_adjustments = EMPTY_ARRAY;

	reader_init(&pp->reader,
			(String) { string, strlen(string) }, EMPTY_ARRAY, false, "??");

	bool ret = preprocess_aux(pp);

	pp->reader = old_reader;
	array_free(&pp->out_adjustments);
	pp->out_adjustments = old_adjustments;

	return ret;
}

// Convenience wrapper around preprocess_string that doesn't append to the
// existing buffer, but instead just returns the string.
static char *macroexpand(PP *pp, char *string)
{
	Array(char) old_out_chars = pp->out_chars;
	pp->out_chars = EMPTY_ARRAY;

	if (!preprocess_string(pp, string)) {
		array_free(&pp->out_chars);
		return NULL;
	}

	*ARRAY_APPEND(&pp->out_chars, char) = '\0';

	char *expanded = (char *)pp->out_chars.elements;
	pp->out_chars = old_out_chars;

	return expanded;
}

static bool preprocess_aux(PP *pp)
{
	Reader *reader = &pp->reader;
	String *buffer = &pp->reader.buffer;

	ARRAY_ENSURE_ROOM(&pp->out_chars, char, buffer->len);

	while (!at_end(reader)) {
		u32 start_input_position = reader->position;
		u32 start_output_position = pp->out_chars.size;

		skip_whitespace_and_comments(pp, true);

		u32 end_input_position = reader->position;
		u32 end_output_position = pp->out_chars.size;

		if (end_input_position - start_input_position
				> end_output_position - start_output_position) {
			// We skipped some chars, but did we skip any newlines? If so
			// they've been placed in the output buffer, and we don't need a
			// token separator. Otherwise we need to append a space.
			if (start_output_position == end_output_position) {
				*ARRAY_APPEND(&pp->out_chars, char) = ' ';
			}

			add_adjustment(pp, NORMAL_ADJUSTMENT);
		}

		SourceLoc start_source_loc = reader->source_loc;
		bool at_start_of_line = reader->at_start_of_line;

		char c = read_char(reader);
		switch (c) {
		// We need to handle string and character literals here so that we
		// don't expand macros inside them.
		case '\'': case '"': {
			if (ignoring_chars(pp))
				break;
			if (!append_string_or_char_literal(reader, c, &pp->out_chars))
				return false;

			break;
		}
		case '#':
			if (peek_char(reader) == '#') {
				// Token pasting operator
				// @TODO: Handle empty replacements properly.
				// @TODO: Handle the case where '##' is formed out of pasting
				// other tokens together. In this case it doesn't invoke the
				// token pasting operator.
				advance(reader);
				if (pp->macro_depth == 0) {
					issue_error(&start_source_loc,
							"The '##' operator is only allowed in a macro definition");
					return false;
				}

				// The only whitespace we should only ever be preceded by is at
				// most one ' '. It's only valid in a macro definition, so it
				// can't contain newlines, and skip_comments_and_whitespace
				// will produce at most one space.
				if (pp->out_chars.size >= 1) {
					char prev_char = *ARRAY_LAST(&pp->out_chars, char);
					if (prev_char == ' ') {
						pp->out_chars.size--;
					}
				}

				skip_whitespace_and_comments(pp, false);
			} else if (at_start_of_line) {
				if (!handle_pp_directive(pp))
					return false;
			} else if (pp->macro_depth == 0) {
				issue_error(&start_source_loc,
						"Unexpected preprocessor directive (not at start of line)");
				return false;
			} else {
				// Stringification operator
				// @TODO: Handle whitespace between preprocessing tokens.
				// @TODO: Handle whitespace at the start or end of the
				//        replacement text.
				skip_whitespace_and_comments(pp, false);

				SourceLoc expected_arg_name_source_loc = reader->source_loc;
				String arg_name = read_symbol(reader);
				
				if (!is_valid(arg_name)) {
					issue_error(&expected_arg_name_source_loc,
							"Expected identifier after '#' operator");
					return false;
				}

				Macro *macro = look_up_macro(&pp->curr_macro_params, arg_name);
				if (macro == NULL) {
					issue_error(&expected_arg_name_source_loc,
							"Argument to '#' does not name a macro parameter");
					return false;
				}

				u32 macro_value_len = strlen(macro->value);

				*ARRAY_APPEND(&pp->out_chars, char) = '"';
				for (u32 i = 0; i < macro_value_len; i++) {
					char c = macro->value[i];
					if (c == '"') {
						*ARRAY_APPEND(&pp->out_chars, char) = '\\';
					}

					*ARRAY_APPEND(&pp->out_chars, char) = c;
				}
				*ARRAY_APPEND(&pp->out_chars, char) = '"';
			}

			break;
		case EOF:
			break;
		default:
			if (ignoring_chars(pp))
				break;

			if (initial_ident_char(c)) {
				u32 symbol_start = reader->position - 1;
				while (ident_char(peek_char(reader)))
					read_char(reader);

				String symbol = (String) {
					buffer->chars + symbol_start,
					reader->position - symbol_start,
				};

				Macro *macro = look_up_macro(&pp->curr_macro_params, symbol);
				if (macro == NULL)
					macro = look_up_macro(&pp->macro_env, symbol);

				if (macro == NULL) {
					ARRAY_APPEND_ELEMS(&pp->out_chars, char,
							symbol.len, symbol.chars);
				} else if (macro->arg_names.size == 0) {
					Array(Macro) old_params = pp->curr_macro_params;
					pp->curr_macro_params = EMPTY_ARRAY;
					add_adjustment_to(pp, BEGIN_MACRO_ADJUSTMENT, start_source_loc);

					if (!preprocess_string(pp, macro->value))
						return false;
					pp->curr_macro_params = old_params;

					add_adjustment(pp, END_MACRO_ADJUSTMENT);
				} else {
					skip_whitespace_and_comments(pp, true);
					if (peek_char(reader) != '(') {
						// This identifier names a function-like macro, but it
						// appears here without arguments. Therefore, we leave
						// it as is.
						ARRAY_APPEND_ELEMS(&pp->out_chars, char, symbol.len, symbol.chars);
					} else {
						read_char(reader);

						Array(Macro) old_params = pp->curr_macro_params;
						add_adjustment_to(pp, BEGIN_MACRO_ADJUSTMENT, start_source_loc);
						if (!substitute_macro_params(pp, macro))
							return false;

						if (!preprocess_string(pp, macro->value))
							return false;

						array_free(&pp->curr_macro_params);
						pp->curr_macro_params = old_params;

						// Add a space to separate tokens if necessary.
						char next_char = peek_char(reader);
						if (next_char != ' ' && next_char != '\n'
								&& next_char != EOF) {
							*ARRAY_APPEND(&pp->out_chars, char) = ' ';
						}

						add_adjustment(pp, END_MACRO_ADJUSTMENT);
					}
				}
			} else {
				*ARRAY_APPEND(&pp->out_chars, char) = c;
			}

			break;
		}
	}

	return true;
}

bool preprocess(char *input_filename, Array(char *) *include_dirs,
		Array(char) *preprocessed, Array(Adjustment) *adjustments)
{
	PP pp = {
		.out_chars = EMPTY_ARRAY,
		.out_adjustments = EMPTY_ARRAY,
		.macro_depth = 0,
		.mapped_files = EMPTY_ARRAY,
		.include_dirs = include_dirs,
		.pp_scope_stack = EMPTY_ARRAY,
		.macro_env = EMPTY_ARRAY,
		.curr_macro_params = EMPTY_ARRAY,
	};

	bool ret = preprocess_file(&pp, input_filename, (SourceLoc) { NULL, 0, 0 });

	for (u32 i = 0; i < pp.mapped_files.size; i++) {
		String *buffer = ARRAY_REF(&pp.mapped_files, String, i);
		unmap_file(*buffer);
	}
	array_free(&pp.mapped_files);
	array_free(&pp.pp_scope_stack);
	for (u32 i = 0; i < pp.macro_env.size; i++) {
		Macro *macro = ARRAY_REF(&pp.macro_env, Macro, i);
		array_free(&macro->arg_names);
	}
	array_free(&pp.macro_env);
	array_free(&pp.curr_macro_params);

	*preprocessed = pp.out_chars;
	*adjustments = pp.out_adjustments;

	return ret;
}
