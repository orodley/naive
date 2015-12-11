#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "array.h"
#include "diagnostics.h"
#include "misc.h"
#include "parse.h"
#include "tokenise.h"

// TODO: Allocate everything from a pool. Then we can just free the whole pool
// afterwards.

typedef struct Reader
{
	Array *tokens;
	u32 index;
} Reader;

static Token *read_token(Reader *reader)
{
	Token *token = array_ref(reader->tokens, reader->index);
	reader->index++;

	return token;
}

static inline Token *current_token(Reader *reader)
{
	return array_ref(reader->tokens, reader->index);
}

static inline bool expect_token(Reader *reader, TokenType type)
{
	if (current_token(reader)->type == type) {
		read_token(reader);
		return true;
	}

	return false;
}

static inline bool expect_keyword(Reader *reader, const char *keyword)
{
	Token *token = read_token(reader);
	return (token->type == TOK_SYMBOL) &&
		(strcmp(token->val.symbol_or_string_literal, keyword) == 0);
}

static inline SourceLoc *token_context(Token *token)
{
	return &((SourceToken *)token)->source_loc;
}

static inline SourceLoc *reader_context(Reader *reader)
{
	return token_context(current_token(reader));
}


static ASTType *parse_type(Reader *reader)
{
	Token *token = read_token(reader);
	if (token->type == TOK_SYMBOL) {
		ASTType *type_spec = malloc(sizeof *type_spec);
		type_spec->name = token->val.symbol_or_string_literal;

		return type_spec;
	} else {
		issue_error(token_context(token), "Expected symbol");
		return NULL;
	}
}

static ASTExpression *parse_expr(Reader *reader)
{
	Token *token = read_token(reader);
	if (token->type != TOK_INT_LITERAL) {
		issue_error(token_context(token), "Expected integer literal");
		return NULL;
	}

	ASTExpression *expr = malloc(sizeof *expr);
	expr->type = AST_INTEGER_LITERAL;
	expr->val.integer_literal = token->val.int_literal;

	return expr;
}

static ASTStatement *parse_statement(Reader *reader)
{
	if (!expect_keyword(reader, "return")) {
		issue_error(reader_context(reader), "Expected 'return'");
		return NULL;
	}

	ASTExpression *expr = parse_expr(reader);
	if (expr == NULL)
		return NULL;

	if (!expect_token(reader, TOK_SEMICOLON)) {
		issue_error(reader_context(reader), "Expected ';'");
		return NULL;
	}

	ASTStatement *statement = malloc(sizeof *statement);
	statement->type = AST_RETURN_STATEMENT;
	statement->val.return_value = expr;

	return statement;
}

// The input array consists of SourceToken*s, but we treat them as Token*s most
// of the time.
ASTToplevel *parse_toplevel(Array *tokens)
{
	Reader _reader = { tokens, 0 };
	Reader *reader = &_reader;
	ASTToplevel *result = malloc(sizeof *result);
	result->type = AST_FUNCTION_DEF;

	ASTType *return_type = parse_type(reader);
	if (return_type == NULL)
		return NULL;
	result->val.function_def.return_type = return_type;

	Token *identifier = read_token(reader);
	if (identifier->type != TOK_SYMBOL) {
		issue_error(token_context(identifier), "Expected identifier");
		return NULL;
	}
	result->val.function_def.name = identifier->val.symbol_or_string_literal;

	if (!expect_token(reader, TOK_LROUND)) {
		issue_error(reader_context(reader), "Expected '('");
		return NULL;
	}

	Array *arguments = &result->val.function_def.arguments;
	array_init(arguments, sizeof(ASTVar *), 3);

	for (;;) {
		ASTType *arg_type = parse_type(reader);
		if (arg_type == NULL)
			return NULL;

		Token *identifier = read_token(reader);
		if (identifier->type != TOK_SYMBOL) {
			issue_error(token_context(identifier), "Expected identifier");
			return NULL;
		}

		ASTVar *argument = malloc(sizeof *argument);
		argument->name = identifier->val.symbol_or_string_literal;
		argument->type = arg_type;

		*(ASTVar **)array_append(arguments) = argument;

		Token *comma_or_close_bracket = read_token(reader);
		if (comma_or_close_bracket->type == TOK_RROUND)
			break;
		if (comma_or_close_bracket->type != TOK_COMMA) {
			issue_error(token_context(comma_or_close_bracket), "Expected ')'");
			return NULL;
		}
	}

	if (!expect_token(reader, TOK_LCURLY)) {
		issue_error(reader_context(reader), "Expected '{'");
		return NULL;
	}

	ASTStatement *body = malloc(sizeof *body);
	body->type = AST_COMPOUND_STATEMENT;
	Array *statements = &body->val.statements;
	array_init(statements, sizeof(ASTStatement *), 10);

	for (;;) {
		if (current_token(reader)->type == TOK_RCURLY)
			break;

		ASTStatement *statement = parse_statement(reader);
		if (statement == NULL)
			return NULL;

		*(ASTStatement **)array_append(statements) = statement;
	}

	result->val.function_def.body = body;

	return result;
}


static void dump_type(ASTType *type)
{
	fputs(type->name, stdout);
}

static void dump_expr(ASTExpression *expr)
{
	switch (expr->type) {
	case AST_INTEGER_LITERAL:
		printf("INTEGER_LITERAL(%" PRId64 ")", expr->val.integer_literal);
		break;
	}
}

static void dump_statement(ASTStatement *statement)
{
	switch (statement->type) {
	case AST_COMPOUND_STATEMENT:
		fputs("COMPOUND_STATEMENT(", stdout);
		Array *statements = &statement->val.statements;
		for (u32 i = 0; i < statements->size; i++) {
			dump_statement(*(ASTStatement **)array_ref(statements, i));
			fputs(i == statements->size - 1 ? "" : ", ", stdout);
		}

		fputs(")", stdout);

		break;

	case AST_RETURN_STATEMENT:
		fputs("RETURN_STATEMENT(", stdout);
		dump_expr(statement->val.return_value);
		fputs(")", stdout);

		break;
	}
}

void dump_toplevel(ASTToplevel *ast)
{
	switch (ast->type) {
	case AST_FUNCTION_DEF:
		fputs("FUNCTION_DEF(", stdout);
		dump_type(ast->val.function_def.return_type);
		printf(", %s, (", ast->val.function_def.name);

		Array *args = &ast->val.function_def.arguments;
		for (u32 i = 0; i < args->size; i++) {
			ASTVar *arg = *(ASTVar **)array_ref(args, i);
			dump_type(arg->type);
			printf(" %s%s", arg->name, i == args->size - 1 ? "" : ", ");
		}

		fputs("), ", stdout);
		dump_statement(ast->val.function_def.body);

		puts(")");

		break;
	}
}
