#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "array.h"
#include "diagnostics.h"
#include "misc.h"
#include "parse.h"
#include "pool.h"
#include "tokenise.h"

// @TODO: Allocate everything from a pool. Then we can just free the whole pool
// afterwards.

typedef struct Parser
{
	Array(SourceToken) *tokens;
	u32 index;

	Pool *pool;
} Parser;

static Token *read_token(Parser *parser)
{
	SourceToken *token = ARRAY_REF(parser->tokens, SourceToken, parser->index);
	parser->index++;

	return (Token *)token;
}

static inline Token *current_token(Parser *parser)
{
	return (Token *)ARRAY_REF(parser->tokens, SourceToken, parser->index);
}

static inline bool expect_token(Parser *parser, TokenType type)
{
	if (current_token(parser)->type == type) {
		read_token(parser);
		return true;
	}

	return false;
}

static inline bool expect_keyword(Parser *parser, const char *keyword)
{
	Token *token = read_token(parser);
	return (token->type == TOK_SYMBOL) &&
		(strcmp(token->val.symbol_or_string_literal, keyword) == 0);
}

static inline SourceLoc *token_context(Token *token)
{
	return &((SourceToken *)token)->source_loc;
}

static inline SourceLoc *parser_context(Parser *parser)
{
	return token_context(current_token(parser));
}


static ASTType *parse_type(Parser *parser)
{
	Token *token = read_token(parser);
	if (token->type == TOK_SYMBOL) {
		ASTType *type_spec = pool_alloc(parser->pool, sizeof *type_spec);
		type_spec->name = token->val.symbol_or_string_literal;

		return type_spec;
	} else {
		issue_error(token_context(token), "Expected symbol");
		return NULL;
	}
}

static ASTExpression *parse_expr(Parser *parser)
{
	Token *token = read_token(parser);
	if (token->type != TOK_INT_LITERAL) {
		issue_error(token_context(token), "Expected integer literal");
		return NULL;
	}

	ASTExpression *expr = pool_alloc(parser->pool, sizeof *expr);
	expr->type = AST_INTEGER_LITERAL;
	expr->val.integer_literal = token->val.int_literal;

	return expr;
}

static ASTStatement *parse_statement(Parser *parser)
{
	if (!expect_keyword(parser, "return")) {
		issue_error(parser_context(parser), "Expected 'return'");
		return NULL;
	}

	ASTExpression *expr = parse_expr(parser);
	if (expr == NULL)
		return NULL;

	if (!expect_token(parser, TOK_SEMICOLON)) {
		issue_error(parser_context(parser), "Expected ';'");
		return NULL;
	}

	ASTStatement *statement = pool_alloc(parser->pool, sizeof *statement);
	statement->type = AST_RETURN_STATEMENT;
	statement->val.return_value = expr;

	return statement;
}

// The input array consists of SourceTokens, but we treat them as Tokens most
// of the time.
ASTToplevel *parse_toplevel(Array(SourceToken) *tokens, Pool *ast_pool)
{
	Parser _parser = { tokens, 0, ast_pool };
	Parser *parser = &_parser;

	ASTToplevel *result = pool_alloc(parser->pool, sizeof *result);
	result->type = AST_FUNCTION_DEF;

	ASTType *return_type = parse_type(parser);
	if (return_type == NULL)
		return NULL;
	result->val.function_def.return_type = return_type;

	Token *identifier = read_token(parser);
	if (identifier->type != TOK_SYMBOL) {
		issue_error(token_context(identifier), "Expected identifier");
		return NULL;
	}
	result->val.function_def.name = identifier->val.symbol_or_string_literal;

	if (!expect_token(parser, TOK_LROUND)) {
		issue_error(parser_context(parser), "Expected '('");
		return NULL;
	}

	Array(ASTVar *) *arguments = &result->val.function_def.arguments;
	ARRAY_INIT(arguments, ASTVar *, 3);

	for (;;) {
		ASTType *arg_type = parse_type(parser);
		if (arg_type == NULL)
			return NULL;

		Token *identifier = read_token(parser);
		if (identifier->type != TOK_SYMBOL) {
			issue_error(token_context(identifier), "Expected identifier");
			return NULL;
		}

		ASTVar *argument = pool_alloc(parser->pool, sizeof *argument);
		argument->name = identifier->val.symbol_or_string_literal;
		argument->type = arg_type;

		*ARRAY_APPEND(arguments, ASTVar *) = argument;

		Token *comma_or_close_bracket = read_token(parser);
		if (comma_or_close_bracket->type == TOK_RROUND)
			break;
		if (comma_or_close_bracket->type != TOK_COMMA) {
			issue_error(token_context(comma_or_close_bracket), "Expected ')'");
			return NULL;
		}
	}

	if (!expect_token(parser, TOK_LCURLY)) {
		issue_error(parser_context(parser), "Expected '{'");
		return NULL;
	}

	ASTStatement *body = pool_alloc(parser->pool, sizeof *body);
	body->type = AST_COMPOUND_STATEMENT;
	Array(ASTStatement *) *statements = &body->val.statements;
	ARRAY_INIT(statements, ASTStatement *, 10);

	for (;;) {
		if (current_token(parser)->type == TOK_RCURLY)
			break;

		ASTStatement *statement = parse_statement(parser);
		if (statement == NULL)
			return NULL;

		*ARRAY_APPEND(statements, ASTStatement *) = statement;
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
		Array(ASTStatement *) *statements = &statement->val.statements;
		for (u32 i = 0; i < statements->size; i++) {
			dump_statement(*ARRAY_REF(statements, ASTStatement *, i));
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

		Array(ASTVar) *args = &ast->val.function_def.arguments;
		for (u32 i = 0; i < args->size; i++) {
			ASTVar *arg = *ARRAY_REF(args, ASTVar *, i);
			dump_type(arg->type);
			printf(" %s%s", arg->name, i == args->size - 1 ? "" : ", ");
		}

		fputs("), ", stdout);
		dump_statement(ast->val.function_def.body);

		puts(")");

		break;
	}
}
