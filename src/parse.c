#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "array.h"
#include "diagnostics.h"
#include "misc.h"
#include "parse.h"
#include "pool.h"
#include "tokenise.h"

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

static inline void back_up(Parser *parser)
{
    parser->index--;
}

static inline void *revert(Parser *parser, u32 index)
{
    parser->index = index;
    return NULL;
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


typedef struct WhichResult
{
	u32 which;
	void *result;
} WhichResult;

typedef struct OptResult
{
	void *result;
} OptResult;

static inline void *middle(Parser *parser, void *a, void *b, void *c)
{
	UNUSED(parser); UNUSED(a); UNUSED(c);

	return b;
}

static inline void *second(Parser *parser, void *a, void *b)
{
	UNUSED(parser); UNUSED(a);

	return b;
}

static ASTType *build_type(Parser *parser, Token *type_name)
{
	assert(type_name->type == TOK_SYMBOL);

	ASTType *type = pool_alloc(parser->pool, sizeof *type);
	type->name = type_name->val.symbol_or_string_literal;

	return type;
}

static ASTExpr *build_constant(Parser *parser, Token *token)
{
	ASTExpr *expr = pool_alloc(parser->pool, sizeof *expr);
	switch (token->type) {
	case TOK_INT_LITERAL:
		expr->type = AST_INT_LITERAL;
		expr->val.int_literal = token->val.int_literal;
		break;
	default:
		assert(!"Not implemented");
	}

	return expr;
}

static ASTExpr *build_postfix_expr(Parser *parser,
		ASTExpr *curr, WhichResult *which)
{
	ASTExpr *next = pool_alloc(parser->pool, sizeof *next);
	switch (which->which) {
	case 0:
		next->type = AST_INDEX;
		next->val.binary_op.arg1 = curr;
		next->val.binary_op.arg2 = which->result;
		return next;
	case 1:
		// @TODO: Function call
		return NULL;
	case 2:
		next->type = AST_STRUCT_DOT_FIELD;
		next->val.struct_field.struct_value = curr;
		next->val.struct_field.field_name = which->result;
		return next;
	case 3:
		next->type = AST_STRUCT_ARROW_FIELD;
		next->val.struct_field.struct_value = curr;
		next->val.struct_field.field_name = which->result;
		return next;
	case 4:
		next->type = AST_POST_INCREMENT;
		next->val.unary_arg = curr;
		return next;
	case 5:
		next->type =AST_POST_DECREMENT;
		next->val.unary_arg = curr;
		return next;
	default:
		UNREACHABLE;
	}

	return NULL;
}

static ASTExpr *build_compound_initializer(Parser *parser,
		void *a, void *b, void *c, void *d, void *e, void *f, void *g)
{
	/// @TODO
	UNUSED(parser);
	UNUSED(a); UNUSED(b); UNUSED(c); UNUSED(d); UNUSED(e); UNUSED(f); UNUSED(g);
	return NULL;
}

static Array(void *) *empty_array(Parser *parser)
{
	Array(void *) *array = pool_alloc(parser->pool, sizeof *array);
	ARRAY_INIT(array, void *, 5);

	return array;
}

static ASTExpr *build_arg_list(Parser *parser, Array(ASTExpr) *curr,
		ASTExpr *next)
{
	// @TODO
	UNUSED(parser); UNUSED(curr); UNUSED(next);
	return NULL;
}


static ASTExpr *build_unary_expr(Parser *parser, Token *token,
		ASTExpr *arg)
{
	ASTExpr *next = pool_alloc(parser->pool, sizeof *next);
	next->val.unary_arg = arg;
	switch (token->type) {
	case TOK_INCREMENT: next->type = AST_PRE_INCREMENT; break;
	case TOK_DECREMENT: next->type = AST_PRE_DECREMENT; break;
	case TOK_AMPERSAND: next->type = AST_ADDRESS_OF; break;
	case TOK_ASTERISK: next->type = AST_DEREF; break;
	case TOK_PLUS: next->type = AST_UNARY_PLUS; break;
	case TOK_MINUS: next->type = AST_UNARY_MINUS; break;
	case TOK_BIT_NOT: next->type = AST_BIT_NOT; break;
	case TOK_NOT: next->type = AST_LOGICAL_NOT; break;
	default: UNREACHABLE;
	}

	return next;
}

static ASTExpr *build_sizeof_expr(
		Parser *parser, Token *tok_sizeof, ASTExpr *arg)
{
	UNUSED(tok_sizeof);

	ASTExpr *sizeof_expr = pool_alloc(parser->pool, sizeof *sizeof_expr);
	sizeof_expr->type = AST_SIZEOF_EXPR;
	sizeof_expr->val.unary_arg = arg;

	return sizeof_expr;
}

static ASTExpr *build_sizeof_type(Parser *parser, Token *tok_sizeof,
		Token *lround, ASTType *type, Token *rround)
{
	UNUSED(tok_sizeof);
	UNUSED(lround);
	UNUSED(rround);

	ASTExpr *sizeof_type = pool_alloc(parser->pool, sizeof *sizeof_type);
	sizeof_type->type = AST_SIZEOF_TYPE;
	sizeof_type->val.type = type;

	return sizeof_type;
}

static ASTExpr *build_cast_expr(Parser *parser, Token *lround,
		ASTType *type, Token *rround, ASTExpr *arg)
{
	UNUSED(lround); UNUSED(rround);

	ASTExpr *cast_expr = pool_alloc(parser->pool, sizeof *cast_expr);
	cast_expr->type = AST_CAST;
	cast_expr->val.cast.cast_type = type;
	cast_expr->val.cast.arg = arg;

	return cast_expr;
}

typedef struct BinaryTail
{
	Token *operator;
	ASTExpr *tail_expr;
} BinaryTail;

static BinaryTail *build_binary_tail(Parser *parser,
		Token *operator, ASTExpr *tail_expr)
{
	BinaryTail *binary_tail = pool_alloc(parser->pool, sizeof *binary_tail);
	binary_tail->operator = operator;
	binary_tail->tail_expr = tail_expr;

	return binary_tail;
}

#define CASE2(token, ast_type) \
	case TOK_##token: expr->type = AST_##ast_type; break;
#define CASE1(operator) CASE2(operator, operator)

static ASTExpr *build_binary_head(Parser *parser, ASTExpr *curr,
		BinaryTail *tail)
{
	ASTExpr *expr = pool_alloc(parser->pool, sizeof *expr);
	expr->val.binary_op.arg1 = curr;
	expr->val.binary_op.arg2 = tail->tail_expr;

	switch (tail->operator->type) {
	CASE2(ASTERISK, MULTIPLY)
	CASE2(DIVIDE, DIVIDE)
	CASE2(MOD, MODULO)
	CASE2(PLUS, ADD)
	CASE1(MINUS)
	CASE1(LEFT_SHIFT)
	CASE1(RIGHT_SHIFT)
	CASE1(LESS_THAN)
	CASE1(GREATER_THAN)
	CASE1(LESS_THAN_OR_EQUAL)
	CASE1(GREATER_THAN_OR_EQUAL)
	CASE1(EQUAL)
	CASE1(NOT_EQUAL)
	CASE2(AMPERSAND, BIT_AND)
	CASE1(BIT_XOR)
	CASE1(BIT_OR)
	CASE1(LOGICAL_AND)
	CASE1(LOGICAL_OR)
	CASE1(ASSIGN)
	CASE1(MULT_ASSIGN)
	CASE1(DIVIDE_ASSIGN)
	CASE1(MOD_ASSIGN)
	CASE1(PLUS_ASSIGN)
	CASE1(MINUS_ASSIGN)
	CASE1(LEFT_SHIFT_ASSIGN)
	CASE1(RIGHT_SHIFT_ASSIGN)
	CASE1(BIT_AND_ASSIGN)
	CASE1(BIT_XOR_ASSIGN)
	CASE1(BIT_OR_ASSIGN)

	default: UNREACHABLE;
	}

	return expr;
}

#undef CASE

// @TODO: We actually want to use a fold for this, so we need
// build_ternary_head and build_ternary_tail.
static ASTExpr *build_conditional_expr(Parser *parser,
		ASTExpr *condition, Token *q, ASTExpr *then_expr,
		Token *colon, ASTExpr *else_expr)
{
	UNUSED(q);
	UNUSED(colon);

	ASTExpr *expr = pool_alloc(parser->pool, sizeof *expr);
	expr->type = AST_CONDITIONAL;
	expr->val.ternary_op.arg1 = condition;
	expr->val.ternary_op.arg2 = then_expr;
	expr->val.ternary_op.arg3 = else_expr;

	return expr;
}


#include "parse.inc"


static ASTStatement *parse_statement(Parser *parser)
{
	if (!expect_keyword(parser, "return")) {
		issue_error(parser_context(parser), "Expected 'return'");
		return NULL;
	}

	ASTExpr *e = expr(parser);
	if (e == NULL)
		return NULL;

	if (!expect_token(parser, TOK_SEMICOLON)) {
		issue_error(parser_context(parser), "Expected ';'");
		return NULL;
	}

	ASTStatement *statement = pool_alloc(parser->pool, sizeof *statement);
	statement->type = AST_RETURN_STATEMENT;
	statement->val.return_value = e;

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

	ASTType *return_type = type_name(parser);
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
		ASTType *arg_type = type_name(parser);
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

#define X(x) #x
static const char *expr_type_names[] = {
	AST_EXPR_TYPES
};
#undef X

static void dump_expr(ASTExpr *expr)
{
	printf("%s(", expr_type_names[expr->type]);
	switch (expr->type) {
	case AST_INT_LITERAL:
		printf("%" PRId64, expr->val.int_literal);
		break;
	case AST_STRUCT_DOT_FIELD: case AST_STRUCT_ARROW_FIELD:
		dump_expr(expr->val.struct_field.struct_value);
		printf(", %s", expr->val.struct_field.field_name);
		break;
	case AST_INDEX: case AST_POST_INCREMENT: case AST_POST_DECREMENT:
	case AST_PRE_INCREMENT: case AST_PRE_DECREMENT: case AST_ADDRESS_OF:
	case AST_DEREF: case AST_UNARY_PLUS: case AST_UNARY_MINUS:
	case AST_BIT_NOT: case AST_LOGICAL_NOT: case AST_SIZEOF_EXPR:
		dump_expr(expr->val.unary_arg);
		break;
	case AST_CAST:
		dump_type(expr->val.cast.cast_type);
		fputs(", ", stdout);
		dump_expr(expr->val.cast.arg);
		break;
	case AST_SIZEOF_TYPE:
		dump_type(expr->val.type);
		break;
	case AST_MULTIPLY: case AST_DIVIDE: case AST_MODULO: case AST_ADD:
	case AST_MINUS: case AST_LEFT_SHIFT: case AST_RIGHT_SHIFT:
	case AST_LESS_THAN: case AST_GREATER_THAN: case AST_LESS_THAN_OR_EQUAL:
	case AST_GREATER_THAN_OR_EQUAL: case AST_EQUAL: case AST_NOT_EQUAL:
	case AST_BIT_AND: case AST_BIT_XOR: case AST_BIT_OR: case AST_LOGICAL_AND:
	case AST_LOGICAL_OR: case AST_ASSIGN: case AST_MULT_ASSIGN:
	case AST_DIVIDE_ASSIGN: case AST_MOD_ASSIGN: case AST_PLUS_ASSIGN:
	case AST_MINUS_ASSIGN: case AST_LEFT_SHIFT_ASSIGN:
	case AST_RIGHT_SHIFT_ASSIGN: case AST_BIT_AND_ASSIGN:
	case AST_BIT_XOR_ASSIGN: case AST_BIT_OR_ASSIGN:
		dump_expr(expr->val.binary_op.arg1);
		fputs(", ", stdout);
		dump_expr(expr->val.binary_op.arg2);
		break;
	case AST_CONDITIONAL:
		dump_expr(expr->val.ternary_op.arg1);
		fputs(", ", stdout);
		dump_expr(expr->val.ternary_op.arg2);
		fputs(", ", stdout);
		dump_expr(expr->val.ternary_op.arg3);
		break;
	default:
		UNREACHABLE;
	}

	putchar(')');
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
