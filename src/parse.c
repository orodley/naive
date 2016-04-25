#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>

#include "array.h"
#include "diagnostics.h"
#include "misc.h"
#include "parse.h"
#include "pool.h"
#include "tokenise.h"

typedef struct TypeTableEntry
{
	const char *type_name;
} TypeTableEntry;

typedef struct TypeTable
{
	Array(TypeTableEntry) entries;
} TypeTable;

static const char *builtin_types[] = {
	"void", "char", "short", "int", "long", "float", "double",
	"signed", "unsigned", "_Bool", "_Complex",
};

static void type_table_add_entry(TypeTable *table, TypeTableEntry entry)
{
	*ARRAY_APPEND(&table->entries, TypeTableEntry) = entry;
}

static void type_table_init(TypeTable *type_table)
{
	ARRAY_INIT(&type_table->entries, TypeTableEntry,
			STATIC_ARRAY_LENGTH(builtin_types));
	for (u32 i = 0; i < STATIC_ARRAY_LENGTH(builtin_types); i++) {
		TypeTableEntry entry = { .type_name = builtin_types[i] };
		type_table_add_entry(type_table, entry);
	}
}

static bool type_table_look_up_name(
		TypeTable *type_table, const char *name, TypeTableEntry *out)
{
	for (u32 i = 0; i < type_table->entries.size; i++) {
		TypeTableEntry *entry = ARRAY_REF(&type_table->entries, TypeTableEntry, i);
		if (strcmp(entry->type_name, name) == 0) {
			*out = *entry;
			return true;
		}
	}

	return false;
}

typedef struct Parser
{
	Pool *pool;

	Array(SourceToken) *tokens;
	u32 index;

	TypeTable defined_types;
} Parser;

// @TODO: Move the functions in this file that are only used by generated code. 
// They could either just be directly in the header produced by peg.py, or in
// a separate "support" file which is #included by the header.

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

// @TODO: A lot of these build_* functions could probably be autogenerated too.

static inline void *middle(Parser *parser, void *a, void *b, void *c)
{
	IGNORE(parser); IGNORE(a); IGNORE(c);

	return b;
}

static inline void *first(Parser *parser, void *a, void *b)
{
	IGNORE(parser); IGNORE(b);

	return a;
}

static inline void *second(Parser *parser, void *a, void *b)
{
	IGNORE(parser); IGNORE(a);

	return b;
}

static inline void *ignore(Parser *parser, ...)
{
	IGNORE(parser);

	return NULL;
}

static ASTExpr *build_identifier(Parser *parser, Token *identifier_token)
{
	ASTExpr *identifier = pool_alloc(parser->pool, sizeof *identifier);
	identifier->type = AST_IDENTIFIER;
	identifier->val.identifier = identifier_token->val.symbol_or_string_literal;

	return identifier;
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
	IGNORE(parser);
	IGNORE(a); IGNORE(b); IGNORE(c); IGNORE(d); IGNORE(e); IGNORE(f); IGNORE(g);
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
	IGNORE(parser); IGNORE(curr); IGNORE(next);
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
	case TOK_LOGICAL_NOT: next->type = AST_LOGICAL_NOT; break;
	default: UNREACHABLE;
	}

	return next;
}

static ASTExpr *build_sizeof_expr(
		Parser *parser, Token *tok_sizeof, ASTExpr *arg)
{
	IGNORE(tok_sizeof);

	ASTExpr *sizeof_expr = pool_alloc(parser->pool, sizeof *sizeof_expr);
	sizeof_expr->type = AST_SIZEOF_EXPR;
	sizeof_expr->val.unary_arg = arg;

	return sizeof_expr;
}

static ASTExpr *build_sizeof_type(Parser *parser, Token *tok_sizeof,
		Token *lround, ASTTypeName *type, Token *rround)
{
	IGNORE(tok_sizeof);
	IGNORE(lround);
	IGNORE(rround);

	ASTExpr *sizeof_type = pool_alloc(parser->pool, sizeof *sizeof_type);
	sizeof_type->type = AST_SIZEOF_TYPE;
	sizeof_type->val.type = type;

	return sizeof_type;
}

static ASTExpr *build_cast_expr(Parser *parser, Token *lround,
		ASTTypeName *type, Token *rround, ASTExpr *arg)
{
	IGNORE(lround); IGNORE(rround);

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
	CASE1(MODULO)
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
	CASE1(MODULO_ASSIGN)
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

#undef CASE1
#undef CASE2

// @TODO: We actually want to use a fold for this, so we need
// build_ternary_head and build_ternary_tail.
static ASTExpr *build_conditional_expr(Parser *parser,
		ASTExpr *condition, Token *q, ASTExpr *then_expr,
		Token *colon, ASTExpr *else_expr)
{
	IGNORE(q);
	IGNORE(colon);

	ASTExpr *expr = pool_alloc(parser->pool, sizeof *expr);
	expr->type = AST_CONDITIONAL;
	expr->val.ternary_op.arg1 = condition;
	expr->val.ternary_op.arg2 = then_expr;
	expr->val.ternary_op.arg3 = else_expr;

	return expr;
	
}


ASTStatement *build_labeled_statement(Parser *parser, Token *label,
		Token *colon, ASTStatement *statement)
{
	IGNORE(colon);

	ASTStatement *labeled_statement =
		pool_alloc(parser->pool, sizeof *labeled_statement);
	labeled_statement->type = AST_LABELED_STATEMENT;
	labeled_statement->val.labeled_statement.label_name =
		label->val.symbol_or_string_literal;
	labeled_statement->val.labeled_statement.statement = statement;
	return labeled_statement;
}

ASTStatement *build_case_statement(Parser *parser, Token *case_keyword,
		ASTExpr *case_value, Token *colon, ASTStatement *statement)
{
	IGNORE(case_keyword);
	IGNORE(colon);

	ASTStatement *case_statement = pool_alloc(parser->pool, sizeof *case_statement);
	case_statement->type = AST_CASE_STATEMENT;
	case_statement->val.expr_and_statement.expr = case_value;
	case_statement->val.expr_and_statement.statement = statement;

	return case_statement;
}

ASTStatement *build_compound_statement(Parser *parser, Token *lcurly,
		ASTBlockItem *block_items, Token *rcurly)
{
	IGNORE(lcurly);
	IGNORE(rcurly);

	ASTStatement *compound_statement =
		pool_alloc(parser->pool, sizeof *compound_statement);
	compound_statement->type = AST_COMPOUND_STATEMENT;
	compound_statement->val.block_items = block_items;

	return compound_statement;
}

ASTBlockItem *build_block_item(Parser *parser, WhichResult *decl_or_statement)
{
	ASTBlockItem *result = pool_alloc(parser->pool, sizeof *result);
	switch (decl_or_statement->which) {
	case 0:
		result->type = BLOCK_ITEM_DECL;
		result->val.decl = decl_or_statement->result;
		break;
	case 1:
		result->type = BLOCK_ITEM_STATEMENT;
		result->val.statement = decl_or_statement->result;
		break;
	default:
		UNREACHABLE;
	}

	return result;
}

ASTStatement *build_expr_statement(
		Parser *parser, OptResult *opt_expr, Token *semicolon)
{
	IGNORE(semicolon);

	ASTStatement *statement = pool_alloc(parser->pool, sizeof *statement);
	if (opt_expr->result == NULL) {
		statement->type = AST_EMPTY_STATEMENT;
		return statement;
	}

	ASTExpr *expr = opt_expr->result;
	statement->type = AST_EXPR_STATEMENT;
	statement->val.expr = expr;
	return statement;
}

ASTStatement *build_if_statement(Parser *parser, Token *if_token, Token *lround,
		ASTExpr *condition, Token *rround, ASTStatement *then_statement,
		OptResult *else_statement)
{
	IGNORE(if_token); IGNORE(lround); IGNORE(rround);

	ASTStatement *if_statement = pool_alloc(parser->pool, sizeof *if_statement);
	if_statement->type = AST_IF_STATEMENT;
	if_statement->val.if_statement.condition = condition;
	if_statement->val.if_statement.then_statement = then_statement;
	// Potentially NULL if no else clause. This is fine as this is exactly what
	// a 'else_statement' field indicates.
	if_statement->val.if_statement.else_statement = else_statement->result;
	return if_statement;
}

ASTStatement *build_switch_statement(Parser *parser, Token *switch_token,
		Token *lround, ASTExpr *switch_expr, Token *rround, ASTStatement *body)
{
	IGNORE(switch_token); IGNORE(lround); IGNORE(rround);

	ASTStatement *switch_statement = pool_alloc(parser->pool, sizeof *switch_statement);
	switch_statement->type = AST_SWITCH_STATEMENT;
	switch_statement->val.expr_and_statement.expr = switch_expr;
	switch_statement->val.expr_and_statement.statement = body;
	return switch_statement;
}

ASTStatement *build_while_statement(Parser *parser, Token *tok_while,
		Token *lround, ASTExpr *condition, Token *rround, ASTStatement *body)
{
	IGNORE(tok_while);
	IGNORE(lround);
	IGNORE(rround);

	ASTStatement *while_statement = pool_alloc(parser->pool, sizeof *while_statement);
	while_statement->type = AST_WHILE_STATEMENT;
	while_statement->val.expr_and_statement.expr = condition;
	while_statement->val.expr_and_statement.statement = body;
	return while_statement;
}

ASTStatement *build_do_while_statement(Parser *parser, Token *tok_do,
		ASTStatement *body, Token *tok_while,
		Token *lround, ASTExpr *condition, Token *rround, Token *semi)
{
	IGNORE(tok_do); IGNORE(tok_while); IGNORE(lround); IGNORE(rround); IGNORE(semi);

	ASTStatement *do_while_statement =
		pool_alloc(parser->pool, sizeof *do_while_statement);
	do_while_statement->type = AST_DO_WHILE_STATEMENT;
	do_while_statement->val.expr_and_statement.expr = condition;
	do_while_statement->val.expr_and_statement.statement = body;
	return do_while_statement;
}

ASTStatement *build_for_statement(Parser *parser, Token *tok_for, Token *lround,
		OptResult *init, Token *semi1, OptResult *condition, Token *semi2,
		OptResult *update, Token *rround, ASTStatement *body)
{
	IGNORE(tok_for); IGNORE(lround); IGNORE(semi1); IGNORE(semi2); IGNORE(rround);

	ASTStatement *for_statement = pool_alloc(parser->pool, sizeof *for_statement);
	for_statement->type = AST_FOR_STATEMENT;
	for_statement->val.for_statement.init_expr = init->result;
	for_statement->val.for_statement.condition = condition->result;
	for_statement->val.for_statement.update_expr = update->result;
	for_statement->val.for_statement.body = body;
	return for_statement;
}

// @TODO
ASTStatement *build_for_decl_statement(void *a, void *b, void *c, void *d,
		void *e, void *f, void *g, void *h, void *i)
{
	(void)a; (void)b; (void)c; (void)d; (void)e; (void)f; (void)g; (void)h; (void)i;
	return NULL;
}

ASTStatement *build_goto_statement(Parser *parser, Token *tok_goto, Token *label)
{
	IGNORE(tok_goto);

	ASTStatement *goto_statement = pool_alloc(parser->pool, sizeof *goto_statement);
	goto_statement->type = AST_GOTO_STATEMENT;
	goto_statement->val.goto_label = label->val.symbol_or_string_literal;
	return goto_statement;
}

ASTStatement *build_continue_statement(Parser *parser, Token *tok_cont, Token *semi)
{
	IGNORE(tok_cont);
	IGNORE(semi);

	ASTStatement *continue_statement =
		pool_alloc(parser->pool, sizeof *continue_statement);
	continue_statement->type = AST_CONTINUE_STATEMENT;
	return continue_statement;
}

ASTStatement *build_break_statement(Parser *parser, Token *tok_break, Token *semi)
{
	IGNORE(tok_break);
	IGNORE(semi);

	ASTStatement *break_statement =
		pool_alloc(parser->pool, sizeof *break_statement);
	break_statement->type = AST_BREAK_STATEMENT;
	return break_statement;
}

ASTStatement *build_return_statement(Parser *parser, Token *tok_return,
		OptResult *expr, Token *semi)
{
	IGNORE(tok_return);
	IGNORE(semi);

	ASTStatement *return_statement =
		pool_alloc(parser->pool, sizeof *return_statement);
	return_statement->type = AST_RETURN_STATEMENT;
	return_statement->val.expr = expr->result;
	return return_statement;
}


static ASTToplevel *build_toplevel(Parser *parser, WhichResult *function_def_or_decl)
{
	ASTToplevel *toplevel = pool_alloc(parser->pool, sizeof *toplevel);
	switch (function_def_or_decl->which) {
	case 0:
		toplevel->type = FUNCTION_DEF;
		toplevel->val.function_def = function_def_or_decl->result;
		break;
	case 1:
		toplevel->type = DECL;
		toplevel->val.decl = function_def_or_decl->result;
		break;
	default:
		UNREACHABLE;
	}

	return toplevel;
}

static ASTDeclSpecifier *build_storage_class_specifier(Parser *parser, WhichResult *keyword)
{
	ASTDeclSpecifier *result = pool_alloc(parser->pool, sizeof *result);
	result->type = STORAGE_CLASS_SPECIFIER;

	ASTStorageClassSpecifier specifier;
	switch (keyword->which) {
	case 0: specifier = TYPEDEF_SPECIFIER; break;
	case 1: specifier = EXTERN_SPECIFIER; break;
	case 2: specifier = STATIC_SPECIFIER; break;
	case 3: specifier = AUTO_SPECIFIER; break;
	case 4: specifier = REGISTER_SPECIFIER; break;
	default: UNREACHABLE;
	}
	result->val.storage_class_specifier = specifier;

	return result;
}

static ASTDeclSpecifier *build_type_qualifier(Parser *parser, WhichResult *keyword)
{
	ASTDeclSpecifier *result = pool_alloc(parser->pool, sizeof *result);
	result->type = TYPE_QUALIFIER;

	ASTTypeQualifier qualifier;
	switch (keyword->which) {
	case 0: qualifier = CONST_QUALIFIER; break;
	case 1: qualifier = RESTRICT_QUALIFIER; break;
	case 2: qualifier = VOLATILE_QUALIFIER; break;
	default: UNREACHABLE;
	}
	result->val.type_qualifier = qualifier;

	return result;
}

static ASTDeclSpecifier *build_function_specifier(Parser *parser, Token *keyword)
{
	IGNORE(keyword);

	ASTDeclSpecifier *result = pool_alloc(parser->pool, sizeof *result);
	result->type = FUNCTION_SPECIFIER;
	result->val.function_specifier = INLINE_SPECIFIER;

	return result;
}

// @TODO: We currently don't add anything to the type table apart from builtin
// types. We need to add typedefs and named tagged types as we go.
static Token *named_type(Parser *parser)
{
	Token *token = read_token(parser);
	if (token->type != TOK_SYMBOL) {
		back_up(parser);
		return NULL;
	}

	const char *name = token->val.symbol_or_string_literal;
	TypeTableEntry entry;
	if (!type_table_look_up_name(&parser->defined_types, name, &entry)) {
		back_up(parser);
		return NULL;
	}

	return token;
}

ASTTypeSpecifier *build_struct_or_union_tagged_named_type(
		Parser *parser, WhichResult *keyword, Token *name)
{
	ASTTypeSpecifier *tagged_type = pool_alloc(parser->pool, sizeof *tagged_type);
	tagged_type->type = keyword->which == 0 ?
		STRUCT_TYPE_SPECIFIER :
		UNION_TYPE_SPECIFIER;
	tagged_type->val.struct_or_union_specifier.name = name->val.symbol_or_string_literal;
	tagged_type->val.struct_or_union_specifier.fields = NULL;

	return tagged_type;
}

ASTTypeSpecifier *build_enum_tagged_named_type(
		Parser *parser, Token *keyword, Token *name)
{
	IGNORE(keyword);

	ASTTypeSpecifier *tagged_type = pool_alloc(parser->pool, sizeof *tagged_type);
	tagged_type->type = ENUM_TYPE_SPECIFIER;
	tagged_type->val.enum_specifier.name = name->val.symbol_or_string_literal;
	tagged_type->val.enum_specifier.enumerators = NULL;

	return tagged_type;
}

ASTTypeSpecifier *build_struct_or_union(Parser *parser, WhichResult *keyword,
		OptResult *name, Token *lcurly, ASTFieldDecl *fields, Token *rcurly)
{
	IGNORE(lcurly);
	IGNORE(rcurly);

	ASTTypeSpecifier *result = pool_alloc(parser->pool, sizeof *result);
	result->type = keyword->which == 0 ?
		STRUCT_TYPE_SPECIFIER :
		UNION_TYPE_SPECIFIER;
	result->val.struct_or_union_specifier.name = name->result;
	result->val.struct_or_union_specifier.fields = fields;

	return result;
}

ASTTypeSpecifier *build_enum(Parser *parser, Token *keyword, OptResult *name,
		Token *lcurly, ASTEnumerator *enumerators, OptResult *comma, Token *rcurly)
{
	IGNORE(keyword); IGNORE(lcurly); IGNORE(comma); IGNORE(rcurly);

	ASTTypeSpecifier *result = pool_alloc(parser->pool, sizeof *result);
	result->type = ENUM_TYPE_SPECIFIER;
	result->val.enum_specifier.name = name->result;
	result->val.enum_specifier.enumerators = enumerators;

	return result;
}

// @TODO: This feels unnecessary. Couldn't we just have the parser keep
// wrapping the next thing in the input? This is complicated a bit because
// 'pointer' is currently a separate parser to the thing after it.
typedef struct PointerResult
{
	ASTDeclarator *first;
	ASTDeclarator *last;
} PointerResult;

PointerResult *build_next_pointer(Parser *parser, PointerResult *pointers,
		ASTDeclarator *pointer)
{
	IGNORE(parser);

	pointers->last->val.pointer_declarator.pointee = pointer;
	pointers->last = pointer;

	return pointers;
}

ASTDeclarator *build_pointee_declarator(Parser *parser, OptResult *pointer,
		ASTDirectDeclarator *declarator)
{
	ASTDeclarator *result = pool_alloc(parser->pool, sizeof *result);
	result->type = DIRECT_DECLARATOR;
	result->val.direct_declarator = declarator;

	if (pointer->result == NULL)
		return result;

	PointerResult *pointer_result = pointer->result;
	pointer_result->last->val.pointer_declarator.pointee = result;

	return pointer_result->first;
}

ASTDeclarator *build_terminal_pointer(Parser *parser, PointerResult *pointer_result)
{
	IGNORE(parser);

	pointer_result->last->val.pointer_declarator.pointee = NULL;
	return pointer_result->first;
}

ASTDirectDeclarator *build_sub_declarator(Parser *parser,
		ASTDirectDeclarator *declarator,
		WhichResult *function_or_array_declarator)
{
	ASTDirectDeclarator *result = pool_alloc(parser->pool, sizeof *result);
	switch (function_or_array_declarator->which) {
	case 0:
		result->type = ARRAY_DECLARATOR;
		result->val.array_declarator.element_declarator = declarator;
		result->val.array_declarator.array_length = function_or_array_declarator->result;
		break;
	case 1:
		result->type = FUNCTION_DECLARATOR;
		result->val.function_declarator.declarator = declarator;
		result->val.function_declarator.parameters = function_or_array_declarator->result;
		break;
	default: UNREACHABLE;
	}

	return result;
}

#include "parse.inc"



// The input array consists of SourceTokens, but we treat them as Tokens most
// of the time.
ASTToplevel *parse_toplevel(Array(SourceToken) *tokens, Pool *ast_pool)
{
	Parser parser = { ast_pool, tokens, 0, { ARRAY_ZEROED } };
	type_table_init(&parser.defined_types);

	ASTToplevel *result = translation_unit(&parser);
	if (result == NULL && _unexpected_token.type != TOK_INVALID) {
		issue_error(&_longest_parse_pos, "Unexpected token %s",
				token_type_names[_unexpected_token.type]);
	}

	return result;
}


static int indent_level = 0;

#if 0
static inline void print_indent(void)
{
	for (int n = 0; n < indent_level; n++)
		fputs("    ", stdout);
}
#endif

static void pretty_printf(const char *fmt, ...)
{
#if 0
	size_t len = strlen(fmt);
	if (len == 1 && fmt[len - 1] == ')') {
		putchar('\n');

		indent_level--;
		print_indent();

		puts(")");
		return;
	} else if (len == 2 && strcmp(fmt, ", ") == 0) {
		puts(",");
		print_indent();
		return;
	}
#endif

	va_list varargs;
	va_start(varargs, fmt);
	vprintf(fmt, varargs);
	va_end(varargs);

#if 0
	if (fmt[len - 1] == '(') {
		indent_level++;
		putchar('\n');
		print_indent();
	}
#endif
}

#define X(x) #x
static const char *expr_type_names[] = {
	AST_EXPR_TYPES
};
#undef X


static void dump_type_name(ASTTypeName *type_name)
{
	IGNORE(type_name);
	assert(!"Not implemented");
}

static void dump_expr(ASTExpr *expr)
{
	pretty_printf("%s(", expr_type_names[expr->type]);
	switch (expr->type) {
	case AST_INT_LITERAL:
		pretty_printf("%" PRId64, expr->val.int_literal);
		break;
	case AST_IDENTIFIER:
		pretty_printf(expr->val.identifier);
		break;
	case AST_STRUCT_DOT_FIELD: case AST_STRUCT_ARROW_FIELD:
		dump_expr(expr->val.struct_field.struct_value);
		pretty_printf(", %s", expr->val.struct_field.field_name);
		break;
	case AST_INDEX: case AST_POST_INCREMENT: case AST_POST_DECREMENT:
	case AST_PRE_INCREMENT: case AST_PRE_DECREMENT: case AST_ADDRESS_OF:
	case AST_DEREF: case AST_UNARY_PLUS: case AST_UNARY_MINUS:
	case AST_BIT_NOT: case AST_LOGICAL_NOT: case AST_SIZEOF_EXPR:
		dump_expr(expr->val.unary_arg);
		break;
	case AST_CAST:
		pretty_printf("<some type>, ");
		dump_expr(expr->val.cast.arg);
		break;
	case AST_SIZEOF_TYPE:
		dump_type_name(expr->val.type);
		break;
	case AST_MULTIPLY: case AST_DIVIDE: case AST_MODULO: case AST_ADD:
	case AST_MINUS: case AST_LEFT_SHIFT: case AST_RIGHT_SHIFT:
	case AST_LESS_THAN: case AST_GREATER_THAN: case AST_LESS_THAN_OR_EQUAL:
	case AST_GREATER_THAN_OR_EQUAL: case AST_EQUAL: case AST_NOT_EQUAL:
	case AST_BIT_AND: case AST_BIT_XOR: case AST_BIT_OR: case AST_LOGICAL_AND:
	case AST_LOGICAL_OR: case AST_ASSIGN: case AST_MULT_ASSIGN:
	case AST_DIVIDE_ASSIGN: case AST_MODULO_ASSIGN: case AST_PLUS_ASSIGN:
	case AST_MINUS_ASSIGN: case AST_LEFT_SHIFT_ASSIGN:
	case AST_RIGHT_SHIFT_ASSIGN: case AST_BIT_AND_ASSIGN:
	case AST_BIT_XOR_ASSIGN: case AST_BIT_OR_ASSIGN:
		dump_expr(expr->val.binary_op.arg1);
		pretty_printf(", ");
		dump_expr(expr->val.binary_op.arg2);
		break;
	case AST_CONDITIONAL:
		dump_expr(expr->val.ternary_op.arg1);
		pretty_printf(", ");
		dump_expr(expr->val.ternary_op.arg2);
		pretty_printf(", ");
		dump_expr(expr->val.ternary_op.arg3);
		break;
	default:
		pretty_printf("%d\n", expr->type);
		UNREACHABLE;
	}

	pretty_printf(")");
}

#define X(x) #x
static const char *statement_type_names[] = {
	AST_STATEMENT_TYPES
};
#undef X

static void dump_statement(ASTStatement *statement)
{
	pretty_printf("%s(", statement_type_names[statement->type]);
	switch (statement->type) {
	case AST_EMPTY_STATEMENT:
	case AST_CONTINUE_STATEMENT:
	case AST_BREAK_STATEMENT:
		break;
	case AST_LABELED_STATEMENT:
		pretty_printf("%s, ", statement->val.labeled_statement.label_name);
		break;
	case AST_COMPOUND_STATEMENT: {
		ASTBlockItem *block_item = statement->val.block_items;
		while (block_item != NULL) {
			dump_statement(block_item->val.statement);
			if (block_item->next != NULL)
				pretty_printf(", ");
			block_item = block_item->next;
		}
		break;
	}
	case AST_EXPR_STATEMENT:
	case AST_RETURN_STATEMENT:
		dump_expr(statement->val.expr);
		break;
	case AST_IF_STATEMENT:
		dump_expr(statement->val.if_statement.condition);
		pretty_printf(", ");
		dump_statement(statement->val.if_statement.then_statement);
		if (statement->val.if_statement.else_statement != NULL) {
			pretty_printf(", ");
			dump_statement(statement->val.if_statement.else_statement);
		}
		break;
	case AST_CASE_STATEMENT:
	case AST_SWITCH_STATEMENT:
	case AST_WHILE_STATEMENT:
	case AST_DO_WHILE_STATEMENT:
		dump_expr(statement->val.expr_and_statement.expr);
		pretty_printf(", ");
		dump_statement(statement->val.expr_and_statement.statement);
		break;
	case AST_FOR_STATEMENT:
		// @TODO: For loops with decls.
		if (statement->val.for_statement.init_expr != NULL)
			dump_expr(statement->val.for_statement.init_expr);
		pretty_printf(", ");
		if (statement->val.for_statement.condition)
			dump_expr(statement->val.for_statement.condition);
		pretty_printf(", ");
		if (statement->val.for_statement.update_expr != NULL)
			dump_expr(statement->val.for_statement.update_expr);
		break;
	case AST_GOTO_STATEMENT:
		pretty_printf(statement->val.goto_label);
		break;
	default:
		assert(!"Not implemented");
	}

	pretty_printf(")");
}

static void dump_type_specifier(ASTTypeSpecifier *type_specifier)
{
	switch (type_specifier->type) {
	case NAMED_TYPE_SPECIFIER:
		pretty_printf("NAMED_TYPE_SPECIFIER(%s)", type_specifier->val.name);
		break;
	default:
		assert(!"Not implemented");
	}
}

static void dump_decl_specifiers(ASTDeclSpecifier *specifiers)
{
	pretty_printf("DECL_SPECIFIER(");

#define CASE(x) case x: pretty_printf(#x); break;
	while (specifiers != NULL) {
		switch (specifiers->type) {
		case STORAGE_CLASS_SPECIFIER:
			switch (specifiers->val.storage_class_specifier) {
			CASE(TYPEDEF_SPECIFIER) CASE(EXTERN_SPECIFIER)
			CASE(STATIC_SPECIFIER) CASE(AUTO_SPECIFIER) CASE(REGISTER_SPECIFIER)
			}
			break;
		case TYPE_QUALIFIER:
			switch (specifiers->val.type_qualifier) {
			CASE(CONST_QUALIFIER) CASE(RESTRICT_QUALIFIER) CASE(VOLATILE_QUALIFIER)
			}
			break;
#undef CASE
		case FUNCTION_SPECIFIER:
			assert(specifiers->val.function_specifier == INLINE_SPECIFIER);
			pretty_printf("INLINE_SPECIFIER");
			break;
		case TYPE_SPECIFIER:
			dump_type_specifier(specifiers->val.type_specifier);
			break;
		}

		if (specifiers->next != NULL)
			pretty_printf(", ");

		specifiers = specifiers->next;
	}

	pretty_printf(")");
}

static void dump_declarator(ASTDeclarator *declarator);

static void dump_parameter_decls(ASTParameterDecl *param_decls)
{
	pretty_printf("PARAM_DECLS(");
	while (param_decls != NULL) {
		pretty_printf("PARAM(");
		dump_decl_specifiers(param_decls->decl_specifiers);
		pretty_printf(", ");
		dump_declarator(param_decls->declarator);
		pretty_printf(")");

		param_decls = param_decls->next;
	}

	pretty_printf(")");
}

static void dump_direct_declarator(ASTDirectDeclarator *declarator)
{
	switch (declarator->type) {
	case DECLARATOR:
		pretty_printf("DECLARATOR(");
		dump_declarator(declarator->val.declarator);
		break;
	case IDENTIFIER_DECLARATOR:
		pretty_printf("IDENTIFIER_DECLARATOR(%s", declarator->val.name);
		break;
	case FUNCTION_DECLARATOR:
		pretty_printf("FUNCTION_DECLARATOR(");
		dump_direct_declarator(declarator->val.function_declarator.declarator);
		pretty_printf(", ");
		dump_parameter_decls(declarator->val.function_declarator.parameters);
		break;
	default:
		assert(!"Not implemented");
	}

	pretty_printf(")");
}

static void dump_declarator(ASTDeclarator *declarator)
{
	switch (declarator->type) {
	case POINTER_DECLARATOR:
		pretty_printf("POINTER_DECLARATOR(");
		dump_decl_specifiers(declarator->val.pointer_declarator.decl_specifiers);
		pretty_printf(", ");
		dump_declarator(declarator->val.pointer_declarator.pointee);
		break;
	case DIRECT_DECLARATOR:
		pretty_printf("DIRECT_DECLARATOR(");
		dump_direct_declarator(declarator->val.direct_declarator);
		break;
	}

	pretty_printf(")");
}

static void dump_init_declarators(ASTInitDeclarator *init_declarators)
{
	IGNORE(init_declarators);

	assert(!"Not implemented");
}

static void dump_decls(ASTDecl *decl)
{
	while (decl != NULL) {
		pretty_printf("DECL(");
		dump_decl_specifiers(decl->decl_specifiers);
		pretty_printf(", ");
		dump_init_declarators(decl->init_declarators);
		pretty_printf("), ");

		decl = decl->next;
	}
}

void dump_toplevel(ASTToplevel *ast)
{
	assert(indent_level == 0);

	switch (ast->type) {
	case FUNCTION_DEF:
		pretty_printf("FUNCTION_DEF(");
		dump_decl_specifiers(ast->val.function_def->specifiers);
		pretty_printf(", ");
		dump_declarator(ast->val.function_def->declarator);
		pretty_printf(", ");
		dump_decls(ast->val.function_def->old_style_param_decls);
		dump_statement(ast->val.function_def->body);
		break;
	case DECL:
		pretty_printf("DECL(");
		dump_decls(ast->val.decl);
		break;

	default:
		assert(!"Not implemented");
	}

	pretty_printf(")");

	assert(indent_level == 0);
}
