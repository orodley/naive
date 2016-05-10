#include <assert.h>
#include <string.h>
#include <stdlib.h>

#include "array.h"
#include "ir.h"
#include "parse.h"

static void ir_gen_function(TransUnit *tu, Builder *builder, ASTFunctionDef *func);
static void ir_gen_toplevel_decl(TransUnit *tu, Builder *builder, ASTDecl *decl);
static void ir_gen_statement(Builder *builder, ASTStatement *statement);
static Value ir_gen_expression(Builder *builder, ASTExpr *expr);

typedef enum CTypeType
{
	INTEGER_TYPE,
} CTypeType;

typedef struct CType
{
	CTypeType type;

	union
	{
		struct
		{
			enum
			{
				CHAR,
				SHORT,
				INT,
				LONG,
				LONGLONG,
			} type;
			bool is_signed;
		} integer;
	} val;
} CType;

void ir_gen_toplevel(TransUnit *tu, Builder *builder, ASTToplevel *toplevel)
{
	while (toplevel != NULL) {
		switch (toplevel->type) {
		case FUNCTION_DEF:
			ir_gen_function(tu, builder, toplevel->val.function_def);
			break;
		case DECL:
			ir_gen_toplevel_decl(tu, builder, toplevel->val.decl);
			break;
		}

		toplevel = toplevel->next;
	}
}

typedef struct Binding
{
	CType type;
	char *name;
} Binding;

static IrType c_type_to_ir_type(CType ctype)
{
	assert(ctype.type == INTEGER_TYPE);
	u32 bit_width;
	switch (ctype.val.integer.type) {
	case INT:
		bit_width = 32;
		break;
	default:
		UNIMPLEMENTED;
	}

	return (IrType) {
		.bit_width = bit_width,
	};
}

static CType function_return_type(ASTFunctionDef *func)
{
	assert(func->declarator->type == DIRECT_DECLARATOR);
	ASTDirectDeclarator *direct_declarator = func->declarator->val.direct_declarator;
	assert(direct_declarator->type == FUNCTION_DECLARATOR);
	ASTDirectDeclarator *function_declarator =
		direct_declarator->val.function_declarator.declarator;
	assert(function_declarator->type == IDENTIFIER_DECLARATOR);

	ASTDeclSpecifier *decl_specifiers = func->specifiers;
	assert(decl_specifiers->next == NULL);
	assert(decl_specifiers->type == TYPE_SPECIFIER);

	ASTTypeSpecifier *type_specifier = decl_specifiers->val.type_specifier;
	assert(type_specifier->type == NAMED_TYPE_SPECIFIER);
	if (streq(type_specifier->val.name, "int")) {
		return (CType) {
			.type = INTEGER_TYPE,
			.val.integer.type = INT,
			.val.integer.is_signed = true,
		};
	} else {
		UNIMPLEMENTED;
	}
}

static Binding decl_to_binding(
		ASTDeclSpecifier *decl_specifiers, ASTDeclarator *declarator)
{
	IGNORE(decl_specifiers); IGNORE(declarator);
	UNIMPLEMENTED;
}

static void ir_gen_function(TransUnit *tu, Builder *builder, ASTFunctionDef *func)
{
	CType return_c_type = function_return_type(func);
	IrType return_ir_type = c_type_to_ir_type(return_c_type);

	assert(func->declarator->type == DIRECT_DECLARATOR);
	ASTDirectDeclarator *direct_declarator = func->declarator->val.direct_declarator;
	assert(direct_declarator->type == FUNCTION_DECLARATOR);

	ASTParameterDecl *params = direct_declarator->val.function_declarator.parameters;
	Array(Binding) param_bindings;
	ARRAY_INIT(&param_bindings, Binding, 5);
	
	while (params != NULL) {
		*ARRAY_APPEND(&param_bindings, Binding) =
			decl_to_binding(params->decl_specifiers, params->declarator);

		params = params->next;
	}

	u32 arity = param_bindings.size;
	IrType *arg_types = malloc(sizeof(*arg_types) * arity);
	for (u32 i = 0; i < arity; i++) {
		arg_types[i] =
			c_type_to_ir_type(ARRAY_REF(&param_bindings, Binding, i)->type);
	}

	IrFunction *f = trans_unit_add_function(
			tu,
			direct_declarator->val.function_declarator.declarator->val.name,
			return_ir_type, arity, arg_types);

	free(arg_types);

	builder->function = f;
	builder->current_block = &f->entry_block;

	ir_gen_statement(builder, func->body);
}

static void ir_gen_toplevel_decl(TransUnit *tu, Builder *builder, ASTDecl *decl)
{
	IGNORE(tu); IGNORE(builder); IGNORE(decl);

	UNIMPLEMENTED;
}

static void ir_gen_statement(Builder *builder, ASTStatement *statement)
{
	switch (statement->type) {
	case COMPOUND_STATEMENT: {
		ASTBlockItem *block_items = statement->val.block_items;
		while (block_items != NULL) {
			switch (block_items->type) {
			case BLOCK_ITEM_DECL:
				UNIMPLEMENTED;
				break;
			case BLOCK_ITEM_STATEMENT:
				ir_gen_statement(builder, block_items->val.statement);
				break;
			}

			block_items = block_items->next;
		}

		break;
	}
	case RETURN_STATEMENT: {
		Value value = ir_gen_expression(builder, statement->val.expr);
		Block *ret_block = &builder->function->ret_block;
		build_branch(builder, ret_block, value);
		break;
	}
	default:
		UNIMPLEMENTED;
	}
}

static Value ir_gen_expression(Builder *builder, ASTExpr *expr)
{
	IGNORE(builder);

	switch (expr->type) {
	case INT_LITERAL_EXPR:
		// @TODO: Determine types of constants correctly.
		return value_const((IrType) { .bit_width = 32 }, expr->val.int_literal);
	case BIT_XOR_EXPR:
		return build_binary_instr(
				builder,
				OP_BIT_XOR,
				ir_gen_expression(builder, expr->val.binary_op.arg1),
				ir_gen_expression(builder, expr->val.binary_op.arg2));
	default:
		UNIMPLEMENTED;
	}
}
