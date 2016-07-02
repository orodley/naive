#include <assert.h>
#include <string.h>
#include <stdlib.h>

#include "array.h"
#include "ir.h"
#include "parse.h"

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

typedef struct Binding
{
	char *name;
	CType type;
	Value address;
} Binding;

typedef struct Scope
{
	Array(Binding) bindings;
	struct Scope *parent_scope;
} Scope;

Binding *binding_for_name(Scope *scope, char *name)
{
	for (u32 i = 0; i < scope->bindings.size; i++) {
		Binding *binding = ARRAY_REF(&scope->bindings, Binding, i);
		if (streq(binding->name, name))
			return binding;
	}

	if (scope->parent_scope != NULL) {
		return binding_for_name(scope->parent_scope, name);
	} else {
		return NULL;
	}
}

static void ir_gen_function(TransUnit *tu, Builder *builder, ASTFunctionDef *func);
static void ir_gen_toplevel_decl(TransUnit *tu, Builder *builder, ASTDecl *decl);
static void ir_gen_statement(Builder *builder, Scope *scope, ASTStatement *statement);
static Value ir_gen_expression(Builder *builder, Scope *scope, ASTExpr *expr);

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


static IrType c_type_to_ir_type(CType *ctype)
{
	assert(ctype->type == INTEGER_TYPE);
	u32 bit_width;
	switch (ctype->val.integer.type) {
	case INT:
		bit_width = 32;
		break;
	default:
		UNIMPLEMENTED;
	}

	return (IrType) {
		.kind = IR_INT,
		.val.bit_width = bit_width,
	};
}

typedef struct CDecl
{
	char *name;
	CType type;
} CDecl;

static void decl_to_cdecl(ASTDeclSpecifier *decl_specifier_list,
		ASTDeclarator *declarator, CDecl *cdecl)
{
	assert(declarator->type == DIRECT_DECLARATOR);
	ASTDirectDeclarator *direct_declarator = declarator->val.direct_declarator;

	if (direct_declarator->type == FUNCTION_DECLARATOR) {
		ASTDirectDeclarator *function_declarator =
			direct_declarator->val.function_declarator.declarator;
		assert(function_declarator->type == IDENTIFIER_DECLARATOR);

		cdecl->name = function_declarator->val.name;
	} else if (direct_declarator->type == IDENTIFIER_DECLARATOR) {
		cdecl->name = direct_declarator->val.name;
	} else {
		UNIMPLEMENTED;
	}

	assert(decl_specifier_list->next == NULL);
	assert(decl_specifier_list->type == TYPE_SPECIFIER);

	ASTTypeSpecifier *type_specifier = decl_specifier_list->val.type_specifier;
	assert(type_specifier->type == NAMED_TYPE_SPECIFIER);
	if (streq(type_specifier->val.name, "int")) {
		cdecl->type = (CType) {
			.type = INTEGER_TYPE,
			.val.integer.type = INT,
			.val.integer.is_signed = true,
		};
	} else {
		UNIMPLEMENTED;
	}
}

static void cdecl_to_binding(Builder *builder, CDecl *cdecl, Binding *binding)
{
	binding->name = cdecl->name;
	binding->type = cdecl->type;

	IrType ir_type = c_type_to_ir_type(&cdecl->type);
	binding->address = build_local(builder, ir_type);
}

static CType function_return_type(ASTFunctionDef *func)
{
	CDecl cdecl;
	decl_to_cdecl(func->decl_specifier_list, func->declarator, &cdecl);

	return cdecl.type;
}

static void ir_gen_function(TransUnit *tu, Builder *builder, ASTFunctionDef *func)
{
	CType return_c_type = function_return_type(func);
	IrType return_ir_type = c_type_to_ir_type(&return_c_type);

	assert(func->declarator->type == DIRECT_DECLARATOR);
	ASTDirectDeclarator *direct_declarator = func->declarator->val.direct_declarator;
	assert(direct_declarator->type == FUNCTION_DECLARATOR);

	ASTParameterDecl *first_param = direct_declarator->val.function_declarator.parameters;
	ASTParameterDecl *params = first_param;

	u32 arity = 0;
	while (params != NULL) {
		arity++;
		params = params->next;
	}

	params = first_param;

	IrType *arg_types = malloc(sizeof(*arg_types) * arity);
	for (u32 i = 0; i < arity; i++) {
		CDecl cdecl;
		decl_to_cdecl(params->decl_specifier_list, params->declarator, &cdecl);
		arg_types[i] = c_type_to_ir_type(&cdecl.type);

		params = params->next;
	}

	IrFunction *f = trans_unit_add_function(
			tu,
			direct_declarator->val.function_declarator.declarator->val.name,
			return_ir_type, arity, arg_types);

	builder->function = f;
	builder->current_block = &f->entry_block;

	Scope scope;
	scope.parent_scope = NULL;
	Array(Binding)* param_bindings = &scope.bindings;
	ARRAY_INIT(param_bindings, Binding, 5);

	params = first_param;
	for (u32 i = 0; params != NULL; i++, params = params->next) {
		Binding *next_binding = ARRAY_APPEND(param_bindings, Binding);

		CDecl cdecl;
		decl_to_cdecl(params->decl_specifier_list, params->declarator, &cdecl);
		cdecl_to_binding(builder, &cdecl, next_binding);

		build_store(builder,
				next_binding->address,
				value_arg(&f->entry_block.args[i]),
				c_type_to_ir_type(&cdecl.type));
	}

	free(arg_types);

	ir_gen_statement(builder, &scope, func->body);
}

static void ir_gen_toplevel_decl(TransUnit *tu, Builder *builder, ASTDecl *decl)
{
	IGNORE(tu); IGNORE(builder); IGNORE(decl);

	UNIMPLEMENTED;
}

static void ir_gen_statement(Builder *builder, Scope *scope, ASTStatement *statement)
{
	switch (statement->type) {
	case COMPOUND_STATEMENT: {
		ASTBlockItem *block_item_list = statement->val.block_item_list;
		while (block_item_list != NULL) {
			switch (block_item_list->type) {
			case BLOCK_ITEM_DECL:
				UNIMPLEMENTED;
				break;
			case BLOCK_ITEM_STATEMENT:
				ir_gen_statement(builder, scope, block_item_list->val.statement);
				break;
			}

			block_item_list = block_item_list->next;
		}

		break;
	}
	case RETURN_STATEMENT: {
		Value value = ir_gen_expression(builder, scope, statement->val.expr);
		IrBlock *ret_block = &builder->function->ret_block;
		build_branch(builder, ret_block, value);
		break;
	}
	default:
		UNIMPLEMENTED;
	}
}

static Value ir_gen_expression(Builder *builder, Scope *scope, ASTExpr *expr)
{
	IGNORE(builder);

	switch (expr->type) {
	case INT_LITERAL_EXPR:
		// @TODO: Determine types of constants correctly.
		return value_const((IrType) { .kind = IR_INT, .val.bit_width = 32 },
				expr->val.int_literal);
	case BIT_XOR_EXPR:
		return build_binary_instr(
				builder,
				OP_BIT_XOR,
				ir_gen_expression(builder, scope, expr->val.binary_op.arg1),
				ir_gen_expression(builder, scope, expr->val.binary_op.arg2));
	case MULTIPLY_EXPR:
		return build_binary_instr(
				builder,
				// @TODO: generate fmuls for float operands
				// @TODO: and muls for unsigned operands
				OP_IMUL,
				ir_gen_expression(builder, scope, expr->val.binary_op.arg1),
				ir_gen_expression(builder, scope, expr->val.binary_op.arg2));
	case IDENTIFIER_EXPR: {
		Binding *binding = binding_for_name(scope, expr->val.identifier);
		return build_load(builder, binding->address, c_type_to_ir_type(&binding->type));
	}
	default:
		UNIMPLEMENTED;
	}
}
