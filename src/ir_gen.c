#include <assert.h>
#include <string.h>
#include <stdlib.h>

#include "ir.h"
#include "parse.h"

static void ir_gen_statement(Builder *builder, ASTStatement *statement);
static Value ir_gen_expression(Builder *builder, ASTExpression *expr);

static struct { const char *name; IrType type; } primitive_types[] =
{
	{ "int", { .bit_width = 32 } },
};

static IrType look_up_type(ASTType *type_spec)
{
	for (u32 i = 0; i < ARRAY_SIZE(primitive_types); i++)
		if (strcmp(type_spec->name, primitive_types[i].name) == 0)
			return primitive_types[i].type;

	UNREACHABLE;
}

void ir_gen_function(TransUnit *tu, Builder *builder, ASTToplevel *ast)
{
	Array(ASTVar *) *arguments = &ast->val.function_def.arguments;
	u32 arity = arguments->size;

	IrType *arg_types = malloc(sizeof(*arg_types) * arity);
	for (u32 i = 0; i < arity; i++) {
		ASTVar *var = *ARRAY_REF(arguments, ASTVar *, i);
		arg_types[i] = look_up_type(var->type);
	}

	IrType return_type = look_up_type(ast->val.function_def.return_type);

	Function *f = trans_unit_add_function(tu, ast->val.function_def.name,
			return_type, arity, arg_types);
	builder->function = f;
	builder->current_block = &f->entry_block;

	ir_gen_statement(builder, ast->val.function_def.body);
}

static void ir_gen_statement(Builder *builder, ASTStatement *statement)
{
	switch (statement->type) {
	case AST_COMPOUND_STATEMENT: {
		Array(ASTStatement *) *statements = &statement->val.statements;
		for (u32 i = 0; i < statements->size; i++) {
			ASTStatement *sub_statement =
				*ARRAY_REF(statements, ASTStatement *, i);

			ir_gen_statement(builder, sub_statement);
		}

		break;
	}

	case AST_RETURN_STATEMENT: {
		Value value = ir_gen_expression(builder, statement->val.return_value);
		Block *ret_block = &builder->function->ret_block;
		build_branch(builder, ret_block, value);
		break;
	}
	}
}

static Value ir_gen_expression(Builder *builder, ASTExpression *expr)
{
	UNUSED(builder);

	switch (expr->type) {
	case AST_INTEGER_LITERAL:
		return value_const(expr->val.integer_literal);
	}
}
