#include <assert.h>
#include <string.h>
#include <stdlib.h>

#include "array.h"
#include "ir.h"
#include "parse.h"
#include "util.h"

typedef enum CTypeType
{
	INTEGER_TYPE,
	FUNCTION_TYPE,
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
		struct
		{
			struct CType *return_type;
			struct CType *arg_type_array;
			u32 arity;
		} function;
	} val;
} CType;

typedef struct Term
{
	CType ctype;
	IrValue value;
} Term;

typedef struct Binding
{
	char *name;
	Term term;
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

static IrType c_type_to_ir_type(CType *ctype)
{
	switch (ctype->type) {
	case INTEGER_TYPE: {
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
	case FUNCTION_TYPE:
		return (IrType) { .kind = FUNCTION_TYPE };
	}
}

static void cdecl_to_binding(IrBuilder *builder, CDecl *cdecl, Binding *binding)
{
	IrType ir_type = c_type_to_ir_type(&cdecl->type);

	binding->name = cdecl->name;
	binding->term.ctype = cdecl->type;
	binding->term.value = build_local(builder, ir_type);
}

static inline IrBlock *add_block(IrBuilder *builder, char *name)
{
	return add_block_to_function(builder->trans_unit, builder->current_function, name);
}

static IrGlobal *ir_global_for_function(IrBuilder *builder,
		ASTDeclSpecifier *decl_specifier_list, ASTDeclarator *declarator,
		CType *result_c_type)
{
	CDecl cdecl;
	decl_to_cdecl(decl_specifier_list, declarator, &cdecl);
	CType return_c_type = cdecl.type;
	IrType return_ir_type = c_type_to_ir_type(&return_c_type);

	assert(declarator->type == DIRECT_DECLARATOR);
	ASTDirectDeclarator *direct_declarator =
		declarator->val.direct_declarator;
	assert(direct_declarator->type == FUNCTION_DECLARATOR);

	ASTParameterDecl *first_param =
		direct_declarator->val.function_declarator.parameters;
	ASTParameterDecl *params = first_param;

	u32 arity = 0;
	while (params != NULL) {
		arity++;
		params = params->next;
	}

	params = first_param;

	IrType *arg_ir_types = malloc(sizeof(*arg_ir_types) * arity);
	CType *arg_c_types = pool_alloc(
			&builder->trans_unit->pool,
			sizeof(*arg_c_types) * arity);

	for (u32 i = 0; i < arity; i++) {
		CDecl cdecl;
		decl_to_cdecl(params->decl_specifier_list, params->declarator, &cdecl);
		arg_ir_types[i] = c_type_to_ir_type(&cdecl.type);
		arg_c_types[i] = cdecl.type;

		params = params->next;
	}

	// @TODO: Search through the currently defined globals for one of the same
	// name. If found, check that the CType matches and use that one instead of
	// adding a new one.
	IrGlobal *global = trans_unit_add_function(
			builder->trans_unit,
			direct_declarator->val.function_declarator.declarator->val.name,
			return_ir_type, arity, arg_ir_types);

	assert(global->kind == IR_GLOBAL_FUNCTION);

	CType *pool_alloced_return_c_type = pool_alloc(&builder->trans_unit->pool,
			sizeof *pool_alloced_return_c_type);
	*pool_alloced_return_c_type = return_c_type;

	result_c_type->type = FUNCTION_TYPE;
	result_c_type->val.function.arity = arity;
	result_c_type->val.function.arg_type_array = arg_c_types;
	result_c_type->val.function.return_type = pool_alloced_return_c_type;

	return global;
}

static void ir_gen_statement(IrBuilder *builder, Scope *scope, ASTStatement *statement);
static Term ir_gen_expression(IrBuilder *builder, Scope *scope, ASTExpr *expr);

void ir_gen_toplevel(IrBuilder *builder, ASTToplevel *toplevel)
{
	Scope global_scope;
	global_scope.parent_scope = NULL;
	Array(Binding)* global_bindings = &global_scope.bindings;
	ARRAY_INIT(global_bindings, Binding, 10);

	while (toplevel != NULL) {
		IrGlobal *global;
		CType global_type;
		ZERO_STRUCT(&global_type);

		switch (toplevel->type) {
		case FUNCTION_DEF: {
			ASTFunctionDef *func = toplevel->val.function_def;
			ASTDeclSpecifier *decl_specifier_list = func->decl_specifier_list;
			ASTDeclarator *declarator = func->declarator;

			global = ir_global_for_function(builder, decl_specifier_list,
					declarator, &global_type);
			IrFunction *function = &global->val.function;

			builder->current_function = function;
			builder->current_block = *ARRAY_REF(&function->blocks, IrBlock *, 0);

			Scope scope;
			scope.parent_scope = &global_scope;
			Array(Binding) *param_bindings = &scope.bindings;
			ARRAY_INIT(param_bindings, Binding, 5);

			ASTDirectDeclarator *direct_declarator =
				declarator->val.direct_declarator;
			assert(direct_declarator->type == FUNCTION_DECLARATOR);

			ASTParameterDecl *param =
				direct_declarator->val.function_declarator.parameters;
			for (u32 i = 0; param != NULL; i++, param = param->next) {
				Binding *binding = ARRAY_APPEND(param_bindings, Binding);

				CDecl cdecl;
				decl_to_cdecl(param->decl_specifier_list, param->declarator, &cdecl);
				cdecl_to_binding(builder, &cdecl, binding);

				build_store(builder,
						binding->term.value,
						value_arg(i, function->arg_types[i]),
						c_type_to_ir_type(&cdecl.type));
			}

			ir_gen_statement(builder, &scope, func->body);

			array_free(param_bindings);
			global->defined = true;

			break;
		}
		case DECL: {
			ASTDecl *decl = toplevel->val.decl;
			ASTDeclSpecifier *decl_specifier_list = decl->decl_specifier_list;
			ASTInitDeclarator *init_declarator = decl->init_declarators;
			assert(init_declarator->initializer == NULL);
			assert(init_declarator->next == NULL);
			ASTDeclarator *declarator = init_declarator->declarator;

			global = ir_global_for_function(builder, decl_specifier_list,
					declarator, &global_type);
			global->defined = false;

			break;
		}
		}

		Binding *binding = ARRAY_APPEND(global_bindings, Binding);
		binding->name = global->name;
		binding->term.ctype = global_type;
		binding->term.value = value_global(global);

		toplevel = toplevel->next;
	}

	array_free(global_bindings);
}

static void add_decl_to_scope(IrBuilder *builder, Scope *scope, ASTDecl *decl)
{
	ASTInitDeclarator *init_declarator = decl->init_declarators;
	while (init_declarator != NULL) {
		CDecl cdecl;
		decl_to_cdecl(decl->decl_specifier_list,
				init_declarator->declarator,
				&cdecl);

		Binding *binding = ARRAY_APPEND(&scope->bindings, Binding);
		cdecl_to_binding(builder, &cdecl, binding);

		ASTInitializer *initializer = init_declarator->initializer;
		if (initializer != NULL) {
			assert(initializer->type == EXPR_INITIALIZER);
			build_store(
				builder,
				binding->term.value,
				ir_gen_expression(builder, scope, initializer->val.expr).value,
				c_type_to_ir_type(&binding->term.ctype));
		}

		init_declarator = init_declarator->next;
	}
}

static void ir_gen_statement(IrBuilder *builder, Scope *scope, ASTStatement *statement)
{
	switch (statement->type) {
	case COMPOUND_STATEMENT: {
		Scope block_scope;
		block_scope.parent_scope = scope;
		ARRAY_INIT(&block_scope.bindings, Binding, 5);

		ASTBlockItem *block_item_list = statement->val.block_item_list;
		while (block_item_list != NULL) {
			switch (block_item_list->type) {
			case BLOCK_ITEM_DECL: {
				add_decl_to_scope(builder, &block_scope, block_item_list->val.decl);
				break;
			}
			case BLOCK_ITEM_STATEMENT:
				ir_gen_statement(builder, &block_scope, block_item_list->val.statement);
				break;
			}

			block_item_list = block_item_list->next;
		}

		break;
	}
	case EXPR_STATEMENT: {
		ir_gen_expression(builder, scope, statement->val.expr);
		break;
	}
	case RETURN_STATEMENT: {
		Term term = ir_gen_expression(builder, scope, statement->val.expr);
		build_unary_instr(builder, OP_RET, term.value);
		break;
	}
	case IF_STATEMENT: {
		IrBlock *initial_block = builder->current_block;
		IrBlock *then_block = add_block(builder, "if.then");
		IrBlock *after_block = add_block(builder, "if.after");

		ASTStatement *then_statement = statement->val.if_statement.then_statement;
		builder->current_block = then_block;
		ir_gen_statement(builder, scope, then_statement);
		build_branch(builder, after_block);

		ASTStatement *else_statement = statement->val.if_statement.else_statement;
		IrBlock *else_block = NULL;
		if (else_statement != NULL) {
			else_block = add_block(builder, "if.else");
			builder->current_block = else_block;
			ir_gen_statement(builder, scope, else_statement);
			build_branch(builder, after_block);
		}

		builder->current_block = initial_block;
		ASTExpr *condition_expr = statement->val.if_statement.condition;
		Term condition_term = ir_gen_expression(builder, scope, condition_expr);
		assert(condition_term.ctype.type == INTEGER_TYPE);

		if (else_statement == NULL) {
			build_cond(builder, condition_term.value, then_block, after_block);
		} else {
			build_cond(builder, condition_term.value, then_block, else_block);
		}

		builder->current_block = after_block;
		break;
	}
	case WHILE_STATEMENT: {
		IrBlock *pre_header = add_block(builder, "while.ph");
		IrBlock *body = add_block(builder, "while.body");
		IrBlock *after = add_block(builder, "while.after");

		ASTExpr *condition_expr = statement->val.expr_and_statement.expr;
		ASTStatement *body_statement = statement->val.expr_and_statement.statement;

		build_branch(builder, pre_header);
		builder->current_block = pre_header;
		Term condition_term = ir_gen_expression(builder, scope, condition_expr);

		assert(condition_term.ctype.type == INTEGER_TYPE);
		build_cond(builder, condition_term.value, body, after);

		builder->current_block = body;
		ir_gen_statement(builder, scope, body_statement);
		build_branch(builder, pre_header);

		builder->current_block = after;

		break;
	}
	case FOR_STATEMENT: {
		IrBlock *pre_header = add_block(builder, "for.ph");
		IrBlock *body = add_block(builder, "for.body");
		IrBlock *after = add_block(builder, "for.after");

		Scope init_scope;
		Scope *for_scope;

		ASTForStatement *f = &statement->val.for_statement;
		if (f->init_type == FOR_INIT_DECL) {
			init_scope.parent_scope = scope;
			ARRAY_INIT(&init_scope.bindings, Binding, 1);
			add_decl_to_scope(builder, &init_scope, f->init.decl);

			for_scope = &init_scope;
		} else {
			assert(f->init_type == FOR_INIT_EXPR);
			ir_gen_expression(builder, scope, f->init.expr);
			for_scope = scope;
		}

		build_branch(builder, pre_header);
		builder->current_block = pre_header;
		Term condition_term = ir_gen_expression(builder, for_scope, f->condition);

		assert(condition_term.ctype.type == INTEGER_TYPE);
		build_cond(builder, condition_term.value, body, after);

		builder->current_block = body;
		ir_gen_statement(builder, for_scope, f->body);
		ir_gen_expression(builder, for_scope, f->update_expr);
		build_branch(builder, pre_header);

		builder->current_block = after;

		break;
	}
	default:
		UNIMPLEMENTED;
	}
}

static Term ir_gen_expression(IrBuilder *builder, Scope *scope, ASTExpr *expr)
{
	IGNORE(builder);

	switch (expr->type) {
	case INT_LITERAL_EXPR: {
		// @TODO: Determine types of constants correctly.
		CType result_type = {
			.type = INTEGER_TYPE,
			.val.integer.type = INT,
			.val.integer.is_signed = true,
		};

		IrValue value = value_const(
				(IrType) { .kind = IR_INT, .val.bit_width = 32 },
				expr->val.int_literal);

		return (Term) { .ctype = result_type, .value = value };
	}
	case BIT_XOR_EXPR: {
		// @TODO: Determine type correctly.
		CType result_type = {
			.type = INTEGER_TYPE,
			.val.integer.type = INT,
			.val.integer.is_signed = true,
		};

		IrValue value = build_binary_instr(
				builder,
				OP_BIT_XOR,
				ir_gen_expression(builder, scope, expr->val.binary_op.arg1).value,
				ir_gen_expression(builder, scope, expr->val.binary_op.arg2).value);

		return (Term) { .ctype = result_type, .value = value };
	}
	case MULTIPLY_EXPR: {
		// @TODO: Determine type correctly.
		CType result_type = {
			.type = INTEGER_TYPE,
			.val.integer.type = INT,
			.val.integer.is_signed = true,
		};

		IrValue value = build_binary_instr(
				builder,
				OP_MUL,
				ir_gen_expression(builder, scope, expr->val.binary_op.arg1).value,
				ir_gen_expression(builder, scope, expr->val.binary_op.arg2).value);

		return (Term) { .ctype = result_type, .value = value };
	}
	case ADD_EXPR: {
		// @TODO: Determine type correctly.
		CType result_type = {
			.type = INTEGER_TYPE,
			.val.integer.type = INT,
			.val.integer.is_signed = true,
		};

		IrValue value = build_binary_instr(
				builder,
				OP_ADD,
				ir_gen_expression(builder, scope, expr->val.binary_op.arg1).value,
				ir_gen_expression(builder, scope, expr->val.binary_op.arg2).value);

		return (Term) { .ctype = result_type, .value = value };
	}
	case EQUAL_EXPR: {
		CType result_type = {
			.type = INTEGER_TYPE,
			.val.integer.type = INT,
			.val.integer.is_signed = true,
		};

		IrValue value = build_binary_instr(
				builder,
				OP_EQ,
				ir_gen_expression(builder, scope, expr->val.binary_op.arg1).value,
				ir_gen_expression(builder, scope, expr->val.binary_op.arg2).value);

		return (Term) { .ctype = result_type, .value = value };
	}
	case NOT_EQUAL_EXPR: {
		CType result_type = {
			.type = INTEGER_TYPE,
			.val.integer.type = INT,
			.val.integer.is_signed = true,
		};

		IrValue value = build_binary_instr(
				builder,
				OP_NEQ,
				ir_gen_expression(builder, scope, expr->val.binary_op.arg1).value,
				ir_gen_expression(builder, scope, expr->val.binary_op.arg2).value);

		return (Term) { .ctype = result_type, .value = value };
	}
	case IDENTIFIER_EXPR: {
		Binding *binding = binding_for_name(scope, expr->val.identifier);
		assert(binding != NULL);
		IrValue value;

		// Functions implicitly have their address taken.
		if (binding->term.ctype.type == FUNCTION_TYPE) {
			value = binding->term.value;
		} else {
			value = build_load(
					builder,
					binding->term.value,
					c_type_to_ir_type(&binding->term.ctype));
		}

		return (Term) { .ctype = binding->term.ctype, .value = value };
	}
	case FUNCTION_CALL_EXPR: {
		Term callee = ir_gen_expression(builder, scope, expr->val.function_call.callee);

		u32 arity = 0;
		ASTArgument *arg = expr->val.function_call.arg_list;
		while (arg != NULL) {
			arity++;
			arg = arg->next;
		}

		assert(callee.ctype.type == FUNCTION_TYPE);

		CType *return_type = callee.ctype.val.function.return_type;
		IrValue *arg_array = pool_alloc(&builder->trans_unit->pool,
				arity * sizeof(*arg_array));

		arg = expr->val.function_call.arg_list;
		for (u32 i = 0; arg != NULL; i++, arg = arg->next) {
			Term arg_term = ir_gen_expression(builder, scope, arg->expr);
			arg_array[i] = arg_term.value;
		}

		IrValue value = build_call(
				builder,
				callee.value,
				c_type_to_ir_type(return_type),
				arity,
				arg_array);

		return (Term) { .ctype = *return_type, .value = value };
	}
	case ASSIGN_EXPR: {
		ASTExpr *lhs = expr->val.binary_op.arg1;
		ASTExpr *rhs = expr->val.binary_op.arg2;

		Term rhs_term = ir_gen_expression(builder, scope, rhs);

		assert(lhs->type == IDENTIFIER_EXPR);
		Binding *binding = binding_for_name(scope, lhs->val.identifier);
		build_store(
				builder,
				binding->term.value,
				rhs_term.value,
				c_type_to_ir_type(&binding->term.ctype));
		return rhs_term;
	}
	case COMMA_EXPR:
		ir_gen_expression(builder, scope, expr->val.binary_op.arg1);
		return ir_gen_expression(builder, scope, expr->val.binary_op.arg2);
	default:
		printf("%d\n", expr->type);
		UNIMPLEMENTED;
	}
}
