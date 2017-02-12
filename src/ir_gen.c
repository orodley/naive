#include <assert.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>

#include "array.h"
#include "ir.h"
#include "parse.h"
#include "util.h"

typedef enum CTypeType
{
	VOID_TYPE,
	INTEGER_TYPE,
	FUNCTION_TYPE,
	STRUCT_TYPE,
	POINTER_TYPE,
	ARRAY_TYPE,
} CTypeType;

typedef struct CType
{
	CTypeType t;

	union
	{
		struct
		{
			enum
			{
				BOOL,
				CHAR,
				SHORT,
				INT,
				LONG,
				LONG_LONG,
			} type;
			bool is_signed;
		} integer;
		struct
		{
			struct CType *return_type;
			struct CType **arg_type_array;
			u32 arity;
			bool variable_arity;
		} function;
		struct
		{
			bool incomplete;
			Array(CDecl) fields;
			IrType *ir_type;
		} strukt;
		struct CType *pointee_type;
		struct
		{
			struct CType *elem_type;
			bool incomplete;
			u64 size;
			IrType *ir_type;
		} array;
	} u;
} CType;

typedef struct CDecl
{
	char *name;
	CType *type;
} CDecl;

static bool c_type_eq(CType *a, CType *b)
{
	if (a->t != b->t)
		return false;

	switch (a->t) {
	case VOID_TYPE:
		return true;
	case INTEGER_TYPE:
		return a->u.integer.type == b->u.integer.type
			&& a->u.integer.is_signed == b->u.integer.is_signed;
	case FUNCTION_TYPE:
		if (a->u.function.arity != b->u.function.arity)
			return false;
		if (!c_type_eq(a->u.function.return_type, b->u.function.return_type))
			return false;
		for (u32 i = 0; i < a->u.function.arity; i++) {
			if (!c_type_eq(a->u.function.arg_type_array[i],
						b->u.function.arg_type_array[i]))
				return false;
		}

		return true;
	case STRUCT_TYPE:
		return a->u.strukt.ir_type == b->u.strukt.ir_type;
	case POINTER_TYPE:
		assert(a != a->u.pointee_type);
		assert(b != b->u.pointee_type);
		return c_type_eq(a->u.pointee_type, b->u.pointee_type);
	case ARRAY_TYPE:
		return a->u.array.size == b->u.array.size
			&& c_type_eq(a->u.array.elem_type, b->u.array.elem_type);
	}
}

static bool c_type_compatible(CType *a, CType *b)
{
	if (c_type_eq(a, b))
		return true;

	if (a->t == POINTER_TYPE && b->t == POINTER_TYPE
			&& (a->u.pointee_type->t == VOID_TYPE
				|| b->u.pointee_type->t == VOID_TYPE)) {
		return true;
	}

	return false;
}

static IrType c_type_to_ir_type(CType *ctype)
{
	switch (ctype->t) {
	case VOID_TYPE:
		return (IrType) { .t = IR_VOID };
	case INTEGER_TYPE: {
		u32 bit_width;
		switch (ctype->u.integer.type) {
		case BOOL: case CHAR: bit_width = 8; break;
		case SHORT: bit_width = 16; break;
		case INT: bit_width = 32; break;
		case LONG: case LONG_LONG: bit_width = 64; break;
		default: UNIMPLEMENTED;
		}

		return (IrType) {
			.t = IR_INT,
			.u.bit_width = bit_width,
		};
	}
	case POINTER_TYPE:
		return (IrType) { .t = IR_POINTER };
	case ARRAY_TYPE:
		assert(!ctype->u.array.incomplete);
		return *ctype->u.array.ir_type;
	case FUNCTION_TYPE:
		return (IrType) { .t = FUNCTION_TYPE };
	case STRUCT_TYPE:
		return *ctype->u.strukt.ir_type;
	}
}

static u8 rank(CType *type)
{
	assert(type->t == INTEGER_TYPE);
	switch (type->u.integer.type) {
	case BOOL: return 0;
	case CHAR: return 1;
	case SHORT: return 2;
	case INT: return 3;
	case LONG: return 4;
	case LONG_LONG: return 5;
	}
}

typedef struct Term
{
	CType *ctype;
	IrValue value;
} Term;

typedef struct Binding
{
	char *name;
	bool constant;
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

typedef struct TypeEnvEntry
{
	char *name;
	CType type;
} TypeEnvEntry;

typedef struct TypeEnv
{
	Pool pool;
	Array(TypeEnvEntry *) struct_types;
	Array(TypeEnvEntry *) union_types;
	Array(TypeEnvEntry *) enum_types;
	Array(TypeEnvEntry *) typedef_types;

	CType void_type;
	CType char_type;
	CType unsigned_char_type;
	CType bool_type;
	CType int_type;
	CType unsigned_int_type;
	CType short_type;
	CType unsigned_short_type;
	CType long_type;
	CType unsigned_long_type;
	CType long_long_type;
	CType unsigned_long_long_type;
} TypeEnv;

static void init_type_env(TypeEnv *type_env)
{
	pool_init(&type_env->pool, 512);
	ARRAY_INIT(&type_env->struct_types, TypeEnvEntry *, 10);
	ARRAY_INIT(&type_env->union_types, TypeEnvEntry *, 10);
	ARRAY_INIT(&type_env->enum_types, TypeEnvEntry *, 10);
	ARRAY_INIT(&type_env->typedef_types, TypeEnvEntry *, 10);

	type_env->void_type = (CType) {
		.t = VOID_TYPE,
	};
	type_env->bool_type = (CType) {
		.t = INTEGER_TYPE,
		.u.integer.type = BOOL,
		.u.integer.is_signed = false,
	};
	type_env->char_type = (CType) {
		.t = INTEGER_TYPE,
		.u.integer.type = CHAR,
		// System V x86-64 says "char" == "signed char"
		.u.integer.is_signed = true,
	};
	type_env->unsigned_char_type = (CType) {
		.t = INTEGER_TYPE,
		.u.integer.type = CHAR,
		.u.integer.is_signed = false,
	};
	type_env->short_type = (CType) {
		.t = INTEGER_TYPE,
		.u.integer.type = SHORT,
		.u.integer.is_signed = true,
	};
	type_env->unsigned_short_type = (CType) {
		.t = INTEGER_TYPE,
		.u.integer.type = SHORT,
		.u.integer.is_signed = false,
	};
	type_env->int_type = (CType) {
		.t = INTEGER_TYPE,
		.u.integer.type = INT,
		.u.integer.is_signed = true,
	};
	type_env->unsigned_int_type = (CType) {
		.t = INTEGER_TYPE,
		.u.integer.type = INT,
		.u.integer.is_signed = false,
	};
	type_env->long_type = (CType) {
		.t = INTEGER_TYPE,
		.u.integer.type = LONG,
		.u.integer.is_signed = true,
	};
	type_env->unsigned_long_type = (CType) {
		.t = INTEGER_TYPE,
		.u.integer.type = LONG,
		.u.integer.is_signed = false,
	};
	type_env->long_long_type = (CType) {
		.t = INTEGER_TYPE,
		.u.integer.type = LONG_LONG,
		.u.integer.is_signed = true,
	};
	type_env->unsigned_long_long_type = (CType) {
		.t = INTEGER_TYPE,
		.u.integer.type = LONG_LONG,
		.u.integer.is_signed = false,
	};
}

static CType *search(Array(TypeEnvEntry *) *types, char *name)
{
	for (u32 i = 0; i < types->size; i++) {
		TypeEnvEntry *entry = *ARRAY_REF(types, TypeEnvEntry *, i);
		if (streq(entry->name, name)) {
			return &entry->type;
		}
	}
	return NULL;
}

static CType *pointer_type(TypeEnv *type_env, CType *type)
{
	CType *pointer_type = pool_alloc(&type_env->pool, sizeof *pointer_type);
	pointer_type->t = POINTER_TYPE;
	pointer_type->u.pointee_type = type;

	return pointer_type;
}

static CType *decay_to_pointer(TypeEnv *type_env, CType *type)
{
	if (type->t == ARRAY_TYPE) {
		return pointer_type(type_env, type->u.array.elem_type);
	} else  {
		return type;
	}
}

static void set_array_type_length(CType *array_type, u64 size)
{
	assert(array_type->t == ARRAY_TYPE);

	array_type->u.array.size = size;
	if (array_type->u.array.elem_type->t == ARRAY_TYPE) {
		size *= array_type->u.array.elem_type->u.array.size;
	}

	array_type->u.array.size = size;
	array_type->u.array.ir_type->u.array.size = size;
	array_type->u.array.incomplete = false;
}

static CType *array_type(IrBuilder *builder, TypeEnv *type_env, CType *type)
{
	CType *array_type = pool_alloc(&type_env->pool, sizeof *array_type);
	array_type->t = ARRAY_TYPE;
	array_type->u.array.elem_type = type;
	array_type->u.array.incomplete = true;

	IrType *ir_array_type =
		pool_alloc(&builder->trans_unit->pool, sizeof *ir_array_type);
	ir_array_type->t = IR_ARRAY;

	IrType elem_type = c_type_to_ir_type(type);
	IrType *ir_elem_type;

	if (elem_type.t == IR_ARRAY) {
		ir_elem_type = elem_type.u.array.elem_type;
	} else {
		ir_elem_type =
			pool_alloc(&builder->trans_unit->pool, sizeof *ir_elem_type);
		*ir_elem_type = c_type_to_ir_type(type);
	}
	ir_array_type->u.array.elem_type = ir_elem_type;
	array_type->u.array.ir_type = ir_array_type;

	return array_type;
}

static CType *struct_type(TypeEnv *type_env, char *name)
{
	TypeEnvEntry *entry = pool_alloc(&type_env->pool, sizeof *entry);
	*ARRAY_APPEND(&type_env->struct_types, TypeEnvEntry *) = entry;
	if (name == NULL)
		name = "<anonymous struct>";
	entry->name = name;

	CType *type = &entry->type;
	type->t = STRUCT_TYPE;
	// Every struct starts out incomplete, until we add the fields.
	type->u.strukt.incomplete = true;
	ARRAY_INIT(&type->u.strukt.fields, CDecl, 5);

	return type;
}

typedef struct SwitchCase
{
	bool is_default;
	IrConst *value;
	IrBlock *block;
} SwitchCase;

typedef struct GotoLabel
{
	char *name;
	IrBlock *block;
} GotoLabel;

typedef struct GotoFixup
{
	char *label_name;
	IrInstr *instr;
} GotoFixup;

typedef struct InlineFunction
{
	IrGlobal *global;
	CType *function_type;
	ASTFunctionDef function_def;
} InlineFunction;

typedef struct Env
{
	Scope *scope;
	TypeEnv type_env;
	Array(InlineFunction) inline_functions;
	Array(SwitchCase) case_labels;
	Array(GotoLabel) goto_labels;
	Array(GotoFixup) goto_fixups;
	IrBlock *break_target;
	IrBlock *continue_target;
} Env;

static IrConst *eval_constant_expr(IrBuilder *builder, TypeEnv *type_env,
		ASTExpr *constant_expr)
{
	switch (constant_expr->t) {
	case INT_LITERAL_EXPR:
		// @TODO: Determine type properly.
		return add_int_const(builder, c_type_to_ir_type(&type_env->int_type),
				constant_expr->u.int_literal);
	default:
		UNIMPLEMENTED;
	}
}

static void decl_to_cdecl(IrBuilder *builder, Env *env,
		CType *decl_spec_type, ASTDeclarator *declarator, CDecl *cdecl);

static bool matches_sequence(ASTDeclSpecifier *decl_specifier_list, int length, ...)
{
	va_list args;
	va_start(args, length);

	while (length != 0) {
		if (decl_specifier_list == NULL) {
			va_end(args);
			return false;
		}

		char *str = va_arg(args, char *);
		length--;

		assert(decl_specifier_list->t == TYPE_SPECIFIER);
		ASTTypeSpecifier *type_spec = decl_specifier_list->u.type_specifier;
		if (type_spec->t != NAMED_TYPE_SPECIFIER
				|| !streq(type_spec->u.name, str)) {
			va_end(args);
			return false;
		}

		decl_specifier_list = decl_specifier_list->next;
	}

	va_end(args);

	// Only return true if we consumed all of the type specifiers, because some
	// sequences are prefixes of other sequences.
	return decl_specifier_list == NULL;
}

static CType *decl_specifier_list_to_c_type(IrBuilder *builder, Env *env,
		ASTDeclSpecifier *decl_specifier_list)
{
	TypeEnv *type_env = &env->type_env;

	// @TODO: Actually handle type qualifiers rather than ignoring them.
	while (decl_specifier_list != NULL &&
			decl_specifier_list->t == TYPE_QUALIFIER) {
		decl_specifier_list = decl_specifier_list->next;
	}

	assert(decl_specifier_list != NULL);
	assert(decl_specifier_list->t == TYPE_SPECIFIER);

	ASTTypeSpecifier *type_spec = decl_specifier_list->u.type_specifier;

	switch (type_spec->t) {
	case NAMED_TYPE_SPECIFIER: {
		// @TODO: This would be more efficiently (but perhaps less readably?)
		// encoded as a tree, so as to eliminate redundant comparisons.
		if (matches_sequence(decl_specifier_list, 1, "void")) {
			return &type_env->void_type;
		}
		if (matches_sequence(decl_specifier_list, 1, "char")
				|| matches_sequence(decl_specifier_list, 2, "signed", "char")) {
			return &type_env->char_type;
		}
		if (matches_sequence(decl_specifier_list, 2, "unsigned", "char")) {
			return &type_env->unsigned_char_type;
		}
		if (matches_sequence(decl_specifier_list, 1, "_Bool")) {
			return &type_env->bool_type;
		}
		if (matches_sequence(decl_specifier_list, 1, "short")
				|| matches_sequence(decl_specifier_list, 2, "signed", "short")
				|| matches_sequence(decl_specifier_list, 2, "short", "int")
				|| matches_sequence(decl_specifier_list, 3, "signed", "short", "int")) {
			return &type_env->short_type;
		}
		if (matches_sequence(decl_specifier_list, 2, "unsigned", "short")
				|| matches_sequence(decl_specifier_list, 3, "unsigned", "short", "int")) {
			return &type_env->unsigned_short_type;
		}
		if (matches_sequence(decl_specifier_list, 1, "int")
				|| matches_sequence(decl_specifier_list, 1, "signed")
				|| matches_sequence(decl_specifier_list, 2, "signed", "int")) {
			return &type_env->int_type;
		}
		if (matches_sequence(decl_specifier_list, 1, "unsigned")
				|| matches_sequence(decl_specifier_list, 2, "unsigned", "int")) {
			return &type_env->unsigned_int_type;
		}
		if (matches_sequence(decl_specifier_list, 1, "long")
				|| matches_sequence(decl_specifier_list, 2, "signed", "long")
				|| matches_sequence(decl_specifier_list, 2, "long", "int")
				|| matches_sequence(decl_specifier_list, 3, "signed", "long", "int")) {
			return &type_env->long_type;
		}
		if (matches_sequence(decl_specifier_list, 2, "unsigned", "long")
				|| matches_sequence(decl_specifier_list, 3, "unsigned", "long", "int")) {
			return &type_env->unsigned_long_type;
		}
		if (matches_sequence(decl_specifier_list, 2, "long", "long")
				|| matches_sequence(decl_specifier_list, 3, "signed", "long", "long")
				|| matches_sequence(decl_specifier_list, 3, "long", "long", "int")
				|| matches_sequence(decl_specifier_list, 4, "signed", "long", "long", "int")) {
			return &type_env->long_long_type;
		}
		if (matches_sequence(decl_specifier_list, 3, "unsigned", "long", "long")
				|| matches_sequence(decl_specifier_list, 4, "unsigned", "long", "long", "int")) {
			return &type_env->unsigned_long_long_type;
		}

		// Phew

		CType *type = search(&type_env->typedef_types, type_spec->u.name);
		assert(type != NULL);
		return type;
	}
	case STRUCT_TYPE_SPECIFIER:
	case UNION_TYPE_SPECIFIER: {
		ASTFieldDecl *field_list =
			type_spec->u.struct_or_union_specifier.field_list;
		char *name = type_spec->u.struct_or_union_specifier.name;
		ASTAttribute *attribute = type_spec->u.struct_or_union_specifier.attribute;
		bool is_packed = attribute != NULL && streq(attribute->name, "packed");

		CType *existing_type = NULL;
		if (name != NULL) {
			// @TODO: Really we just want to search in the current scope; it's
			// perfectly valid to shadow a struct or union type from an
			// enclosing scope.
			existing_type = search(&type_env->struct_types, name);
		}

		if (field_list == NULL) {
			if (name == NULL) {
				assert(!"Error, no name or fields for struct or union type");
			} else if (existing_type == NULL) {
				// Incomplete type
				return struct_type(type_env, name);
			} else {
				return existing_type;
			}
		}
		CType *type;
		if (existing_type != NULL) {
			assert(existing_type->t == STRUCT_TYPE);
			if (!existing_type->u.strukt.incomplete)
				assert(!"Error, redefinition of struct or union type");

			type = existing_type;
		} else {
			type = struct_type(type_env, name);
		}
		Array(CDecl) *fields = &type->u.strukt.fields;

		while (field_list != NULL) {
			CType *decl_spec_type = decl_specifier_list_to_c_type(
					builder, env, field_list->decl_specifier_list);
			ASTFieldDeclarator *field_declarator = field_list->field_declarator_list;
			while (field_declarator != NULL) {
				assert(field_declarator->t == NORMAL_FIELD_DECLARATOR);
				ASTDeclarator *declarator = field_declarator->u.declarator;

				CDecl *cdecl = ARRAY_APPEND(fields, CDecl);
				decl_to_cdecl(builder, env, decl_spec_type, declarator, cdecl);

				field_declarator = field_declarator->next;
			}

			field_list = field_list->next;
		}

		IrType *ir_struct =
			trans_unit_add_struct(builder->trans_unit, name, fields->size);
		u32 current_offset = 0;
		u32 max_field_size = 0;
		u32 max_field_align = 0;
		for (u32 i = 0; i < fields->size; i++) {
			CDecl *field = ARRAY_REF(fields, CDecl, i);
			IrType field_type = c_type_to_ir_type(field->type);

			ir_struct->u.strukt.fields[i].type = field_type;

			u32 field_size = size_of_ir_type(field_type);
			u32 field_align = align_of_ir_type(field_type);
			max_field_size = max(max_field_size, field_size);
			max_field_align = max(max_field_align, field_align);

			if (type_spec->t == STRUCT_TYPE_SPECIFIER) {
				if (!is_packed)
					current_offset = align_to(current_offset, field_align);
				ir_struct->u.strukt.fields[i].offset = current_offset;

				current_offset += field_size;
			} else {
				ir_struct->u.strukt.fields[i].offset = 0;
			}
		}
		ir_struct->u.strukt.total_size = align_to(
				type_spec->t == STRUCT_TYPE_SPECIFIER
					? current_offset
					: max_field_size,
				is_packed ? 1 : max_field_align);
		ir_struct->u.strukt.alignment = is_packed ? 1 : max_field_align;

		type->u.strukt.ir_type = ir_struct;
		type->u.strukt.incomplete = false;

		return type;
	}
	case ENUM_TYPE_SPECIFIER: {
		char *tag = type_spec->u.enum_specifier.name;
		ASTEnumerator *enumerator_list =
			type_spec->u.enum_specifier.enumerator_list;

		// @TODO: Pick the type based on the largest member
		CType *ctype = &type_env->int_type;

		CType *existing_type = NULL;
		if (tag != NULL) {
			existing_type = search(&type_env->enum_types, tag);
		}

		if (enumerator_list == NULL) {
			if (tag == NULL) {
				assert(!"Error, no name or enumerators for enum type");
			} else if (existing_type == NULL) {
				// Incomplete type.
				// @TODO: This should be illegal to use, but for now we just
				// call it int
				return ctype;
			} else {
				return existing_type;
			}
		}
		// @TODO: Incomplete enum types.
		assert(existing_type == NULL);

		if (tag != NULL) {
			TypeEnvEntry *new_type_alias =
				pool_alloc(&env->type_env.pool, sizeof *new_type_alias);
			*ARRAY_APPEND(&env->type_env.enum_types, TypeEnvEntry *)
				= new_type_alias;
			new_type_alias->name = tag;
			new_type_alias->type = *ctype;
		}

		u64 curr_enum_value = 0;
		while (enumerator_list != NULL) {
			char *name = enumerator_list->name;
			ASTExpr *expr = enumerator_list->value;

			if (expr != NULL) {
				IrConst *value =
					eval_constant_expr(builder, &env->type_env, expr);
				assert(value->type.t == IR_INT);
				curr_enum_value = value->u.integer;
			}

			Binding *binding = ARRAY_APPEND(&env->scope->bindings, Binding);
			binding->name = name;
			binding->constant = true;
			binding->term.ctype = ctype;
			binding->term.value =
				value_const(c_type_to_ir_type(ctype), curr_enum_value++);

			enumerator_list = enumerator_list->next;
		}

		return ctype;
	}
	default: UNIMPLEMENTED;
	}
}

static ASTParameterDecl *params_for_function_declarator(ASTDeclarator *declarator)
{
	while (declarator->t != DIRECT_DECLARATOR) {
		assert(declarator->t == POINTER_DECLARATOR);
		declarator = declarator->u.pointer_declarator.pointee;
	}

	assert(declarator->t == DIRECT_DECLARATOR);
	ASTDirectDeclarator* direct_declarator = declarator->u.direct_declarator;
	assert(direct_declarator->t == FUNCTION_DECLARATOR);

	return direct_declarator->u.function_declarator.parameters;
}

typedef struct CDeclAux
{
	CType *type;
	CType **ident_type;
	char *name;
} CDeclAux;

static void decl_to_cdecl_aux(IrBuilder *builder, Env *env,
		CType *decl_spec_type, ASTDeclarator *declarator, CDeclAux *cdecl);

static void direct_declarator_to_cdecl(IrBuilder *builder, Env *env,
		CType *decl_spec_type, ASTDirectDeclarator *direct_declarator,
		CDeclAux *cdecl) {
	TypeEnv *type_env = &env->type_env;

	switch (direct_declarator->t) {
	case FUNCTION_DECLARATOR: {
		ASTParameterDecl *first_param =
			direct_declarator->u.function_declarator.parameters;
		ASTParameterDecl *params = first_param;

		u32 arity = 0;
		while (params != NULL) {
			switch (params->t) {
			case PARAMETER_DECL:
				arity++;
				break;
			case ELLIPSIS_DECL:
				assert(params->next == NULL);
				break;
			}
			params = params->next;
		}

		params = first_param;

		CType **arg_c_types = pool_alloc(
				&builder->trans_unit->pool,
				sizeof(*arg_c_types) * arity);

		bool variable_arity = false;
		for (u32 i = 0; params != NULL; i++) {
			switch (params->t) {
			case PARAMETER_DECL: {
				CType *decl_spec_type = decl_specifier_list_to_c_type(
						builder, env, params->decl_specifier_list);
				CDecl cdecl;
				decl_to_cdecl(
						builder, env, decl_spec_type, params->declarator, &cdecl);

				// As per 6.7.5.3.7, parameters of array type are adjusted to
				// pointers to the element type.
				cdecl.type = decay_to_pointer(type_env, cdecl.type);

				arg_c_types[i] = cdecl.type;
				break;
			}
			case ELLIPSIS_DECL:
				variable_arity = true;
				// Can't have more params after an ellipsis.
				assert(params->next == NULL);
				break;
			}

			params = params->next;
		}

		ASTDirectDeclarator *function_declarator =
			direct_declarator->u.function_declarator.declarator;
		CDeclAux func_name_cdecl;
		direct_declarator_to_cdecl(builder, env, decl_spec_type,
				function_declarator, &func_name_cdecl);

		CType *ctype = pool_alloc(&builder->trans_unit->pool, sizeof *ctype);
		ctype->t = FUNCTION_TYPE;
		ctype->u.function.arity = arity;
		ctype->u.function.variable_arity = variable_arity;
		ctype->u.function.arg_type_array = arg_c_types;
		ctype->u.function.return_type = *func_name_cdecl.ident_type;

		*func_name_cdecl.ident_type = ctype;
		cdecl->name = func_name_cdecl.name;
		cdecl->type = func_name_cdecl.type;
		cdecl->ident_type = &ctype->u.function.return_type;
		break;
	}
	case IDENTIFIER_DECLARATOR: {
		cdecl->name = direct_declarator->u.name;
		cdecl->type = decl_spec_type;
		cdecl->ident_type = &cdecl->type;
		break;
	}
	case ARRAY_DECLARATOR: {
		ASTDirectDeclarator *elem_declarator =
			direct_declarator->u.array_declarator.element_declarator;

		CDeclAux elem_cdecl;
		direct_declarator_to_cdecl(builder, env, decl_spec_type,
				elem_declarator, &elem_cdecl);

		CType *array = array_type(builder, type_env, *elem_cdecl.ident_type);
		*elem_cdecl.ident_type = array;
		cdecl->name = elem_cdecl.name;
		cdecl->type = elem_cdecl.type;
		cdecl->ident_type = &array->u.array.elem_type;

		ASTExpr *array_length_expr =
			direct_declarator->u.array_declarator.array_length;
		if (array_length_expr != NULL) {
			IrConst *length_const =
				eval_constant_expr(builder, type_env, array_length_expr);
			assert(length_const->type.t == IR_INT);
			u64 length = length_const->u.integer;

			set_array_type_length(array, length);
		}

		break;
	}
	case DECLARATOR:
		decl_to_cdecl_aux(builder, env,
				decl_spec_type, direct_declarator->u.declarator, cdecl);
		break;
	}
}

static void decl_to_cdecl_aux(IrBuilder *builder, Env *env,
		CType *decl_spec_type, ASTDeclarator *declarator, CDeclAux *cdecl)
{
	if (declarator == NULL) {
		cdecl->name = NULL;
		cdecl->type = decl_spec_type;
		cdecl->ident_type = &cdecl->type;
	} else if (declarator->t == POINTER_DECLARATOR) {
		CDeclAux pointee_cdecl;
		assert(declarator->u.pointer_declarator.decl_specifier_list == NULL);
		decl_to_cdecl_aux(builder, env, decl_spec_type,
				declarator->u.pointer_declarator.pointee, &pointee_cdecl);
		cdecl->name = pointee_cdecl.name;
		CType *ptr = pointer_type(&env->type_env, *pointee_cdecl.ident_type);
		*pointee_cdecl.ident_type = ptr;
		cdecl->ident_type = &ptr->u.pointee_type;
		cdecl->type = pointee_cdecl.type;
	} else {
		assert(declarator->t == DIRECT_DECLARATOR);
		ASTDirectDeclarator *direct_declarator = declarator->u.direct_declarator;

		direct_declarator_to_cdecl(builder, env, decl_spec_type,
				direct_declarator, cdecl);
	}
}

static void decl_to_cdecl(IrBuilder *builder, Env *env,
		CType *decl_spec_type, ASTDeclarator *declarator, CDecl *cdecl)
{
	CDeclAux cdecl_aux;
	decl_to_cdecl_aux(builder, env, decl_spec_type, declarator, &cdecl_aux);
	cdecl->name = cdecl_aux.name;
	cdecl->type = cdecl_aux.type;
}

static CType *type_name_to_c_type(
		IrBuilder *builder, Env *env, ASTTypeName *type_name)
{
	CDecl cdecl;
	CType *decl_spec_type = decl_specifier_list_to_c_type(
			builder, env, type_name->decl_specifier_list);
	decl_to_cdecl(builder, env, decl_spec_type,
			type_name->declarator, &cdecl);
	assert(cdecl.name == NULL);
	return cdecl.type;
}

static void cdecl_to_binding(IrBuilder *builder, CDecl *cdecl, Binding *binding)
{
	IrType ir_type = c_type_to_ir_type(cdecl->type);

	binding->name = cdecl->name;
	binding->constant = false;
	binding->term.ctype = cdecl->type;
	binding->term.value = build_local(builder, ir_type);
}

static inline IrBlock *add_block(IrBuilder *builder, char *name)
{
	return add_block_to_function(builder->trans_unit, builder->current_function, name);
}

static IrGlobal *ir_global_for_decl(IrBuilder *builder, Env *env,
		ASTDeclSpecifier *decl_specifier_list, ASTDeclarator *declarator,
		CType **result_c_type)
{
	CType *decl_spec_type =
		decl_specifier_list_to_c_type(builder, env, decl_specifier_list);
	CDecl cdecl;
	decl_to_cdecl(builder, env, decl_spec_type, declarator, &cdecl);
	CType *ctype = cdecl.type;
	if (ctype->t == FUNCTION_TYPE) {
		// Struct returns are handled in the frontend, by adding a pointer
		// parameter at the start, and allocating a local in the caller.
		bool struct_ret = ctype->u.function.return_type->t == STRUCT_TYPE;

		u32 arity = ctype->u.function.arity;
		if (struct_ret)
			arity++;

		IrType *arg_ir_types = malloc(sizeof(*arg_ir_types) * arity);
		
		u32 i = 0;
		if (struct_ret) {
			arg_ir_types[i++] = (IrType) { .t = IR_POINTER };
		}
		for (; i < arity; i++) {
			CType *arg_c_type = cdecl.type->u.function.arg_type_array[i];
			arg_ir_types[i] = c_type_to_ir_type(arg_c_type);
		}

		IrGlobal *global = NULL;
		Array(IrGlobal *) *globals = &builder->trans_unit->globals;
		for (u32 i = 0; i < globals->size; i++) {
			IrGlobal *curr_global = *ARRAY_REF(globals, IrGlobal *, i);
			if (streq(curr_global->name, cdecl.name)) {
				// @TODO: Check C type matches
				global = curr_global;
				break;
			}
		}
		
		if (global == NULL) {
			IrType return_type = struct_ret
				? (IrType) { .t = IR_VOID }
				: c_type_to_ir_type(ctype->u.function.return_type);

			global = trans_unit_add_function(builder->trans_unit, cdecl.name,
					return_type, arity, ctype->u.function.variable_arity,
					arg_ir_types);
		}

		assert(global->type.t == IR_FUNCTION);
		*result_c_type = ctype;

		return global;
	} else {
		IrGlobal *global = NULL;
		Array(IrGlobal *) *globals = &builder->trans_unit->globals;
		for (u32 i = 0; i < globals->size; i++) {
			IrGlobal *curr_global = *ARRAY_REF(globals, IrGlobal *, i);
			if (streq(curr_global->name, cdecl.name)) {
				// @TODO: Check C type matches
				global = curr_global;
				break;
			}
		}

		if (global == NULL) {
			global = trans_unit_add_var(
					builder->trans_unit,
					cdecl.name,
					c_type_to_ir_type(ctype));
		}

		*result_c_type = cdecl.type;

		return global;
	}
}

static void ir_gen_statement(IrBuilder *builder, Env *env, ASTStatement *statement);

typedef enum ExprContext
{
	LVALUE_CONTEXT,
	RVALUE_CONTEXT,
} ExprContext;
static Term ir_gen_expr(IrBuilder *builder, Env *env, ASTExpr *expr,
		ExprContext context);

static IrConst *zero_initializer(IrBuilder *builder, CType *ctype)
{
	switch (ctype->t) {
	case INTEGER_TYPE:
		return add_int_const(builder, c_type_to_ir_type(ctype), 0);
	case ARRAY_TYPE: {
		assert(!ctype->u.array.incomplete);
		// @TODO: This allocates unnecessarily by calling zero_initializer
		// recursively and then copying the result into array_elems.
		IrConst *konst = add_array_const(builder, c_type_to_ir_type(ctype));
		for (u32 i = 0; i < ctype->u.array.size; i++) {
			konst->u.array_elems[i] =
				*zero_initializer(builder, ctype->u.array.elem_type);
		}
		return konst;
	}
	case STRUCT_TYPE: {
		assert(!ctype->u.strukt.incomplete);
		// @TODO: This allocates unnecessarily by calling zero_initializer
		// recursively and then copying the result into array_elems.
		IrConst *konst = add_struct_const(builder, c_type_to_ir_type(ctype));
		for (u32 i = 0; i < ctype->u.strukt.fields.size; i++) {
			CType *field_type = ARRAY_REF(&ctype->u.strukt.fields, CDecl, i)->type;
			konst->u.struct_fields[i] = *zero_initializer(builder, field_type);
		}
		return konst;
	}
	default:
		UNIMPLEMENTED;
	}
}

static Term ir_gen_assign_op(IrBuilder *builder, Env *env, Term left,
		Term right, IrOp ir_op, Term *pre_assign_value);

void ir_gen_function(IrBuilder *builder, Env *env, IrGlobal *global,
		CType *function_type, ASTFunctionDef *function_def)
{
	IrConst *konst = add_init_to_function(builder->trans_unit, global);
	IrFunction *function = &konst->u.function;

	builder->current_function = function;
	builder->current_block = *ARRAY_REF(&function->blocks, IrBlock *, 0);

	Scope scope;
	scope.parent_scope = env->scope;
	Array(Binding) *param_bindings = &scope.bindings;
	ARRAY_INIT(param_bindings, Binding, 5);
	env->scope = &scope;

	// @TODO: We shouldn't have to re-process all of the parameter decls.
	// At the moment we have to because we throw away the name of the
	// parameter when parsing function parameter declarators, since
	// this has nothing to do with the type.
	ASTParameterDecl *param =
		params_for_function_declarator(function_def->declarator);
	for (u32 i = 0; param != NULL; i++, param = param->next) {
		if (param->t == ELLIPSIS_DECL) {
			assert(param->next == NULL);
			continue;
		}

		Binding *binding = ARRAY_APPEND(param_bindings, Binding);

		CType *decl_spec_type = decl_specifier_list_to_c_type(
				builder, env, param->decl_specifier_list);
		CDecl cdecl;
		decl_to_cdecl(builder, env, decl_spec_type,
				param->declarator, &cdecl);
		// @HACK: We have to do this because decl_to_cdecl does extra
		// stuff to adjust parameter types when it knows that the
		// declarator is for a parameter. The proper fix is just to not
		// re-process at all.
		cdecl.type = function_type->u.function.arg_type_array[i];
		cdecl_to_binding(builder, &cdecl, binding);

		Term arg = {
			.ctype = cdecl.type,
			.value = value_arg(i, global->type.u.function.arg_types[i]),
		};
		ir_gen_assign_op(builder, env, binding->term, arg, OP_INVALID, NULL);
	}

	ir_gen_statement(builder, env, function_def->body);

	if (function_type->u.function.return_type->t == VOID_TYPE)
		build_nullary_instr(builder, OP_RET_VOID, (IrType) { .t = IR_VOID });

	env->scope = env->scope->parent_scope;
	array_free(param_bindings);
}

void ir_gen_toplevel(IrBuilder *builder, ASTToplevel *toplevel)
{
	Scope global_scope;
	global_scope.parent_scope = NULL;
	Array(Binding)* global_bindings = &global_scope.bindings;
	ARRAY_INIT(global_bindings, Binding, 10);

	Env env;
	init_type_env(&env.type_env);
	env.scope = &global_scope;
	env.inline_functions = EMPTY_ARRAY;
	env.case_labels = EMPTY_ARRAY;
	env.goto_labels = EMPTY_ARRAY;
	env.goto_fixups = EMPTY_ARRAY;
	env.break_target = NULL;
	env.continue_target = NULL;

	while (toplevel != NULL) {
		IrGlobal *global = NULL;
		CType *global_type;
		ZERO_STRUCT(&global_type);

		switch (toplevel->t) {
		case FUNCTION_DEF: {
			ASTFunctionDef *func = toplevel->u.function_def;
			ASTDeclSpecifier *decl_specifier_list = func->decl_specifier_list;

			IrLinkage linkage = IR_GLOBAL_LINKAGE;
			while (decl_specifier_list->t == STORAGE_CLASS_SPECIFIER) {
				switch (decl_specifier_list->u.storage_class_specifier) {
				case STATIC_SPECIFIER:
					linkage = IR_LOCAL_LINKAGE;
					break;
				default:
					UNIMPLEMENTED;
				}

				decl_specifier_list = decl_specifier_list->next;
			}

			bool is_inline = false;
			while (decl_specifier_list != NULL
					&& decl_specifier_list->t == FUNCTION_SPECIFIER
					&& decl_specifier_list->u.function_specifier == INLINE_SPECIFIER) {
				is_inline = true;
				decl_specifier_list = decl_specifier_list->next;
			}

			ASTDeclarator *declarator = func->declarator;

			global = ir_global_for_decl(builder, &env, decl_specifier_list,
					declarator, &global_type);
			global->linkage = linkage;

			if (is_inline) {
				*ARRAY_APPEND(&env.inline_functions, InlineFunction) =
					(InlineFunction) {
						.global = global,
						.function_type = global_type,
						.function_def = {
							.decl_specifier_list = decl_specifier_list,
							.declarator = declarator,
							.old_style_param_decl_list =
								func->old_style_param_decl_list,
							.body = func->body,
						},
					};
			} else {
				ir_gen_function(builder, &env, global, global_type, func);
			}

			break;
		}
		case DECL: {
			ASTDecl *decl = toplevel->u.decl;
			ASTDeclSpecifier *decl_specifier_list = decl->decl_specifier_list;
			ASTInitDeclarator *init_declarator = decl->init_declarators;
			assert(decl_specifier_list != NULL);

			if (decl_specifier_list->t == STORAGE_CLASS_SPECIFIER
					&& decl_specifier_list->u.storage_class_specifier == EXTERN_SPECIFIER
					&& decl_specifier_list->next != NULL
					&& decl_specifier_list->next->t == FUNCTION_SPECIFIER
					&& decl_specifier_list->next->u.function_specifier == INLINE_SPECIFIER) {
				decl_specifier_list = decl_specifier_list->next->next;

				CType *decl_spec_type = decl_specifier_list_to_c_type(
						builder, &env, decl_specifier_list);
				CDecl cdecl;
				decl_to_cdecl(builder, &env, decl_spec_type,
						init_declarator->declarator, &cdecl);

				InlineFunction *matching = NULL;

				for (u32 i = 0; i < env.inline_functions.size; i++) {
					InlineFunction *inline_function =
						ARRAY_REF(&env.inline_functions, InlineFunction, i);
					if (streq(inline_function->global->name, cdecl.name)) {
						assert(c_type_eq(cdecl.type, inline_function->function_type));
						matching = inline_function;
						break;
					}
				}
				assert(matching != NULL);

				ir_gen_function(builder, &env, matching->global,
						matching->function_type, &matching->function_def);
			} else if (decl_specifier_list->t == STORAGE_CLASS_SPECIFIER
					&& decl_specifier_list->u.storage_class_specifier == TYPEDEF_SPECIFIER) {
				assert(init_declarator != NULL);
				decl_specifier_list = decl_specifier_list->next;
				CType *decl_spec_type = decl_specifier_list_to_c_type(
						builder, &env, decl_specifier_list);

				while (init_declarator != NULL) {
					assert(init_declarator->initializer == NULL);
					CDecl cdecl;
					decl_to_cdecl(builder, &env, decl_spec_type,
							init_declarator->declarator, &cdecl);

					TypeEnvEntry *new_type_alias =
						pool_alloc(&env.type_env.pool, sizeof *new_type_alias);
					*ARRAY_APPEND(&env.type_env.typedef_types, TypeEnvEntry *)
						= new_type_alias;
					new_type_alias->name = cdecl.name;
					new_type_alias->type = *cdecl.type;

					init_declarator = init_declarator->next;
				}
			} else {
				ASTDeclSpecifier *type_specs = decl_specifier_list;
				while (type_specs->t == STORAGE_CLASS_SPECIFIER) {
					type_specs = type_specs->next;
				}

				if (init_declarator == NULL) {
					decl_specifier_list_to_c_type(builder, &env, type_specs);
				} else {
					assert(init_declarator->next == NULL);
					ASTDeclarator *declarator = init_declarator->declarator;

					// @TODO: Multiple declarators in one global decl.
					global = ir_global_for_decl(builder, &env, type_specs,
							declarator, &global_type);
					bool is_extern = global_type->t == FUNCTION_TYPE;

					global->linkage = IR_GLOBAL_LINKAGE;
					while (decl_specifier_list != type_specs) {
						assert(decl_specifier_list->t == STORAGE_CLASS_SPECIFIER);
						ASTStorageClassSpecifier storage_class =
							decl_specifier_list->u.storage_class_specifier;
						if (storage_class == STATIC_SPECIFIER)
							global->linkage = IR_LOCAL_LINKAGE;
						else if (storage_class == EXTERN_SPECIFIER)
							is_extern = true;
						else
							UNIMPLEMENTED;

						decl_specifier_list = decl_specifier_list->next;
					}

					ASTInitializer *init = init_declarator->initializer;
					if (init == NULL) {
						if (!is_extern) {
							global->initializer =
								zero_initializer(builder, global_type);
						}
					} else {
						assert(!is_extern);
						assert(init->t == EXPR_INITIALIZER);
						global->initializer = eval_constant_expr(
								builder, &env.type_env, init->u.expr);
					}
				}
			}

			break;
		}
		}

		if (global != NULL) {
			Binding *binding = ARRAY_APPEND(global_bindings, Binding);
			binding->name = global->name;
			binding->constant = false;
			binding->term.ctype = global_type;
			binding->term.value = value_global(global);
		}

		toplevel = toplevel->next;
	}

	for (u32 i = 0; i < env.goto_fixups.size; i++) {
		GotoFixup *fixup = ARRAY_REF(&env.goto_fixups, GotoFixup, i);
		assert(fixup->instr->op == OP_BRANCH);
		assert(fixup->instr->u.target_block == NULL);

		for (u32 j = 0; j < env.goto_labels.size; j++) {
			GotoLabel *label = ARRAY_REF(&env.goto_labels, GotoLabel, j);
			if (streq(label->name, fixup->label_name)) {
				fixup->instr->u.target_block = label->block;
				break;
			}
		}
		assert(fixup->instr->u.target_block != NULL);
	}

	pool_free(&env.type_env.pool);
	array_free(&env.goto_labels);
	array_free(&env.goto_fixups);
	array_free(&env.type_env.struct_types);
	array_free(&env.type_env.union_types);
	array_free(&env.type_env.enum_types);
	array_free(&env.type_env.typedef_types);
	array_free(global_bindings);
}

static Term convert_type(IrBuilder *builder, Term term, CType *target_type)
{
	if (c_type_eq(term.ctype, target_type))
		return term;

	IrValue converted;
	if (term.ctype->t == INTEGER_TYPE && target_type->t == INTEGER_TYPE) {
		IrType ir_type = c_type_to_ir_type(target_type);

		if (c_type_to_ir_type(term.ctype).u.bit_width > ir_type.u.bit_width) {
			converted = build_type_instr(builder, OP_TRUNC, term.value, ir_type);
		} else if (term.ctype->u.integer.is_signed) {
			converted = build_type_instr(builder, OP_SEXT, term.value, ir_type);
		} else {
			converted = build_type_instr(builder, OP_ZEXT, term.value, ir_type);
		}
	} else if (term.ctype->t == POINTER_TYPE && target_type->t == POINTER_TYPE) {
		converted = term.value;
	} else if (term.ctype->t == ARRAY_TYPE && target_type->t == POINTER_TYPE) {
		// Array values are only ever passed around as pointers to the first
		// element anyway, so this conversion is a no-op that just changes type.
		assert(term.value.type.t == IR_POINTER);
		converted = term.value;
	} else if (target_type->t == POINTER_TYPE
			&& term.ctype->t == FUNCTION_TYPE
			&& c_type_eq(target_type->u.pointee_type, term.ctype)) {
		// Implicit conversion from function to pointer-to-function.
		converted = term.value;
	} else if (target_type->t == VOID_TYPE) {
		// Converting to void does nothing. The resulting value can't possibly
		// be used (since it has type void) so it doesn't actually matter what
		// that value is as long as the conversion doesn't cause side effects.
		converted = term.value;
	} else {
		UNIMPLEMENTED;
	}

	return (Term) {
		.ctype = target_type,
		.value = converted,
	};
}

typedef struct SubObject
{
	CType *ctype;
	ASTExpr *expr;
	u32 offset;

	enum
	{
		SUB_OBJECT_ARRAY_ELEM,
		SUB_OBJECT_STRUCT_FIELD,
	} t;
	union
	{
		u32 array_index;
		struct
		{
			CType *struct_type;
			u32 field_index;
		} struct_field;
	} u;
} SubObject;

static void add_sub_objects(IrBuilder *builder, TypeEnv *type_env,
		Array(SubObject) *sub_objects, u32 starting_offset,
		CType *aggregate_type, ASTInitializerElement *initializer_element)
{
	ASTDesignator *designator_list = initializer_element->designator_list;
	bool no_designators = designator_list == NULL;
	ASTInitializer *initializer = initializer_element->initializer;

	SubObject new_sub_object = {
		.ctype = aggregate_type,
		.expr = NULL,
		.offset = starting_offset,
	};
	while (designator_list != NULL) {
		switch (designator_list->t) {
		case INDEX_DESIGNATOR: {
			assert(new_sub_object.ctype->t == ARRAY_TYPE);
			new_sub_object.ctype = new_sub_object.ctype->u.array.elem_type;
			IrConst *index = eval_constant_expr(
					builder, type_env, designator_list->u.index_expr);
			assert(index->type.t == IR_INT);

			new_sub_object.offset += index->u.integer
				* size_of_ir_type(c_type_to_ir_type(new_sub_object.ctype));
			break;
		}
		case FIELD_DESIGNATOR: {
			assert(new_sub_object.ctype->t == STRUCT_TYPE);
			Array(CDecl) *fields = &new_sub_object.ctype->u.strukt.fields;
			CDecl *selected_field = NULL;
			u32 field_number;
			for (u32 i = 0; i < fields->size; i++) {
				CDecl *field = ARRAY_REF(fields, CDecl, i);
				if (streq(field->name, designator_list->u.field_name)) {
					selected_field = field;
					field_number = i;
					break;
				}
			}
			assert(selected_field != NULL);

			IrStructField *ir_fields =
				new_sub_object.ctype->u.strukt.ir_type->u.strukt.fields;
			new_sub_object.ctype = selected_field->type;
			new_sub_object.offset += ir_fields[field_number].offset;
			break;
		}
		}

		designator_list = designator_list->next;
	}

	if (no_designators) {
		if (sub_objects->size == 0) {
			switch (aggregate_type->t) {
			case STRUCT_TYPE: {
				Array(CDecl) *fields = &aggregate_type->u.strukt.fields;
				IrStructField *ir_fields =
					aggregate_type->u.strukt.ir_type->u.strukt.fields;
				new_sub_object.ctype = ARRAY_REF(fields, CDecl, 0)->type;
				new_sub_object.offset = ir_fields[0].offset;
				new_sub_object.t = SUB_OBJECT_STRUCT_FIELD;
				new_sub_object.u.struct_field.struct_type = aggregate_type;
				new_sub_object.u.struct_field.field_index = 0;
				break;
			}
			case ARRAY_TYPE:
				new_sub_object.ctype = aggregate_type->u.array.elem_type;
				new_sub_object.offset = 0;
				new_sub_object.t = SUB_OBJECT_ARRAY_ELEM;
				new_sub_object.u.array_index = 0;
				break;
			default:
				UNREACHABLE;
			}
		} else {
			SubObject *prev_sub_object =
				ARRAY_REF(sub_objects, SubObject, sub_objects->size - 1);
			switch (prev_sub_object->t) {
			case SUB_OBJECT_ARRAY_ELEM:
				new_sub_object.ctype = prev_sub_object->ctype;
				new_sub_object.offset = prev_sub_object->offset
					+ size_of_ir_type(c_type_to_ir_type(new_sub_object.ctype));

				new_sub_object.t = SUB_OBJECT_ARRAY_ELEM;
				new_sub_object.u.array_index =
					prev_sub_object->u.array_index + 1;
				break;
			case SUB_OBJECT_STRUCT_FIELD: {
				Array(CDecl) *fields =
					&prev_sub_object->u.struct_field.struct_type->u.strukt.fields;
				IrStructField *ir_fields =
					new_sub_object.ctype->u.strukt.ir_type->u.strukt.fields;

				u32 index = prev_sub_object->u.struct_field.field_index;
				assert(index + 1 < fields->size);
				new_sub_object.ctype = ARRAY_REF(fields, CDecl, index + 1)->type;
				new_sub_object.offset = starting_offset + ir_fields[index + 1].offset;

				new_sub_object.t = SUB_OBJECT_STRUCT_FIELD;
				new_sub_object.u.struct_field.struct_type =
					prev_sub_object->u.struct_field.struct_type;
				new_sub_object.u.struct_field.field_index = index + 1;
				break;
			}
			}
		}
	}

	switch (initializer->t) {
	case EXPR_INITIALIZER: {
		SubObject *sub_object = NULL;
		for (u32 i = 0; i < sub_objects->size; i++) {
			SubObject *existing_sub_object =
				ARRAY_REF(sub_objects, SubObject, i);
			if (existing_sub_object->offset == new_sub_object.offset) {
				sub_object = existing_sub_object;
				break;
			}
		}
		if (sub_object == NULL)
			sub_object = ARRAY_APPEND(sub_objects, SubObject);

		new_sub_object.expr = initializer->u.expr;
		*sub_object = new_sub_object;
		break;
	}
	case BRACE_INITIALIZER: {
		ASTInitializerElement *initializer_element_list =
			initializer->u.initializer_element_list;
		while (initializer_element_list != NULL) {
			add_sub_objects(builder, type_env, sub_objects, new_sub_object.offset,
					new_sub_object.ctype, initializer_element_list);
			initializer_element_list = initializer_element_list->next;
		}
		break;
	}
	}
}

static void ir_gen_initializer(IrBuilder *builder, Env *env,
		Term to_init, ASTInitializerElement *initializer_element_list)
{
	Array(SubObject) sub_objects;
	ARRAY_INIT(&sub_objects, SubObject, 5);

	// @TODO: There should be a better way to structure this such that we don't
	// need to duplicate the handling of brace initializers.
	while (initializer_element_list != NULL) {
		add_sub_objects(builder, &env->type_env, &sub_objects, 0, to_init.ctype,
				initializer_element_list);
		initializer_element_list = initializer_element_list->next;
	}

	IrValue *memset_args = pool_alloc(&builder->trans_unit->pool,
			3 * sizeof *memset_args);
	memset_args[0] = to_init.value;
	memset_args[1] = value_const(c_type_to_ir_type(&env->type_env.int_type), 0);
		// @TODO: Don't hardcode size_t!
	memset_args[2] = value_const((IrType) { .t = IR_INT, .u.bit_width = 64 },
			size_of_ir_type(c_type_to_ir_type(to_init.ctype)));

	// @TODO: Open-code this for small sizes
	build_call(builder, builtin_memset(builder),
			(IrType) { .t = IR_POINTER }, 3, memset_args);

	// @TODO: Sort initializer element list by offset because something
	// something cache something something.

	IrType int_ptr_type = (IrType) { .t = IR_INT, .u.bit_width = 64 };
	IrValue int_ptr = build_type_instr(builder, OP_CAST, to_init.value, int_ptr_type);
	for (u32 i = 0; i < sub_objects.size; i++) {
		SubObject *sub_object = ARRAY_REF(&sub_objects, SubObject, i);
		IrValue ptr = build_binary_instr(builder, OP_ADD,
				int_ptr, value_const(int_ptr_type, sub_object->offset));
		Term val = ir_gen_expr(builder, env, sub_object->expr, RVALUE_CONTEXT);
		assert(c_type_compatible(val.ctype, sub_object->ctype));
		build_store(builder, ptr, convert_type(builder, val, sub_object->ctype).value,
				c_type_to_ir_type(val.ctype));
	}

	array_free(&sub_objects);
}

static Term ir_gen_assign_op(IrBuilder *builder, Env *env, Term left,
		Term right, IrOp ir_op, Term *pre_assign_value);

static void add_decl_to_scope(IrBuilder *builder, Env *env, ASTDecl *decl)
{
	ASTInitDeclarator *init_declarator = decl->init_declarators;
	CType *decl_spec_type =
		decl_specifier_list_to_c_type(builder, env, decl->decl_specifier_list);
	
	while (init_declarator != NULL) {
		CDecl cdecl;
		decl_to_cdecl(builder, env, decl_spec_type,
				init_declarator->declarator, &cdecl);

		Binding *binding = ARRAY_APPEND(&env->scope->bindings, Binding);
		cdecl_to_binding(builder, &cdecl, binding);

		ASTInitializer *initializer = init_declarator->initializer;
		if (initializer != NULL) {
			// @TODO: This case isn't really necessary, as it should work
			// through ir_gen_initializer. However, ir_gen_initializer
			// currently unconditionally memsets to zero before assigning to
			// fields, which just feels gross to do for every local scalar
			// value. Once we've fixed this, we should remove this case.
			if (initializer->t == EXPR_INITIALIZER) {
				Term init_term = ir_gen_expr(builder, env,
						initializer->u.expr, RVALUE_CONTEXT);
				ir_gen_assign_op(builder, env,
						binding->term, init_term, OP_INVALID, NULL);
			} else {
				ir_gen_initializer(builder, env, binding->term,
						initializer->u.initializer_element_list);
			}
		}

		init_declarator = init_declarator->next;
	}
}

static void ir_gen_statement(IrBuilder *builder, Env *env, ASTStatement *statement)
{
	switch (statement->t) {
	case COMPOUND_STATEMENT: {
		Scope block_scope;
		block_scope.parent_scope = env->scope;
		ARRAY_INIT(&block_scope.bindings, Binding, 5);
		env->scope = &block_scope;

		ASTBlockItem *block_item_list = statement->u.block_item_list;
		while (block_item_list != NULL) {
			switch (block_item_list->t) {
			case BLOCK_ITEM_DECL: {
				add_decl_to_scope(builder, env, block_item_list->u.decl);
				break;
			}
			case BLOCK_ITEM_STATEMENT:
				ir_gen_statement(builder, env, block_item_list->u.statement);
				break;
			}

			block_item_list = block_item_list->next;
		}

		env->scope = env->scope->parent_scope;

		break;
	}
	case EXPR_STATEMENT: {
		ir_gen_expr(builder, env, statement->u.expr, RVALUE_CONTEXT);
		break;
	}
	case RETURN_STATEMENT: {
		if (statement->u.expr == NULL) {
			build_nullary_instr(builder, OP_RET_VOID, (IrType) { .t = IR_VOID });
		} else {
			Term term =
				ir_gen_expr(builder, env, statement->u.expr, RVALUE_CONTEXT);
			if (term.ctype->t == STRUCT_TYPE) {
				// If we return a struct, the first arg is a pointer to space
				// the caller allocated for the struct.
				Term caller_ptr = (Term) {
					.ctype = term.ctype,
					.value = value_arg(0, (IrType) { .t = IR_POINTER }),
				};
				ir_gen_assign_op(builder, env, caller_ptr, term, OP_INVALID, NULL);
				build_nullary_instr(builder, OP_RET_VOID, (IrType) { .t = IR_VOID });
			} else {
				build_unary_instr(builder, OP_RET, term.value);
			}
		}
		break;
	}
	case IF_STATEMENT: {
		ASTStatement *then_statement = statement->u.if_statement.then_statement;
		ASTStatement *else_statement = statement->u.if_statement.else_statement;

		ASTExpr *condition_expr = statement->u.if_statement.condition;
		Term condition_term =
			ir_gen_expr(builder, env, condition_expr, RVALUE_CONTEXT);
		assert(condition_term.ctype->t == INTEGER_TYPE);

		IrBlock *then_block = add_block(builder, "if.then");
		IrBlock *after_block = add_block(builder, "if.after");
		IrBlock *else_block =
			else_statement == NULL ? NULL : add_block(builder, "if.else");

		if (else_statement == NULL) {
			build_cond(builder, condition_term.value, then_block, after_block);
		} else {
			build_cond(builder, condition_term.value, then_block, else_block);
		}

		builder->current_block = then_block;
		ir_gen_statement(builder, env, then_statement);
		build_branch(builder, after_block);

		if (else_statement != NULL) {
			builder->current_block = else_block;
			ir_gen_statement(builder, env, else_statement);
			build_branch(builder, after_block);
		}

		builder->current_block = after_block;
		break;
	}
	case SWITCH_STATEMENT: {
		Array(SwitchCase) prev_case_labels = env->case_labels;
		env->case_labels = EMPTY_ARRAY;

		IrBlock *switch_entry = builder->current_block;
		IrBlock *after = add_block(builder, "switch.after");
		IrBlock *prev_break_target = env->break_target;
		env->break_target = after;

		builder->current_block = add_block(builder, "switch.body");
		ir_gen_statement(builder, env, statement->u.expr_and_statement.statement);
		build_branch(builder, after);

		builder->current_block = switch_entry;
		Term switch_value =
			ir_gen_expr(builder, env, statement->u.expr_and_statement.expr, RVALUE_CONTEXT);
		assert(switch_value.ctype->t == INTEGER_TYPE);
		for (u32 i = 0; i < env->case_labels.size; i++) {
			SwitchCase *label = ARRAY_REF(&env->case_labels, SwitchCase, i);
			if (label->is_default) {
				build_branch(builder, label->block);
			} else {
				IrBlock *next = add_block(builder, "switch.cmp");
				IrValue cmp = build_binary_instr(builder,
						OP_EQ,
						switch_value.value,
						value_const(switch_value.value.type,
							label->value->u.integer));
				build_cond(builder, cmp, label->block, next);
				builder->current_block = next;
			}
		}

		build_branch(builder, after);
		builder->current_block = after;

		env->break_target = prev_break_target;
		array_free(&env->case_labels);
		env->case_labels = prev_case_labels;
		break;
	}
	case CASE_STATEMENT: {
		// @TODO: Ensure we're inside a switch statement.
		IrBlock *case_block = add_block(builder, "switch.case");
		build_branch(builder, case_block);
		builder->current_block = case_block;

		ir_gen_statement(builder, env, statement->u.expr_and_statement.statement);

		SwitchCase *switch_case = ARRAY_APPEND(&env->case_labels, SwitchCase);
		switch_case->is_default = false;
		switch_case->value = eval_constant_expr(builder, &env->type_env,
				statement->u.expr_and_statement.expr);
		switch_case->block = case_block;
		break;
	}
	case LABELED_STATEMENT: {
		char *label_name = statement->u.labeled_statement.label_name;
		IrBlock *label_block = add_block(builder, label_name);
		build_branch(builder, label_block);
		builder->current_block = label_block;
		if (streq(label_name, "default")) {
			SwitchCase *default_case = ARRAY_APPEND(&env->case_labels, SwitchCase);
			default_case->is_default = true;
			default_case->block = label_block;
		} else {
			GotoLabel *label = ARRAY_APPEND(&env->goto_labels, GotoLabel);
			label->name = label_name;
			label->block = label_block;
		}

		ir_gen_statement(builder, env, statement->u.labeled_statement.statement);

		break;
	}
	case WHILE_STATEMENT: {
		IrBlock *pre_header = add_block(builder, "while.ph");
		IrBlock *body = add_block(builder, "while.body");
		IrBlock *after = add_block(builder, "while.after");

		ASTExpr *condition_expr = statement->u.expr_and_statement.expr;
		ASTStatement *body_statement = statement->u.expr_and_statement.statement;

		build_branch(builder, pre_header);
		builder->current_block = pre_header;
		Term condition_term =
			ir_gen_expr(builder, env, condition_expr, RVALUE_CONTEXT);

		assert(condition_term.ctype->t == INTEGER_TYPE);
		build_cond(builder, condition_term.value, body, after);

		IrBlock *prev_break_target = env->break_target;
		IrBlock *prev_continue_target = env->continue_target;
		env->break_target = after;
		env->continue_target = pre_header;
		builder->current_block = body;

		ir_gen_statement(builder, env, body_statement);

		build_branch(builder, pre_header);
		env->break_target = prev_break_target;
		env->continue_target = prev_continue_target;

		builder->current_block = after;

		break;
	}
	case DO_WHILE_STATEMENT: {
		IrBlock *pre_header = add_block(builder, "do_while.ph");
		IrBlock *body = add_block(builder, "do_while.body");
		IrBlock *after = add_block(builder, "do_while.after");

		ASTExpr *condition_expr = statement->u.expr_and_statement.expr;
		ASTStatement *body_statement = statement->u.expr_and_statement.statement;

		build_branch(builder, body);
		builder->current_block = pre_header;
		Term condition_term =
			ir_gen_expr(builder, env, condition_expr, RVALUE_CONTEXT);

		assert(condition_term.ctype->t == INTEGER_TYPE);
		build_cond(builder, condition_term.value, body, after);

		IrBlock *prev_break_target = env->break_target;
		IrBlock *prev_continue_target = env->continue_target;
		env->break_target = after;
		env->continue_target = pre_header;
		builder->current_block = body;

		ir_gen_statement(builder, env, body_statement);

		build_branch(builder, pre_header);
		env->break_target = prev_break_target;
		env->continue_target = prev_continue_target;

		builder->current_block = after;

		break;
	}
	case FOR_STATEMENT: {
		IrBlock *pre_header = add_block(builder, "for.ph");
		IrBlock *body = add_block(builder, "for.body");
		IrBlock *update = add_block(builder, "for.update");
		IrBlock *after = add_block(builder, "for.after");

		Scope init_scope;
		Scope *prev_scope = env->scope;

		ASTForStatement *f = &statement->u.for_statement;
		if (f->init_type == FOR_INIT_DECL) {
			init_scope.parent_scope = env->scope;
			ARRAY_INIT(&init_scope.bindings, Binding, 1);
			env->scope = &init_scope;
			add_decl_to_scope(builder, env, f->init.decl);
			env->scope = env->scope->parent_scope;

			env->scope = &init_scope;
		} else {
			assert(f->init_type == FOR_INIT_EXPR);
			if (f->init.expr != NULL)
				ir_gen_expr(builder, env, f->init.expr, RVALUE_CONTEXT);
		}

		build_branch(builder, pre_header);
		builder->current_block = pre_header;
		Term condition_term =
			ir_gen_expr(builder, env, f->condition, RVALUE_CONTEXT);

		assert(condition_term.ctype->t == INTEGER_TYPE);
		build_cond(builder, condition_term.value, body, after);

		builder->current_block = body;
		IrBlock *prev_break_target = env->break_target;
		IrBlock *prev_continue_target = env->continue_target;
		env->break_target = after;
		env->continue_target = update;

		ir_gen_statement(builder, env, f->body);
		build_branch(builder, update);
		builder->current_block = update;

		env->break_target = prev_break_target;
		env->continue_target = prev_continue_target;

		if (f->update_expr != NULL)
			ir_gen_expr(builder, env, f->update_expr, RVALUE_CONTEXT);

		build_branch(builder, pre_header);

		env->scope = prev_scope;
		builder->current_block = after;

		break;
	}
	case GOTO_STATEMENT: {
		IrInstr *branch_instr = build_branch(builder, NULL);
		GotoFixup *fixup = ARRAY_APPEND(&env->goto_fixups, GotoFixup);
		fixup->label_name = statement->u.goto_label;
		fixup->instr = branch_instr;

		builder->current_block = add_block(builder, "goto.after");
		break;
	}
	case BREAK_STATEMENT:
		assert(env->break_target != NULL);
		build_branch(builder, env->break_target);
		break;
	case CONTINUE_STATEMENT:
		assert(env->continue_target != NULL);
		build_branch(builder, env->continue_target);
		break;
	case EMPTY_STATEMENT:
		break;
	}
}

static Term ir_gen_struct_field(IrBuilder *builder, Term struct_term,
		char *field_name, ExprContext context)
{
		assert(struct_term.value.type.t == IR_POINTER);

		CType *ctype = struct_term.ctype;
		if (struct_term.ctype->t == POINTER_TYPE) {
			ctype = ctype->u.pointee_type;
		}

		assert(ctype->t == STRUCT_TYPE);
		Array(CDecl) *fields = &ctype->u.strukt.fields;
		CDecl *selected_field = NULL;
		u32 field_number;
		for (u32 i = 0; i < fields->size; i++) {
			CDecl *field = ARRAY_REF(fields, CDecl, i);
			if (streq(field->name, field_name)) {
				selected_field = field;
				field_number = i;
				break;
			}
		}
		assert(selected_field != NULL);

		IrValue value = build_field(builder, struct_term.value,
				*ctype->u.strukt.ir_type, field_number);
		IrType *struct_ir_type = ctype->u.strukt.ir_type;
		assert(struct_ir_type->t == IR_STRUCT);
		IrType field_type = struct_ir_type->u.strukt.fields[field_number].type;

		if (context == RVALUE_CONTEXT
				&& selected_field->type->t != STRUCT_TYPE
				&& selected_field->type->t != ARRAY_TYPE) {
			value = build_load(builder, value, field_type);
		}

		return (Term) { .ctype = selected_field->type, .value = value };
}

static Term ir_gen_add(IrBuilder *builder, Env *env, Term left, Term right)
{
	left.ctype = decay_to_pointer(&env->type_env, left.ctype);
	right.ctype = decay_to_pointer(&env->type_env, right.ctype);

	bool left_is_pointer = left.ctype->t == POINTER_TYPE;
	bool right_is_pointer = right.ctype->t == POINTER_TYPE;

	if (left.ctype->t == INTEGER_TYPE && right.ctype->t == INTEGER_TYPE) {
		IrValue value = build_binary_instr(builder, OP_ADD, left.value, right.value);

		// @TODO: Determine type correctly
		return (Term) {
			.ctype = left.ctype,
			.value = value,
		};
	} else if (left_is_pointer ^ right_is_pointer) {
		Term pointer = left_is_pointer ? left : right;
		Term other = left_is_pointer ? right : left;
		assert(other.ctype->t == INTEGER_TYPE);

		CType *result_type = pointer.ctype;
		CType *pointee_type = result_type->u.pointee_type;

		// @TODO: Determine type correctly
		// @TODO: Don't hardcode in the size of a pointer!
		IrType pointer_int_type = (IrType) { .t = IR_INT, .u.bit_width = 64 };

		IrValue zext =
			build_type_instr(builder, OP_ZEXT, other.value, pointer_int_type);
		IrValue ptr_to_int =
			build_type_instr(builder, OP_CAST, pointer.value, pointer_int_type);
		IrValue addend = build_binary_instr(
				builder,
				OP_MUL,
				zext,
				value_const(pointer_int_type,
					size_of_ir_type(c_type_to_ir_type(pointee_type))));

		IrValue sum = build_binary_instr(builder, OP_ADD, ptr_to_int, addend);
		IrValue int_to_ptr = build_type_instr(builder, OP_CAST, sum,
				c_type_to_ir_type(result_type));

		return (Term) {
			.ctype = result_type,
			.value = int_to_ptr,
		};
	} else {
		UNIMPLEMENTED;
	}
}

static Term ir_gen_sub(IrBuilder *builder, Env *env, Term left, Term right)
{
	left.ctype = decay_to_pointer(&env->type_env, left.ctype);
	right.ctype = decay_to_pointer(&env->type_env, right.ctype);

	bool left_is_pointer = left.ctype->t == POINTER_TYPE;
	bool right_is_pointer = right.ctype->t == POINTER_TYPE;

	if (left.ctype->t == INTEGER_TYPE && right.ctype->t == INTEGER_TYPE) {
		IrValue value = build_binary_instr(builder, OP_SUB, left.value, right.value);

		// @TODO: Determine type correctly
		return (Term) {
			.ctype = left.ctype,
			.value = value,
		};
	} else if (left_is_pointer && right_is_pointer) {
		CType *pointee_type = left.ctype->u.pointee_type;

		// @TODO: Determine type correctly
		// @TODO: Don't hardcode in the size of a pointer!
		IrType pointer_int_type = { .t = IR_INT, .u.bit_width = 64 };

		// @TODO: This should be ptrdiff_t
		CType *result_c_type = &env->type_env.int_type;

		IrValue left_int =
			build_type_instr(builder, OP_CAST, left.value, pointer_int_type);
		IrValue right_int =
			build_type_instr(builder, OP_CAST, right.value, pointer_int_type);
		IrValue diff = build_binary_instr(builder, OP_SUB, left_int, right_int);
		IrValue cast = build_type_instr(
				builder, OP_CAST, diff, c_type_to_ir_type(result_c_type));
		IrValue scaled = build_binary_instr(
				builder,
				OP_DIV,
				cast,
				value_const(cast.type,
					size_of_ir_type(c_type_to_ir_type(pointee_type))));

		return (Term) {
			.ctype = result_c_type,
			.value = scaled,
		};
	} else if (left_is_pointer && (right.ctype->t == INTEGER_TYPE)) {
		// @TODO: This block is almost identical to the corresponding block in
		// ir_gen_add, except for OP_SUB instead of OP_ADD. Factor out?
		assert(right.ctype->t == INTEGER_TYPE);

		CType *result_type = left.ctype;
		CType *pointee_type = result_type->u.pointee_type;

		// @TODO: Determine type correctly
		// @TODO: Don't hardcode in the size of a pointer!
		IrType pointer_int_type = (IrType) { .t = IR_INT, .u.bit_width = 64 };

		IrValue zext =
			build_type_instr(builder, OP_ZEXT, right.value, pointer_int_type);
		IrValue ptr_to_int =
			build_type_instr(builder, OP_CAST, left.value, pointer_int_type);
		IrValue subtrahend = build_binary_instr(
				builder,
				OP_MUL,
				zext,
				value_const(pointer_int_type,
					size_of_ir_type(c_type_to_ir_type(pointee_type))));

		IrValue sum = build_binary_instr(builder, OP_SUB, ptr_to_int, subtrahend);
		IrValue int_to_ptr = build_type_instr(builder, OP_CAST, sum,
				c_type_to_ir_type(result_type));

		return (Term) {
			.ctype = result_type,
			.value = int_to_ptr,
		};
	} else {
		UNIMPLEMENTED;
	}
}

static Term ir_gen_binary_operator(IrBuilder *builder, Env *env, Term left,
		Term right, IrOp ir_op)
{
	if (ir_op == OP_ADD)
		return ir_gen_add(builder, env, left, right);
	if (ir_op == OP_SUB)
		return ir_gen_sub(builder, env, left, right);

	left.ctype = decay_to_pointer(&env->type_env, left.ctype);
	right.ctype = decay_to_pointer(&env->type_env, right.ctype);

	// @TODO: Determine type correctly.
	CType *result_type = &env->type_env.int_type;

	if (!(ir_op == OP_EQ
				&& left.ctype->t == POINTER_TYPE
				&& right.ctype->t == POINTER_TYPE)) {
		// @TODO: Proper arithmetic conversions
		assert(left.ctype->t == INTEGER_TYPE && right.ctype->t == INTEGER_TYPE);
		if (rank(left.ctype) != rank(right.ctype)) {
			Term *to_convert = rank(left.ctype) < rank(right.ctype) ? &left : &right;
			CType *conversion_type =
				rank(left.ctype) < rank(right.ctype) ? right.ctype : left.ctype;
			Term converted = convert_type(builder, *to_convert, conversion_type);

			to_convert->value = converted.value;
			to_convert->ctype = conversion_type;
		}
	}

	IrValue value = build_binary_instr(builder, ir_op, left.value, right.value);

	return (Term) { .ctype = result_type, .value = value };
}

static Term ir_gen_binary_expr(IrBuilder *builder, Env *env, ASTExpr *expr,
		IrOp ir_op)
{
	return ir_gen_binary_operator(
			builder,
			env,
			ir_gen_expr(builder, env, expr->u.binary_op.arg1, RVALUE_CONTEXT),
			ir_gen_expr(builder, env, expr->u.binary_op.arg2, RVALUE_CONTEXT),
			ir_op);
}

static Term ir_gen_assign_op(IrBuilder *builder, Env *env, Term left,
		Term right, IrOp ir_op, Term *pre_assign_value)
{
	Term result = right;

	if (left.ctype->t == STRUCT_TYPE || left.ctype->t == ARRAY_TYPE) {
		assert(c_type_eq(left.ctype, right.ctype));

		IrValue *memcpy_args = pool_alloc(&builder->trans_unit->pool,
				3 * sizeof *memcpy_args);
		memcpy_args[0] = left.value,
		memcpy_args[1] = right.value,
		// @TODO: Don't hardcode the size of a pointer!
		memcpy_args[2] = value_const((IrType) { .t = IR_INT, .u.bit_width = 64 },
				size_of_ir_type(*left.ctype->u.strukt.ir_type)),

		// @TODO: Open-code this for small sizes.
		build_call(builder, builtin_memcpy(builder),
				(IrType) { .t = IR_POINTER }, 3, memcpy_args);
	} else {
		if (ir_op != OP_INVALID) {
			Term load = (Term) {
				.ctype = left.ctype,
				.value = build_load(builder, left.value, c_type_to_ir_type(left.ctype)),
			};
			result = ir_gen_binary_operator(builder, env, load, right, ir_op);

			if (pre_assign_value != NULL)
				*pre_assign_value = load;

		}

		result = convert_type(builder, result, left.ctype);
		build_store(builder, left.value, result.value,
				c_type_to_ir_type(left.ctype));
	}

	return result;
}

static Term ir_gen_assign_expr(IrBuilder *builder, Env *env, ASTExpr *expr,
		IrOp ir_op)
{
	Term left = ir_gen_expr(builder, env, expr->u.binary_op.arg1, LVALUE_CONTEXT);
	Term right = ir_gen_expr(builder, env, expr->u.binary_op.arg2, RVALUE_CONTEXT);
	return ir_gen_assign_op(builder, env, left, right, ir_op, NULL);
}

static Term ir_gen_deref(IrBuilder *builder, TypeEnv *type_env,
		Term pointer, ExprContext context)
{
	CType *pointer_type = decay_to_pointer(type_env, pointer.ctype);
	assert(pointer_type->t == POINTER_TYPE);
	CType *pointee_type = pointer_type->u.pointee_type;

	IrValue value;
	if (context == LVALUE_CONTEXT) {
		value = pointer.value;
	} else {
		assert(context == RVALUE_CONTEXT);

		value = build_load(
				builder,
				pointer.value,
				c_type_to_ir_type(pointee_type));
	}

	return (Term) { .ctype = pointee_type, .value = value };
}

static Term ir_gen_expr(IrBuilder *builder, Env *env, ASTExpr *expr,
		ExprContext context)
{
	IGNORE(builder);

	if (context == LVALUE_CONTEXT) {
		ASTExprType t = expr->t;
		assert(t == IDENTIFIER_EXPR
				|| t == STRUCT_DOT_FIELD_EXPR
				|| t == STRUCT_ARROW_FIELD_EXPR
				|| t == INDEX_EXPR
				|| t == DEREF_EXPR);
	}

	switch (expr->t) {
	case IDENTIFIER_EXPR: {
		Binding *binding = binding_for_name(env->scope, expr->u.identifier);
		assert(binding != NULL);
		IrValue value;
		assert(binding->term.value.type.t == IR_POINTER || binding->constant);

		// Functions, arrays, and structs implicitly have their address taken.
		if (context == LVALUE_CONTEXT
				|| binding->term.ctype->t == FUNCTION_TYPE
				|| binding->term.ctype->t == ARRAY_TYPE
				|| binding->term.ctype->t == STRUCT_TYPE) {
			assert(!binding->constant);
			value = binding->term.value;
		} else {
			assert(context == RVALUE_CONTEXT);
			if (binding->constant) {
				value = binding->term.value;
			} else {
				value = build_load(
						builder,
						binding->term.value,
						c_type_to_ir_type(binding->term.ctype));
			}
		}

		return (Term) { .ctype = binding->term.ctype, .value = value };
	}
	case STRUCT_ARROW_FIELD_EXPR: {
		ASTExpr *struct_expr = expr->u.struct_field.struct_expr;
		Term struct_term = ir_gen_expr(builder, env, struct_expr, RVALUE_CONTEXT);
		assert(struct_term.ctype->t == POINTER_TYPE);
		assert(struct_term.ctype->u.pointee_type->t == STRUCT_TYPE);

		return ir_gen_struct_field(builder, struct_term,
				expr->u.struct_field.field_name, context);
	}
	case STRUCT_DOT_FIELD_EXPR: {
		ASTExpr *struct_expr = expr->u.struct_field.struct_expr;
		Term struct_term = ir_gen_expr(builder, env, struct_expr, RVALUE_CONTEXT);
		assert(struct_term.ctype->t == STRUCT_TYPE);

		return ir_gen_struct_field(builder, struct_term,
				expr->u.struct_field.field_name, context);
	}
	case ADDRESS_OF_EXPR: {
		ASTExpr *inner_expr = expr->u.unary_arg;
		Term ptr = ir_gen_expr(builder, env, inner_expr, LVALUE_CONTEXT);
		ptr.ctype = pointer_type(&env->type_env, ptr.ctype);

		return ptr;
	}
	case DEREF_EXPR: {
		ASTExpr *inner_expr = expr->u.unary_arg;
		Term pointer = ir_gen_expr(builder, env, inner_expr, RVALUE_CONTEXT);
		return ir_gen_deref(builder, &env->type_env, pointer, context);
	}
	case INDEX_EXPR: {
		Term pointer = ir_gen_add(
				builder,
				env,
				ir_gen_expr(builder, env, expr->u.binary_op.arg1, RVALUE_CONTEXT),
				ir_gen_expr(builder, env, expr->u.binary_op.arg2, RVALUE_CONTEXT));
		assert(pointer.ctype->t == POINTER_TYPE);
		return ir_gen_deref(builder, &env->type_env, pointer, context);
	}
	case INT_LITERAL_EXPR: {
		// @TODO: Determine types of constants correctly.
		CType *result_type = &env->type_env.int_type;

		IrValue value = value_const(
				(IrType) { .t = IR_INT, .u.bit_width = 32 },
				expr->u.int_literal);

		return (Term) { .ctype = result_type, .value = value };
	}
	// @TODO: Deduplicate identical string literals
	case STRING_LITERAL_EXPR: {
		char fmt[] = "__string_literal_%x";

		// - 2 adjusts down for the "%x" which isn't present in the output
		// sizeof(u32) * 2 is the max length of globals.size in hex
		// + 1 for the null terminator
		u32 name_max_length = sizeof fmt - 2 + sizeof(u32) * 2 + 1;
		char *name = pool_alloc(&builder->trans_unit->pool, name_max_length);
		snprintf(name, name_max_length, fmt, builder->trans_unit->globals.size);

		char *string = expr->u.string_literal;
		u32 length = strlen(string) + 1;
		CType *result_type =
			array_type(builder, &env->type_env, &env->type_env.char_type);
		set_array_type_length(result_type, length);
		IrType ir_type = c_type_to_ir_type(result_type);
		IrGlobal *global = trans_unit_add_var(builder->trans_unit, name, ir_type);
		global->linkage = IR_LOCAL_LINKAGE;

		IrConst *konst = add_array_const(builder, ir_type);
		IrType ir_char_type = c_type_to_ir_type(&env->type_env.char_type);
		for (u32 i = 0; i < length; i++) {
			konst->u.array_elems[i] = (IrConst) {
				.type = ir_char_type,
				.u.integer = string[i],
			};
		}

		konst->type = ir_type;

		global->initializer = konst;

		return (Term) { .ctype = result_type, .value = value_global(global) };
	}
	case ADD_EXPR: return ir_gen_binary_expr(builder, env, expr, OP_ADD);
	case MINUS_EXPR: return ir_gen_binary_expr(builder, env, expr, OP_SUB);
	case BIT_XOR_EXPR: return ir_gen_binary_expr(builder, env, expr, OP_BIT_XOR);
	case BIT_AND_EXPR: return ir_gen_binary_expr(builder, env, expr, OP_BIT_AND);
	case BIT_OR_EXPR: return ir_gen_binary_expr(builder, env, expr, OP_BIT_OR);
	case BIT_NOT_EXPR: {
		// @TODO: Determine type correctly.
		CType *result_type = &env->type_env.int_type;
		Term term = ir_gen_expr(builder, env, expr->u.unary_arg, RVALUE_CONTEXT);

		return (Term) {
			.value = build_unary_instr(builder, OP_BIT_NOT, term.value),
			.ctype = result_type,
		};
	}
	case LOGICAL_NOT_EXPR: {
		CType *result_type = &env->type_env.int_type;
		Term term = ir_gen_expr(builder, env, expr->u.unary_arg, RVALUE_CONTEXT);

		return (Term) {
			.value = build_unary_instr(builder, OP_LOG_NOT, term.value),
			.ctype = result_type,
		};
	}
	case LEFT_SHIFT_EXPR: return ir_gen_binary_expr(builder, env, expr, OP_SHL);
	case MULTIPLY_EXPR: return ir_gen_binary_expr(builder, env, expr, OP_MUL);
	case DIVIDE_EXPR: return ir_gen_binary_expr(builder, env, expr, OP_DIV);
	case EQUAL_EXPR: return ir_gen_binary_expr(builder, env, expr, OP_EQ);
	case NOT_EQUAL_EXPR: return ir_gen_binary_expr(builder, env, expr, OP_NEQ);
	case GREATER_THAN_EXPR: return ir_gen_binary_expr(builder, env, expr, OP_GT);
	case GREATER_THAN_OR_EQUAL_EXPR: return ir_gen_binary_expr(builder, env, expr, OP_GTE);
	case LESS_THAN_EXPR: return ir_gen_binary_expr(builder, env, expr, OP_LT);
	case LESS_THAN_OR_EQUAL_EXPR: return ir_gen_binary_expr(builder, env, expr, OP_LTE);

	case ASSIGN_EXPR: return ir_gen_assign_expr(builder, env, expr, OP_INVALID);
	case ADD_ASSIGN_EXPR: return ir_gen_assign_expr(builder, env, expr, OP_ADD);
	case MINUS_ASSIGN_EXPR: return ir_gen_assign_expr(builder, env, expr, OP_SUB);
	case PRE_INCREMENT_EXPR: case POST_INCREMENT_EXPR: {
		Term ptr = ir_gen_expr(builder, env, expr->u.unary_arg, LVALUE_CONTEXT);
		// @TODO: Correct type
		CType *one_type = &env->type_env.int_type;
		Term one = (Term) {
			.value = value_const(c_type_to_ir_type(one_type), 1),
			.ctype = one_type,
		};
		Term pre_assign_value;
		Term incremented =
			ir_gen_assign_op(builder, env, ptr, one, OP_ADD, &pre_assign_value);

		if (expr->t == PRE_INCREMENT_EXPR) {
			return incremented;
		} else {
			return pre_assign_value;
		}
	}
	case BIT_XOR_ASSIGN_EXPR: return ir_gen_assign_expr(builder, env, expr, OP_BIT_XOR);
	case BIT_AND_ASSIGN_EXPR: return ir_gen_assign_expr(builder, env, expr, OP_BIT_AND);
	case BIT_OR_ASSIGN_EXPR: return ir_gen_assign_expr(builder, env, expr, OP_BIT_OR);
	case MULTIPLY_ASSIGN_EXPR: return ir_gen_assign_expr(builder, env, expr, OP_MUL);
	case DIVIDE_ASSIGN_EXPR: return ir_gen_assign_expr(builder, env, expr, OP_DIV);
	case FUNCTION_CALL_EXPR: {
		ASTExpr *callee_expr = expr->u.function_call.callee;

		u32 call_arity = 0;
		ASTArgument *arg = expr->u.function_call.arg_list;
		while (arg != NULL) {
			call_arity++;
			arg = arg->next;
		}

		if (callee_expr->t == IDENTIFIER_EXPR) {
			char *name = callee_expr->u.identifier;
			if (streq(name, "__builtin_va_start")) {
				assert(call_arity == 1);

				Term va_list_ptr = ir_gen_expr(
						builder, env, expr->u.function_call.arg_list->expr,
						RVALUE_CONTEXT);
				assert(va_list_ptr.ctype->t == POINTER_TYPE);

				return (Term) {
					.ctype = &env->type_env.void_type,
					.value = build_builtin_va_start(builder, va_list_ptr.value),
				};
			} else if (streq(name, "__builtin_va_arg")) {
				assert(call_arity == 2);

				Term va_list_ptr = ir_gen_expr(
						builder, env, expr->u.function_call.arg_list->expr,
						RVALUE_CONTEXT);
				Term object_size = ir_gen_expr(
						builder, env, expr->u.function_call.arg_list->next->expr,
						RVALUE_CONTEXT);

				assert(va_list_ptr.ctype->t == POINTER_TYPE);

				assert(object_size.ctype->t == INTEGER_TYPE);
				assert(object_size.value.t == VALUE_CONST);

				return (Term) {
					.ctype = pointer_type(&env->type_env, &env->type_env.void_type),
					.value = build_builtin_va_arg(builder, va_list_ptr.value,
						value_const(object_size.value.type,
							object_size.value.u.constant)),
				};
			}
		}

		Term callee = ir_gen_expr(builder, env, callee_expr, RVALUE_CONTEXT);
		// @TODO: We should never have objects of bare function type in the
		// first place - when ir_gen'ing an identifier expr referring to a
		// global function it should have type "pointer to F", where F is the
		// type of the function in question.
		if (callee.ctype->t != FUNCTION_TYPE) {
			assert(callee.ctype->t == POINTER_TYPE);
			CType *pointee_type = callee.ctype->u.pointee_type;
			assert(pointee_type->t == FUNCTION_TYPE);
			callee.ctype = pointee_type;
		}

		u32 callee_arity = callee.ctype->u.function.arity;

		// Struct returns are handled in the frontend, by adding a pointer
		// parameter at the start, and allocating a local in the caller.
		bool struct_ret = callee.ctype->u.function.return_type->t == STRUCT_TYPE;
		if (struct_ret)
			call_arity++;

		CType *return_type = callee.ctype->u.function.return_type;
		IrValue *arg_array = pool_alloc(&builder->trans_unit->pool,
				call_arity * sizeof(*arg_array));

		u32 i = 0;
		IrValue local_for_ret_value;
		if (struct_ret) {
			local_for_ret_value =
				build_local(builder, c_type_to_ir_type(return_type));
			arg_array[i++] = local_for_ret_value;
		}

		arg = expr->u.function_call.arg_list;
		for (; arg != NULL; i++, arg = arg->next) {
			Term arg_term = ir_gen_expr(builder, env, arg->expr, RVALUE_CONTEXT);

			if (i < callee_arity) {
				CType *arg_type = callee.ctype->u.function.arg_type_array[i];
				assert(c_type_compatible(arg_term.ctype, arg_type));
				arg_term = convert_type(builder, arg_term, arg_type);
			}

			arg_array[i] = arg_term.value;
		}

		IrType return_ir_type = struct_ret
			? (IrType) { .t = IR_VOID }
			: c_type_to_ir_type(return_type);

		IrValue value = build_call(builder, callee.value, return_ir_type,
				call_arity, arg_array);
		if (struct_ret)
			value = local_for_ret_value;

		return (Term) { .ctype = return_type, .value = value, };
	}
	case COMMA_EXPR:
		ir_gen_expr(builder, env, expr->u.binary_op.arg1, RVALUE_CONTEXT);
		return ir_gen_expr(builder, env, expr->u.binary_op.arg2, RVALUE_CONTEXT);
	case SIZEOF_TYPE_EXPR: {
		ASTDeclSpecifier *decl_specifier_list = expr->u.type->decl_specifier_list;
		ASTDeclarator *declarator = expr->u.type->declarator;

		CDecl cdecl;
		CType *decl_spec_type = decl_specifier_list_to_c_type(
				builder, env, decl_specifier_list);
		decl_to_cdecl(builder, env, decl_spec_type, declarator, &cdecl);

		// @TODO: This should be a size_t
		CType *result_type = &env->type_env.int_type;

		IrValue value = value_const(c_type_to_ir_type(result_type),
				size_of_ir_type(c_type_to_ir_type(cdecl.type)));

		return (Term) { .ctype = result_type, .value = value };
	}
	case LOGICAL_OR_EXPR: case LOGICAL_AND_EXPR: {
		bool is_or = expr->t == LOGICAL_OR_EXPR;

		IrBlock *initial_block = builder->current_block;
		IrBlock *rhs_block = add_block(builder, is_or ? "or.rhs" : "and.rhs");
		IrBlock *after_block = add_block(builder, is_or ? "or.after" : "and.after");

		ASTExpr *lhs_expr = expr->u.binary_op.arg1;
		ASTExpr *rhs_expr = expr->u.binary_op.arg2;

		Term lhs = ir_gen_expr(builder, env, lhs_expr, RVALUE_CONTEXT);
		assert(lhs.ctype->t == INTEGER_TYPE);
		if (is_or) {
			build_cond(builder, lhs.value, after_block, rhs_block);
		} else {
			build_cond(builder, lhs.value, rhs_block, after_block);
		}

		builder->current_block = rhs_block;
		Term rhs = ir_gen_expr(builder, env, rhs_expr, RVALUE_CONTEXT);
		assert(rhs.ctype->t == INTEGER_TYPE);
		IrValue rhs_as_bool = build_binary_instr(builder, OP_NEQ, rhs.value,
				value_const(c_type_to_ir_type(&env->type_env.int_type), 0));
		build_branch(builder, after_block);

		builder->current_block = after_block;
		IrValue phi =
			build_phi(builder, c_type_to_ir_type(&env->type_env.int_type), 2);
		phi_set_param(phi, 0, initial_block,
				value_const(c_type_to_ir_type(&env->type_env.int_type),
					is_or ? 1 : 0));
		phi_set_param(phi, 1, rhs_block, rhs_as_bool);

		return (Term) { .ctype = &env->type_env.int_type, .value = phi };
	}
	case CONDITIONAL_EXPR: {
		IrBlock *then_block = add_block(builder, "ternary.then");
		IrBlock *else_block = add_block(builder, "ternary.else");
		IrBlock *after_block = add_block(builder, "ternary.after");

		ASTExpr *condition_expr = expr->u.ternary_op.arg1;
		Term condition_term =
			ir_gen_expr(builder, env, condition_expr, RVALUE_CONTEXT);
		assert(condition_term.ctype->t == INTEGER_TYPE);
		build_cond(builder, condition_term.value, then_block, else_block);

		ASTExpr *then_expr = expr->u.ternary_op.arg2;
		builder->current_block = then_block;
		Term then_term = ir_gen_expr(builder, env, then_expr, RVALUE_CONTEXT);
		build_branch(builder, after_block);

		ASTExpr *else_expr = expr->u.ternary_op.arg3;
		builder->current_block = else_block;
		Term else_term = ir_gen_expr(builder, env, else_expr, RVALUE_CONTEXT);
		build_branch(builder, after_block);

		assert(c_type_eq(then_term.ctype, else_term.ctype));

		builder->current_block = after_block;
		IrValue phi = build_phi(builder, c_type_to_ir_type(then_term.ctype), 2);
		phi_set_param(phi, 0, then_block, then_term.value);
		phi_set_param(phi, 1, else_block, else_term.value);
		return (Term) { .ctype = then_term.ctype, .value = phi };
	}
	case COMPOUND_EXPR: {
		CType *type =
			type_name_to_c_type(builder, env, expr->u.compound.type_name);
		IrValue local = build_local(builder, c_type_to_ir_type(type));
		Term compound_value = { .value = local, .ctype = type };

		ir_gen_initializer(builder, env, compound_value,
				expr->u.compound.initializer_element_list);

		return compound_value;
	}
	case SIZEOF_EXPR_EXPR: {
		ASTExpr *sizeof_expr = expr->u.unary_arg;

		u64 size;

		// @TODO: Do this more generally. We probably want a third ExprContext,
		// SIZEOF_CONTEXT. This would behave like RVALUE_CONTEXT, but not
		// actually generate any IR, just determine types. Either that or
		// switch to a scratch IrFunction and delete it afterwards but that
		// seems messy...
		if (sizeof_expr->t == IDENTIFIER_EXPR) {
			Term term = ir_gen_expr(builder, env, sizeof_expr, LVALUE_CONTEXT);
			size = size_of_ir_type(c_type_to_ir_type(term.ctype));
		} else if (sizeof_expr->t == DEREF_EXPR) {
			ASTExpr *inner_expr = sizeof_expr->u.unary_arg;
			if (inner_expr->t != IDENTIFIER_EXPR)
				UNIMPLEMENTED;
			Term term = ir_gen_expr(builder, env, inner_expr, LVALUE_CONTEXT);
			assert(term.ctype->t == POINTER_TYPE);
			size = size_of_ir_type(c_type_to_ir_type(term.ctype->u.pointee_type));
		} else {
			UNIMPLEMENTED;
		}

		// @TODO: This should be a size_t
		CType *result_type = &env->type_env.int_type;
		IrValue value = value_const(c_type_to_ir_type(result_type), size);
		return (Term) { .ctype = result_type, .value = value };
	}
	case CAST_EXPR: {
		CType *cast_type =
			type_name_to_c_type(builder, env, expr->u.cast.cast_type);
		Term castee =
			ir_gen_expr(builder, env, expr->u.cast.arg, RVALUE_CONTEXT);
		return convert_type(builder, castee, cast_type);
	}
	default:
		printf("%d\n", expr->t);
		UNIMPLEMENTED;
	}
}
