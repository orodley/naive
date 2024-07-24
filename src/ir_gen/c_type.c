#include "ir_gen/c_type.h"

#include <stdarg.h>
#include <stdbool.h>

#include "parse.h"
#include "tokenise.h"

extern inline u32 c_type_size(CType *type);

bool c_type_eq(CType *a, CType *b)
{
  if (a->t != b->t) return false;

  switch (a->t) {
  case VOID_TYPE: return true;
  case INTEGER_TYPE:
    return a->u.integer.type == b->u.integer.type
           && a->u.integer.is_signed == b->u.integer.is_signed;
  case FLOAT_TYPE: return a->u.floatt == b->u.floatt;
  case FUNCTION_TYPE:
    if (a->u.function.arity != b->u.function.arity) return false;
    if (!c_type_eq(a->u.function.return_type, b->u.function.return_type))
      return false;
    for (u32 i = 0; i < a->u.function.arity; i++) {
      if (!c_type_eq(
              a->u.function.arg_type_array[i], b->u.function.arg_type_array[i]))
        return false;
    }

    return true;
  case STRUCT_TYPE:
    if (a->u.strukt.incomplete != b->u.strukt.incomplete) return false;
    if (a->u.strukt.incomplete) return a == b;
    return a->u.strukt.ir_type == b->u.strukt.ir_type;
  case POINTER_TYPE:
    assert(a != a->u.pointee_type);
    assert(b != b->u.pointee_type);
    return c_type_eq(a->u.pointee_type, b->u.pointee_type);
  case ARRAY_TYPE:
    return ((a->u.array.incomplete && b->u.array.incomplete)
            || (a->u.array.size == b->u.array.size))
           && c_type_eq(a->u.array.elem_type, b->u.array.elem_type);
  }

  UNREACHABLE;
}

#if 0
bool c_type_compatible(CType *a, CType *b)
{
  if (c_type_eq(a, b)) return true;

  if (a->t == POINTER_TYPE && b->t == POINTER_TYPE
      && (a->u.pointee_type->t == VOID_TYPE
          || b->u.pointee_type->t == VOID_TYPE)) {
    return true;
  }

  return false;
}
#endif

IrType c_type_to_ir_type(CType *ctype)
{
  switch (ctype->t) {
  case VOID_TYPE: return (IrType){.t = IR_VOID};
  case INTEGER_TYPE: {
    u32 bit_width;
    switch (ctype->u.integer.type) {
    case BOOL:
    case CHAR: bit_width = 8; break;
    case SHORT: bit_width = 16; break;
    case INT: bit_width = 32; break;
    case LONG:
    case LONG_LONG: bit_width = 64; break;
    default: UNIMPLEMENTED;
    }

    return (IrType){
        .t = IR_INT,
        .u.bit_width = bit_width,
    };
  }
  case FLOAT_TYPE: {
    u32 float_bits;
    switch (ctype->u.floatt) {
    case FLOAT: float_bits = 32; break;
    case DOUBLE: float_bits = 64; break;
    case LONG_DOUBLE: float_bits = 80; break;
    }
    return (IrType){.t = IR_FLOAT, .u.float_bits = float_bits};
  }
  case POINTER_TYPE: return (IrType){.t = IR_POINTER};
  case ARRAY_TYPE: return *ctype->u.array.ir_type;
  case FUNCTION_TYPE: return (IrType){.t = IR_FUNCTION};
  case STRUCT_TYPE:
    assert(!ctype->u.strukt.incomplete);
    return *ctype->u.strukt.ir_type;
  }

  UNREACHABLE;
}

u8 c_type_rank(CType *type)
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

  UNREACHABLE;
}

void init_type_env(TypeEnv *type_env)
{
  pool_init(&type_env->pool, 512);
  ARRAY_INIT(&type_env->struct_types, TypeEnvEntry *, 10);
  ARRAY_INIT(&type_env->union_types, TypeEnvEntry *, 10);
  ARRAY_INIT(&type_env->enum_types, TypeEnvEntry *, 10);
  ARRAY_INIT(&type_env->typedef_types, TypeEnvEntry *, 10);

  // @PORT: Most of these types are x86-64 dependent.
  type_env->void_type = (CType){
      .t = VOID_TYPE,
  };
  type_env->bool_type = (CType){
      .t = INTEGER_TYPE,
      .u.integer.type = BOOL,
      .u.integer.is_signed = false,
  };
  type_env->char_type = (CType){
      .t = INTEGER_TYPE,
      .u.integer.type = CHAR,
      // System V x86-64 says "char" == "signed char"
      .u.integer.is_signed = true,
  };
  type_env->unsigned_char_type = (CType){
      .t = INTEGER_TYPE,
      .u.integer.type = CHAR,
      .u.integer.is_signed = false,
  };
  type_env->short_type = (CType){
      .t = INTEGER_TYPE,
      .u.integer.type = SHORT,
      .u.integer.is_signed = true,
  };
  type_env->unsigned_short_type = (CType){
      .t = INTEGER_TYPE,
      .u.integer.type = SHORT,
      .u.integer.is_signed = false,
  };
  type_env->int_type = (CType){
      .t = INTEGER_TYPE,
      .u.integer.type = INT,
      .u.integer.is_signed = true,
  };
  type_env->unsigned_int_type = (CType){
      .t = INTEGER_TYPE,
      .u.integer.type = INT,
      .u.integer.is_signed = false,
  };
  type_env->long_type = (CType){
      .t = INTEGER_TYPE,
      .u.integer.type = LONG,
      .u.integer.is_signed = true,
  };
  type_env->unsigned_long_type = (CType){
      .t = INTEGER_TYPE,
      .u.integer.type = LONG,
      .u.integer.is_signed = false,
  };
  type_env->long_long_type = (CType){
      .t = INTEGER_TYPE,
      .u.integer.type = LONG_LONG,
      .u.integer.is_signed = true,
  };
  type_env->unsigned_long_long_type = (CType){
      .t = INTEGER_TYPE,
      .u.integer.type = LONG_LONG,
      .u.integer.is_signed = false,
  };

  type_env->size_type = &type_env->unsigned_long_type;
  type_env->int_ptr_type = &type_env->unsigned_long_type;

  type_env->float_type = (CType){
      .t = FLOAT_TYPE,
      .u.floatt = FLOAT,
  };
  type_env->double_type = (CType){
      .t = FLOAT_TYPE,
      .u.floatt = DOUBLE,
  };
  type_env->long_double_type = (CType){
      .t = FLOAT_TYPE,
      .u.floatt = LONG_DOUBLE,
  };
}

CType *search(Array(TypeEnvEntry *) *types, char *name)
{
  for (u32 i = 0; i < types->size; i++) {
    TypeEnvEntry *entry = *ARRAY_REF(types, TypeEnvEntry *, i);
    if (streq(entry->name, name)) {
      return &entry->type;
    }
  }
  return NULL;
}

CType *pointer_type(TypeEnv *type_env, CType *type)
{
  CType *pointer_type = pool_alloc(&type_env->pool, sizeof *pointer_type);
  pointer_type->t = POINTER_TYPE;
  pointer_type->u.pointee_type = type;

  return pointer_type;
}

CType *decay_to_pointer(TypeEnv *type_env, CType *type)
{
  if (type->t == ARRAY_TYPE) {
    return pointer_type(type_env, type->u.array.elem_type);
  } else {
    return type;
  }
}

CType *array_type(IrBuilder *builder, TypeEnv *type_env, CType *type)
{
  CType *array_type = pool_alloc(&type_env->pool, sizeof *array_type);
  array_type->t = ARRAY_TYPE;
  array_type->u.array.elem_type = type;
  array_type->u.array.incomplete = true;

  IrType *ir_array_type =
      pool_alloc(&builder->module->pool, sizeof *ir_array_type);
  ir_array_type->t = IR_ARRAY;
  ir_array_type->u.array.size = 0;

  IrType elem_type = c_type_to_ir_type(type);
  IrType *ir_elem_type;

  if (elem_type.t == IR_ARRAY) {
    ir_elem_type = elem_type.u.array.elem_type;
    ir_array_type->u.array.size = elem_type.u.array.size;
  } else {
    ir_elem_type = pool_alloc(&builder->module->pool, sizeof *ir_elem_type);
    *ir_elem_type = c_type_to_ir_type(type);
  }
  ir_array_type->u.array.elem_type = ir_elem_type;
  array_type->u.array.ir_type = ir_array_type;

  return array_type;
}

void set_array_type_length(CType *array_type, u64 size)
{
  assert(array_type->t == ARRAY_TYPE);

  array_type->u.array.size = size;

  CType *elem_type = array_type->u.array.elem_type;
  if (elem_type->t == ARRAY_TYPE) {
    size *= elem_type->u.array.ir_type->u.array.size;
  }
  array_type->u.array.ir_type->u.array.size = size;
  array_type->u.array.incomplete = false;
}

CType *struct_type(TypeEnv *type_env, char *name)
{
  TypeEnvEntry *entry = pool_alloc(&type_env->pool, sizeof *entry);
  *ARRAY_APPEND(&type_env->struct_types, TypeEnvEntry *) = entry;
  if (name == NULL) name = "<anonymous struct>";
  entry->name = name;

  CType *type = &entry->type;
  type->t = STRUCT_TYPE;
  // Every struct starts out incomplete, until we add the fields.
  type->u.strukt.incomplete = true;
  ARRAY_INIT(&type->u.strukt.fields, CDecl, 5);

  return type;
}

u32 c_type_num_fields(CType *type)
{
  switch (type->t) {
  case STRUCT_TYPE: return type->u.strukt.fields.size;
  case ARRAY_TYPE: return type->u.array.size;
  default: UNREACHABLE;
  }
}

static bool value_fits_in_type(CType *type, u64 value)
{
  assert(type->t == INTEGER_TYPE);

  bool is_signed = type->u.integer.is_signed;
  u32 int_width = c_type_size(type) * 8;

  i64 min;
  i64 max;
  if (is_signed) {
    min = -(i64)(1ULL << (int_width - 1));
    max = (1ULL << (int_width - 1)) - 1;
  } else if (int_width == 64) {
    // Handle 64 specially, because we can't do 1 << 64
    return true;
  } else {
    min = 0;
    max = (1ULL << int_width) - 1;
  }

  return (i64)value >= min && (i64)value <= max;
}

static CType *type_that_fits(CType **types, u32 num_types, u64 value)
{
  for (u32 i = 0; i < num_types; i++) {
    CType *type = types[i];
    if (value_fits_in_type(type, value)) {
      return type;
    }
  }

  // @TODO: Error message for literals that don't fit into any of the types
  // in their list.
  UNIMPLEMENTED;
}

// @TODO: We also need to keep track of whether it's a hex/octal literal -
// there is a second column in the table with the types to use if so. For now
// we just treat all literals as decimal literals.
CType *type_of_int_literal(TypeEnv *type_env, IntLiteral int_literal)
{
  u64 value = int_literal.value;
  NumericSuffix suffix = int_literal.suffix;
  bool u_suffix = (suffix & UNSIGNED_SUFFIX) != 0;
  bool l_suffix = (suffix & LONG_SUFFIX) != 0;
  bool ll_suffix = (suffix & LONG_LONG_SUFFIX) != 0;

  assert(!l_suffix || !ll_suffix);

  // This follows the table given in C99 6.4.4.1.5
  if (suffix == NO_SUFFIX) {
    CType *types[] = {
        &type_env->int_type, &type_env->long_type, &type_env->long_long_type};
    return type_that_fits(types, STATIC_ARRAY_LENGTH(types), value);
  } else if (u_suffix && !l_suffix && !ll_suffix) {
    CType *types[] = {
        &type_env->unsigned_int_type,
        &type_env->unsigned_long_type,
        &type_env->unsigned_long_long_type,
    };
    return type_that_fits(types, STATIC_ARRAY_LENGTH(types), value);
  } else if (!u_suffix && l_suffix) {
    CType *types[] = {
        &type_env->long_type,
        &type_env->long_long_type,
    };
    return type_that_fits(types, STATIC_ARRAY_LENGTH(types), value);
  } else if (u_suffix && l_suffix) {
    CType *types[] = {
        &type_env->unsigned_long_type,
        &type_env->unsigned_long_long_type,
    };
    return type_that_fits(types, STATIC_ARRAY_LENGTH(types), value);
  } else if (!u_suffix && ll_suffix) {
    CType *types[] = {
        &type_env->long_long_type,
    };
    return type_that_fits(types, STATIC_ARRAY_LENGTH(types), value);
  } else if (u_suffix && ll_suffix) {
    CType *types[] = {
        &type_env->unsigned_long_long_type,
    };
    return type_that_fits(types, STATIC_ARRAY_LENGTH(types), value);
  } else {
    // The cases above should cover everything.
    UNREACHABLE;
  }
}

CType *type_of_float_literal(TypeEnv *type_env, FloatLiteral float_literal)
{
  NumericSuffix suffix = float_literal.suffix;
  if (suffix == FLOAT_SUFFIX) {
    return &type_env->float_type;
  } else if (suffix == LONG_SUFFIX) {
    return &type_env->long_double_type;
  }
  return &type_env->double_type;
}

static bool matches_sequence(
    ASTDeclSpecifier *decl_specifier_list, int length, ...)
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

CType *named_type_specifier_to_ctype(
    TypeEnv *type_env, ASTDeclSpecifier *decl_specifier_list)
{
  ASTTypeSpecifier *type_spec = decl_specifier_list->u.type_specifier;
  assert(type_spec->t == NAMED_TYPE_SPECIFIER);

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
      || matches_sequence(
          decl_specifier_list, 4, "signed", "long", "long", "int")) {
    return &type_env->long_long_type;
  }
  if (matches_sequence(decl_specifier_list, 3, "unsigned", "long", "long")
      || matches_sequence(
          decl_specifier_list, 4, "unsigned", "long", "long", "int")) {
    return &type_env->unsigned_long_long_type;
  }

  // Phew

  CType *type = search(&type_env->typedef_types, type_spec->u.name);
  assert(type != NULL);
  return type;
}
