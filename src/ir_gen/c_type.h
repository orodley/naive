#ifndef NAIVE_IR_GEN_C_TYPE_H_
#define NAIVE_IR_GEN_C_TYPE_H_

#include "array.h"
#include "ir.h"
#include "misc.h"
#include "syntax/lex.h"
#include "syntax/parse.h"

typedef enum CKind
{
  VOID_TYPE,
  INTEGER_TYPE,
  FLOAT_TYPE,
  FUNCTION_TYPE,
  STRUCT_TYPE,
  POINTER_TYPE,
  ARRAY_TYPE,
} CKind;

typedef struct CType
{
  CKind t;

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
    enum
    {
      FLOAT,
      DOUBLE,
      LONG_DOUBLE,
    } floatt;
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

bool c_type_eq(CType *a, CType *b);
IrType c_type_to_ir_type(CType *ctype);
u8 c_type_rank(CType *type);

inline u32 c_type_size(CType *type)
{
  return size_of_ir_type(c_type_to_ir_type(type));
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

  CType *size_type;
  CType *int_ptr_type;

  CType float_type;
  CType double_type;
  CType long_double_type;
} TypeEnv;

void init_type_env(TypeEnv *type_env);
CType *search(Array(TypeEnvEntry *) *types, char *name);

CType *pointer_type(TypeEnv *type_env, CType *type);
CType *decay_to_pointer(TypeEnv *type_env, CType *type);

CType *array_type(IrBuilder *builder, TypeEnv *type_env, CType *type);
void set_array_type_length(CType *array_type, u64 size);

CType *struct_type(TypeEnv *type_env, char *name);

u32 c_type_num_fields(CType *type);

CType *type_of_int_literal(TypeEnv *type_env, IntLiteral int_literal);
CType *type_of_float_literal(TypeEnv *type_env, FloatLiteral float_literal);
CType *named_type_specifier_to_ctype(
    TypeEnv *type_env, ASTDeclSpecifier *decl_specifier_list);

bool is_arithmetic_type(CType *type);

#endif