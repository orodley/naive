#ifndef NAIVE_IR_GEN_CONTEXT_HH
#define NAIVE_IR_GEN_CONTEXT_HH

#include "array.h"
#include "ir.h"
#include "ir_gen/c_type.h"

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

Binding *binding_for_name(Scope *scope, char *name);

typedef struct IrGenContext
{
  IrBuilder *builder;
  Scope *global_scope;
  Scope *scope;
  TypeEnv type_env;
  CType *current_function_type;
  Array(InlineFunction) inline_functions;
  Array(SwitchCase) case_labels;
  Array(GotoLabel) goto_labels;
  Array(GotoFixup) goto_fixups;
  IrBlock *break_target;
  IrBlock *continue_target;
  IrFunction *scratch_function;
} IrGenContext;

#endif