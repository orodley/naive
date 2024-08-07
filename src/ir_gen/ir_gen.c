#include "ir_gen/ir_gen.h"

#include <assert.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#include "array.h"
#include "diagnostics.h"
#include "ir.h"
#include "ir_gen/c_type.h"
#include "ir_gen/context.h"
#include "ir_gen/convert.h"
#include "ir_gen/decl.h"
#include "ir_gen/expr.h"
#include "ir_gen/initializer.h"
#include "ir_gen/statement.h"
#include "syntax/parse.h"
#include "util.h"

static void ir_gen_toplevel(IrGenContext *ctx, ASTToplevel *toplevel);
static void ir_gen_function(
    IrGenContext *ctx, IrGlobal *global, CType *function_type,
    ASTFunctionDef *function_def);
static ASTParameterDecl *params_for_function_declarator(
    ASTDeclarator *declarator);

IrModule ir_gen(ASTToplevel *toplevel)
{
  IrModule ir_module;
  ir_module_init(&ir_module);

  IrBuilder builder;
  builder_init(&builder, &ir_module);

  Scope global_scope;
  global_scope.parent_scope = NULL;
  Array(Binding) *global_bindings = &global_scope.bindings;
  ARRAY_INIT(global_bindings, Binding, 10);

  // This is used for sizeof expr. We switch to this function, ir_gen the
  // expression, and then switch back, keeping only the type of the resulting
  // Term.
  IrGlobal *scratch_function = ir_module_add_function(
      builder.module, "__scratch", (IrType){.t = IR_VOID}, 0, false, NULL);
  add_init_to_function(builder.module, scratch_function);

  IrGenContext ctx;
  ctx.builder = &builder;
  init_type_env(&ctx.type_env);
  ctx.scope = &global_scope;
  ctx.current_function_type = NULL;
  ctx.inline_functions = EMPTY_ARRAY;
  ctx.case_labels = EMPTY_ARRAY;
  ctx.goto_labels = EMPTY_ARRAY;
  ctx.goto_fixups = EMPTY_ARRAY;
  ctx.break_target = NULL;
  ctx.continue_target = NULL;
  ctx.scratch_function = &scratch_function->initializer->u.function;

  while (toplevel != NULL) {
    ir_gen_toplevel(&ctx, toplevel);
    toplevel = toplevel->next;
  }

  // @TODO: Do this once per function and reset size to 0 afterwards.
  for (u32 i = 0; i < ctx.goto_fixups.size; i++) {
    GotoFixup *fixup = ARRAY_REF(&ctx.goto_fixups, GotoFixup, i);
    assert(fixup->instr->op == OP_BRANCH);
    assert(fixup->instr->u.target_block == NULL);

    for (u32 j = 0; j < ctx.goto_labels.size; j++) {
      GotoLabel *label = ARRAY_REF(&ctx.goto_labels, GotoLabel, j);
      if (streq(label->name, fixup->label_name)) {
        fixup->instr->u.target_block = label->block;
        break;
      }
    }
    assert(fixup->instr->u.target_block != NULL);
  }

  IrGlobal *first_global = *ARRAY_REF(&builder.module->globals, IrGlobal *, 0);
  assert(streq(first_global->name, "__scratch"));
  ARRAY_REMOVE(&builder.module->globals, IrGlobal *, 0);

  pool_free(&ctx.type_env.pool);
  array_free(&ctx.goto_labels);
  array_free(&ctx.goto_fixups);
  array_free(&ctx.type_env.struct_types);
  array_free(&ctx.type_env.union_types);
  array_free(&ctx.type_env.enum_types);
  array_free(&ctx.type_env.typedef_types);
  array_free(global_bindings);

  return ir_module;
}

static void ir_gen_toplevel(IrGenContext *ctx, ASTToplevel *toplevel)
{
  switch (toplevel->t) {
  case FUNCTION_DEF: {
    ASTFunctionDef *func = toplevel->u.function_def;
    ASTDeclSpecifier *decl_specifier_list = func->decl_specifier_list;

    IrLinkage linkage = IR_GLOBAL_LINKAGE;
    while (decl_specifier_list->t == STORAGE_CLASS_SPECIFIER) {
      switch (decl_specifier_list->u.storage_class_specifier) {
      case STATIC_SPECIFIER: linkage = IR_LOCAL_LINKAGE; break;
      default: UNIMPLEMENTED;
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

    CType *global_type;
    IrGlobal *global = ir_global_for_decl(
        ctx, decl_specifier_list, declarator, NULL, &global_type);
    global->linkage = linkage;

    Binding *binding = ARRAY_APPEND(&ctx->scope->bindings, Binding);
    binding->name = global->name;
    binding->constant = false;
    binding->term.ctype = global_type;
    binding->term.value = value_global(global);

    if (is_inline) {
      *ARRAY_APPEND(&ctx->inline_functions, InlineFunction) = (InlineFunction){
          .global = global,
          .function_type = global_type,
          .function_def =
              {
                  .decl_specifier_list = decl_specifier_list,
                  .declarator = declarator,
                  .old_style_param_decl_list = func->old_style_param_decl_list,
                  .body = func->body,
              },
      };
    } else {
      ir_gen_function(ctx, global, global_type, func);
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
        && decl_specifier_list->next->u.function_specifier
               == INLINE_SPECIFIER) {
      decl_specifier_list = decl_specifier_list->next->next;

      CType *decl_spec_type =
          decl_specifier_list_to_c_type(ctx, decl_specifier_list);
      CDecl cdecl;
      decl_to_cdecl(ctx, decl_spec_type, init_declarator->declarator, &cdecl);

      InlineFunction *matching = NULL;

      for (u32 i = 0; i < ctx->inline_functions.size; i++) {
        InlineFunction *inline_function =
            ARRAY_REF(&ctx->inline_functions, InlineFunction, i);
        if (streq(inline_function->global->name, cdecl.name)) {
          assert(c_type_eq(cdecl.type, inline_function->function_type));
          matching = inline_function;
          break;
        }
      }
      assert(matching != NULL);

      ir_gen_function(
          ctx, matching->global, matching->function_type,
          &matching->function_def);
    } else if (
        decl_specifier_list->t == STORAGE_CLASS_SPECIFIER
        && decl_specifier_list->u.storage_class_specifier
               == TYPEDEF_SPECIFIER) {
      assert(init_declarator != NULL);
      decl_specifier_list = decl_specifier_list->next;
      CType *decl_spec_type =
          decl_specifier_list_to_c_type(ctx, decl_specifier_list);

      while (init_declarator != NULL) {
        assert(init_declarator->initializer == NULL);
        CDecl cdecl;
        decl_to_cdecl(ctx, decl_spec_type, init_declarator->declarator, &cdecl);

        TypeEnvEntry *new_type_alias =
            pool_alloc(&ctx->type_env.pool, sizeof *new_type_alias);
        *ARRAY_APPEND(&ctx->type_env.typedef_types, TypeEnvEntry *) =
            new_type_alias;
        new_type_alias->name = cdecl.name;
        new_type_alias->type = *cdecl.type;

        init_declarator = init_declarator->next;
      }
    } else {
      ASTDeclSpecifier *type_specs = decl_specifier_list;
      while (type_specs != NULL && type_specs->t == STORAGE_CLASS_SPECIFIER) {
        type_specs = type_specs->next;
      }
      assert(type_specs != NULL);

      if (init_declarator == NULL) {
        decl_specifier_list_to_c_type(ctx, type_specs);
      } else {
        assert(init_declarator->next == NULL);
        ASTDeclarator *declarator = init_declarator->declarator;

        // @TODO: Multiple declarators in one global decl.
        CType *global_type;
        IrGlobal *global = ir_global_for_decl(
            ctx, type_specs, declarator, init_declarator->initializer,
            &global_type);
        bool is_extern = global_type->t == FUNCTION_TYPE;

        Binding *binding = ARRAY_APPEND(&ctx->scope->bindings, Binding);
        binding->name = global->name;
        binding->constant = false;
        binding->term.ctype = global_type;
        binding->term.value = value_global(global);

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
            global->initializer = zero_initializer(ctx->builder, global_type);
          }
        } else {
          assert(!is_extern);

          Pool c_init_pool;
          pool_init(&c_init_pool, sizeof(CInitializer) * 5);

          CInitializer c_init;
          make_c_initializer(
              ctx, &c_init_pool, global_type, init, true, &c_init);
          assert(c_type_eq(c_init.type, global_type));

          global->initializer = const_gen_c_init(ctx->builder, &c_init);
          pool_free(&c_init_pool);
        }
      }
    }

    break;
  }
  }
}

static void ir_gen_function(
    IrGenContext *ctx, IrGlobal *global, CType *function_type,
    ASTFunctionDef *function_def)
{
  IrBuilder *builder = ctx->builder;
  IrConst *konst = add_init_to_function(builder->module, global);
  IrFunction *function = &konst->u.function;

  builder->current_function = function;
  builder->current_block = *ARRAY_REF(&function->blocks, IrBlock *, 0);

  Scope function_scope;
  push_scope(ctx, &function_scope);

  ctx->current_function_type = function_type;

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

    CType *decl_spec_type =
        decl_specifier_list_to_c_type(ctx, param->decl_specifier_list);
    CDecl cdecl;
    decl_to_cdecl(ctx, decl_spec_type, param->declarator, &cdecl);

    if (cdecl.type->t == VOID_TYPE) {
      assert(i == 0);
      assert(cdecl.name == NULL);
      assert(param->next == NULL);
      break;
    }

    Binding *binding = ARRAY_APPEND(&ctx->scope->bindings, Binding);

    // @HACK: We have to do this because decl_to_cdecl does extra stuff to
    // adjust parameter types when it knows that the declarator is for a
    // parameter. The proper fix is just to not re-process at all.
    cdecl.type = function_type->u.function.arg_type_array[i];
    cdecl_to_binding(builder, &cdecl, binding);

    u32 ir_arg_index = i;
    if (function_type->u.function.return_type->t == STRUCT_TYPE) ir_arg_index++;
    Term arg = {
        .ctype = cdecl.type,
        .value = value_arg(
            ir_arg_index, global->type.u.function.arg_types[ir_arg_index]),
    };
    ir_gen_assign_op(ctx, binding->term, arg, OP_INVALID, NULL);
  }

  ir_gen_statement(ctx, function_def->body);

  Array(IrInstr *) *instrs = &builder->current_block->instrs;
  if (instrs->size == 0
      || ((*ARRAY_LAST(instrs, IrInstr *))->op != OP_RET
          && (*ARRAY_LAST(instrs, IrInstr *))->op != OP_RET_VOID)) {
    // @NOTE: We emit a ret_void here even if the function doesn't return
    // void. This ret is purely to ensure that every block ends in a
    // terminating instruction (ret, ret_void, branch, or cond) as it makes
    // it easier for us. We don't emit a warning because we don't know if
    // this block is actually reachable.
    build_nullary_instr(builder, OP_RET_VOID, (IrType){.t = IR_VOID});
  }

  pop_scope(ctx);
}

static ASTParameterDecl *params_for_function_declarator(
    ASTDeclarator *declarator)
{
  while (declarator->t != DIRECT_DECLARATOR) {
    assert(declarator->t == POINTER_DECLARATOR);
    declarator = declarator->u.pointer_declarator.pointee;
  }

  assert(declarator->t == DIRECT_DECLARATOR);
  ASTDirectDeclarator *direct_declarator = declarator->u.direct_declarator;
  assert(direct_declarator->t == FUNCTION_DECLARATOR);

  return direct_declarator->u.function_declarator.parameters;
}