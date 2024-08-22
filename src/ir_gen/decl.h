#ifndef NAIVE_IR_GEN_DECL_H_
#define NAIVE_IR_GEN_DECL_H_

#include "ir_gen/c_type.h"
#include "ir_gen/context.h"
#include "syntax/ast.h"

IrGlobal *ir_global_for_decl(
    IrGenContext *ctx, ASTDeclSpecifier *decl_specifier_list,
    ASTDeclarator *declarator, ASTInitializer *initializer,
    CType **result_c_type);
void decl_to_cdecl(
    IrGenContext *ctx, CType *ident_type, ASTDeclarator *declarator,
    CDecl *cdecl);
CType *decl_specifier_list_to_c_type(
    IrGenContext *ctx, ASTDeclSpecifier *decl_specifier_list);
void cdecl_to_binding(IrBuilder *builder, CDecl *cdecl, Binding *binding);

#endif