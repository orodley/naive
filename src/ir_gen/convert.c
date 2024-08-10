#include "ir_gen/convert.h"

#include "ir.h"
#include "ir_gen/c_type.h"

Term convert_type(IrBuilder *builder, Term term, CType *target_type)
{
  if (c_type_eq(term.ctype, target_type)) return term;

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
  } else if (term.ctype->t == INTEGER_TYPE && target_type->t == FLOAT_TYPE) {
    IrType ir_type = c_type_to_ir_type(target_type);
    // @TODO: Handle unsigned int to float conversions. This is a bit more
    // complicated on the asm_gen side as there is no instruction for it.
    assert(term.ctype->u.integer.is_signed);
    converted =
        build_type_instr(builder, OP_SINT_TO_FLOAT, term.value, ir_type);
  } else if (term.ctype->t == INTEGER_TYPE && target_type->t == POINTER_TYPE) {
    u32 width = c_type_to_ir_type(term.ctype).u.bit_width;

    IrValue value = term.value;
    if (width < 64) {
      value = build_type_instr(
          builder, OP_ZEXT, term.value,
          (IrType){.t = IR_INT, .u.bit_width = 64});
    } else {
      assert(width == 64);
    }

    converted = build_type_instr(
        builder, OP_CAST, value, c_type_to_ir_type(target_type));
  } else if (term.ctype->t == POINTER_TYPE && target_type->t == INTEGER_TYPE) {
    converted = build_type_instr(
        builder, OP_CAST, term.value, c_type_to_ir_type(target_type));
  } else if (term.ctype->t == POINTER_TYPE && target_type->t == POINTER_TYPE) {
    converted = term.value;
  } else if (term.ctype->t == ARRAY_TYPE && target_type->t == POINTER_TYPE) {
    // Array values are only ever passed around as pointers to the first
    // element anyway, so this conversion is a no-op that just changes type.
    assert(term.value.type.t == IR_POINTER);
    converted = term.value;
  } else if (
      target_type->t == POINTER_TYPE && term.ctype->t == FUNCTION_TYPE
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

  return (Term){
      .ctype = target_type,
      .value = converted,
  };
}

void do_arithmetic_conversions(IrBuilder *builder, Term *left, Term *right)
{
  do_arithmetic_conversions_with_blocks(
      builder, left, builder->current_block, right, builder->current_block);
}

// @TODO: Implement this fully
void do_arithmetic_conversions_with_blocks(
    IrBuilder *builder, Term *left, IrBlock *left_block, Term *right,
    IrBlock *right_block)
{
  assert(left->ctype->t == INTEGER_TYPE && right->ctype->t == INTEGER_TYPE);

  IrBlock *original_block = builder->current_block;

  if (left->ctype->u.integer.is_signed == right->ctype->u.integer.is_signed) {
    if (c_type_rank(left->ctype) != c_type_rank(right->ctype)) {
      Term *to_convert;
      CType *conversion_type;
      IrBlock *conversion_block;
      if (c_type_rank(left->ctype) < c_type_rank(right->ctype)) {
        to_convert = left;
        conversion_type = right->ctype;
        conversion_block = left_block;
      } else {
        to_convert = right;
        conversion_type = left->ctype;
        conversion_block = right_block;
      }

      builder->current_block = conversion_block;
      *to_convert = convert_type(builder, *to_convert, conversion_type);
    }
  } else {
    Term *signed_term, *unsigned_term;
    IrBlock *signed_block, *unsigned_block;
    if (left->ctype->u.integer.is_signed) {
      signed_term = left;
      unsigned_term = right;
      signed_block = left_block;
      unsigned_block = right_block;
    } else {
      signed_term = right;
      unsigned_term = left;
      signed_block = right_block;
      unsigned_block = left_block;
    }

    if (c_type_rank(unsigned_term->ctype) >= c_type_rank(signed_term->ctype)) {
      builder->current_block = signed_block;
      *signed_term = convert_type(builder, *signed_term, unsigned_term->ctype);
    } else if (
        c_type_rank(signed_term->ctype) > c_type_rank(unsigned_term->ctype)) {
      builder->current_block = unsigned_block;
      *unsigned_term =
          convert_type(builder, *unsigned_term, signed_term->ctype);
    } else {
      UNIMPLEMENTED;
    }
  }

  builder->current_block = original_block;
}