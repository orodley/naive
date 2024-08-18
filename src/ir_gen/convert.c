#include "ir_gen/convert.h"

#include "exit_code.h"
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
  } else if (term.ctype->t == FLOAT_TYPE && target_type->t == FLOAT_TYPE) {
    IrType ir_type = c_type_to_ir_type(target_type);

    if (c_type_to_ir_type(term.ctype).u.float_bits > ir_type.u.float_bits) {
      converted = build_type_instr(builder, OP_TRUNCF, term.value, ir_type);
    } else {
      converted = build_type_instr(builder, OP_EXTF, term.value, ir_type);
    }
  } else if (term.ctype->t == INTEGER_TYPE && target_type->t == FLOAT_TYPE) {
    IrType ir_type = c_type_to_ir_type(target_type);
    IrOp op =
        term.ctype->u.integer.is_signed ? OP_SINT_TO_FLOAT : OP_UINT_TO_FLOAT;
    converted = build_type_instr(builder, op, term.value, ir_type);
  } else if (term.ctype->t == FLOAT_TYPE && target_type->t == INTEGER_TYPE) {
    IrType ir_type = c_type_to_ir_type(target_type);
    // @TODO: Handle float to unsigned int conversions.
    assert(target_type->u.integer.is_signed);
    converted =
        build_type_instr(builder, OP_FLOAT_TO_SINT, term.value, ir_type);
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
    UNIMPLEMENTED("Conversion from %u to %u", term.ctype->t, target_type->t);
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
  assert(is_arithmetic_type(left->ctype) && is_arithmetic_type(right->ctype));

  IrBlock *original_block = builder->current_block;

  int left_is_float = left->ctype->t == FLOAT_TYPE;
  int right_is_float = right->ctype->t == FLOAT_TYPE;
  if (left_is_float || right_is_float
      || left->ctype->u.integer.is_signed
             == right->ctype->u.integer.is_signed) {
    int left_type_priority;
    int right_type_priority;
    if (left_is_float || right_is_float) {
      left_type_priority = left_is_float ? left->ctype->u.floatt : -1;
      right_type_priority = right_is_float ? right->ctype->u.floatt : -1;
    } else {
      left_type_priority = c_type_rank(left->ctype);
      right_type_priority = c_type_rank(right->ctype);
    }
    if (left_type_priority != right_type_priority) {
      Term *to_convert;
      CType *conversion_type;
      IrBlock *conversion_block;
      if (left_type_priority < right_type_priority) {
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
    } else {
      builder->current_block = unsigned_block;
      *unsigned_term =
          convert_type(builder, *unsigned_term, signed_term->ctype);
    }
  }

  builder->current_block = original_block;
}