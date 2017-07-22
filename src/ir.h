#ifndef NAIVE_IR_H_
#define NAIVE_IR_H_

#include "asm.h"
#include "array.h"
#include "pool.h"
#include "misc.h"

typedef struct TransUnit
{
	Array(IrGlobal *) globals;
	Array(IrType *) types;

	Pool pool;
} TransUnit;

typedef struct IrType
{
	enum
	{
		IR_VOID,
		IR_INT,
		IR_POINTER,
		IR_ARRAY,
		IR_FUNCTION,
		IR_STRUCT,
	} t;

	union
	{
		u8 bit_width;
		struct
		{
			// @TODO: This field probably shouldn't be here.
			char *name;
			struct IrStructField *fields;
			u32 total_size;
			u16 num_fields;
			u8 alignment;
		} strukt;
		struct
		{
			struct IrType *return_type;
			u32 arity;
			bool variable_arity;
			struct IrType *arg_types;
		} function;
		struct
		{
			struct IrType *elem_type;
			u64 size;
		} array;
	} u;
} IrType;

typedef struct IrStructField
{
	IrType type;
	u32 offset;
} IrStructField;

typedef struct IrBlock
{
	u32 id;
	char *name;
	Array(IrInstr *) instrs;

	// used by asm_gen
	AsmSymbol *label;
} IrBlock;

typedef struct IrFunction
{
	// @TODO: Do we even need this? We could just store the entry block, since
	// the 'cond' and 'branch' instrs at the end of each block form a CFG of
	// all the blocks that would be in this Array. Then we wouldn't have to do
	// fiddly stuff to make sure we emit the blocks in the right order, asm_gen
	// could just do the linearisation itself, in depth-first order or whatever
	// else it wants.
	Array(IrBlock *) blocks;
	u32 curr_instr_id;
} IrFunction;

typedef struct IrConst
{
	IrType type;

	union
	{
		u64 integer;
		struct IrGlobal *global_pointer;
		struct IrConst *array_elems;
		struct IrConst *struct_fields;
		IrFunction function;
	} u;
} IrConst;

typedef enum IrLinkage
{
	IR_GLOBAL_LINKAGE,
	IR_LOCAL_LINKAGE,
} IrLinkage;

typedef struct IrGlobal
{
	char *name;
	IrType type;
	IrLinkage linkage;
	AsmSymbol *asm_symbol;
	IrConst *initializer;

	// @TODO: We should find a better place to put this. It only makes sense
	// for functions, but we can't put it on IrFunction because we need it for
	// undefined functions too, to call them.
	CallSeq call_seq;
} IrGlobal;

typedef struct IrBuilder
{
	TransUnit *trans_unit;
	IrFunction *current_function;
	IrBlock *current_block;
} IrBuilder;


typedef struct IrValue
{
	enum
	{
		VALUE_CONST,
		VALUE_ARG,
		VALUE_INSTR,
		VALUE_GLOBAL,
	} t;
	// @TODO: Should this be removed? It's contained in most (all?) of the
	// union members below; we could just write a function which extracts it
	// instead of duplicating it.
	IrType type;

	union
	{
		// @TODO: Replace with an IrConst?
		u64 constant;
		struct IrInstr *instr;
		u32 arg_index;
		IrGlobal *global;
	} u;
} IrValue;

// @TODO: Maybe all the relational operations should be folded into OP_CMP with
// an enum arg that determines the type of comparision? They are all handled
// the same way besides the particular comparision op anyway.
#define IR_OPS \
	X(OP_INVALID), \
\
	X(OP_BIT_XOR), \
	X(OP_BIT_OR), \
	X(OP_BIT_AND), \
	X(OP_BIT_NOT), \
	X(OP_NEG), \
	X(OP_SHL), \
	X(OP_SHR), \
	X(OP_MUL), \
	X(OP_DIV), \
	X(OP_MOD), \
	X(OP_ADD), \
	X(OP_SUB), \
	X(OP_EQ), \
	X(OP_NEQ), \
	X(OP_GT), \
	X(OP_GTE), \
	X(OP_LT), \
	X(OP_LTE), \
	X(OP_CALL), \
	X(OP_CAST), \
	X(OP_ZEXT), \
	X(OP_SEXT), \
	X(OP_TRUNC), \
	X(OP_FIELD), \
	X(OP_LOAD), \
	X(OP_STORE), \
	X(OP_LOCAL), \
	X(OP_RET), \
	X(OP_RET_VOID), \
	X(OP_BRANCH), \
	X(OP_COND), \
	X(OP_PHI), \
\
	X(OP_BUILTIN_VA_START), \
	X(OP_BUILTIN_VA_ARG),

#define X(x) x
typedef enum IrOp
{
	IR_OPS
} IrOp;
#undef X

typedef struct IrPhiParam
{
	IrBlock *block;
	IrValue value;
} IrPhiParam;

typedef struct IrInstr
{
	u32 id;
	IrType type;
	IrOp op;
	i32 vreg_number; // used by asm_gen

	union
	{
		u64 constant;
		IrValue arg;
		struct
		{
			IrValue arg1;
			IrValue arg2;
		} binary_op;
		struct
		{
			IrValue ptr;
			IrType type;
			u32 field_number;
		} field;
		struct
		{
			IrValue pointer;
			IrType type;
		} load;
		struct
		{
			IrValue callee;
			u32 arity;
			IrValue *arg_array;
			IrType return_type;
		} call;
		struct
		{
			IrValue condition;
			IrBlock *then_block;
			IrBlock *else_block;
		} cond;
		IrBlock *target_block;
		struct
		{
			IrType type;
			u32 stack_offset; // used by asm_gen
		} local;
		struct
		{
			u32 arity;
			IrPhiParam *params;
		} phi;
	} u;
} IrInstr;

void trans_unit_init(TransUnit *trans_unit);
void trans_unit_free(TransUnit *trans_unit);
IrGlobal *trans_unit_add_function(TransUnit *trans_unit, char *name,
		IrType return_type, u32 arity, bool variable_arity, IrType *arg_types);
IrGlobal *trans_unit_add_var(TransUnit *trans_unit, char *name, IrType type);
IrType *trans_unit_add_struct(TransUnit *trans_unit, char *name, u32 num_fields);

void block_init(IrBlock *block, char *name, u32 id);

IrConst *add_init_to_function(TransUnit *trans_unit, IrGlobal *global);
IrBlock *add_block_to_function(
		TransUnit *trans_unit, IrFunction *function, char *name);

bool ir_type_eq(IrType *a, IrType *b);
u32 size_of_ir_type(IrType type);
u32 align_of_ir_type(IrType type);
void dump_ir_type(IrType type);

void dump_trans_unit(TransUnit *trans_unit);

void builder_init(IrBuilder *builder, TransUnit *trans_unit);
IrInstr *build_branch(IrBuilder *builder, IrBlock *block);
IrInstr *build_cond(IrBuilder *builder,
		IrValue condition, IrBlock *then_block, IrBlock *else_block);

IrValue value_const(IrType type, u64 constant);
IrValue value_arg(u32 arg_index, IrType type);
IrValue value_global(IrGlobal *global);

IrConst *add_int_const(IrBuilder *builder, IrType int_type, u64 value);
IrConst *add_global_const(IrBuilder *builder, IrGlobal *global);
IrConst *add_array_const(IrBuilder *builder, IrType type);
IrConst *add_struct_const(IrBuilder *builder, IrType type);

IrValue build_nullary_instr(IrBuilder *builder, IrOp op, IrType type);
IrValue build_unary_instr(IrBuilder *builder, IrOp op, IrValue arg);
IrValue build_binary_instr(IrBuilder *builder, IrOp op, IrValue arg1, IrValue arg2);
IrValue build_local(IrBuilder *builder, IrType type);
IrValue build_field(IrBuilder *builder, IrValue ptr, IrType type,
		u32 field_number);
IrValue build_load(IrBuilder *builder, IrValue pointer, IrType type);
IrValue build_store(IrBuilder *builder, IrValue pointer, IrValue value);
IrValue build_call(IrBuilder *builder, IrValue callee, IrType return_type, u32 arity,
		IrValue *arg_array);
IrValue build_type_instr(IrBuilder *builder, IrOp op, IrValue value, IrType result_type);
IrValue build_phi(IrBuilder *builder, IrType type, u32 arity);

void phi_set_param(IrValue phi, u32 index, IrBlock *source_block, IrValue value);

IrValue builtin_memcpy(IrBuilder *builder);
IrValue builtin_memset(IrBuilder *builder);

IrValue build_builtin_va_start(IrBuilder *builder, IrValue va_list_ptr);
IrValue build_builtin_va_arg(IrBuilder *builder, IrValue va_list_ptr,
		IrValue object_size);

#endif
