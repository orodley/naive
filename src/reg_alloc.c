#include "reg_alloc.h"

#include <stdlib.h>

#include "asm.h"
#include "asm_gen.h"
#include "bit_set.h"
#include "flags.h"
#include "macros.h"
#include "types.h"

// Reserved for spills and fills.
#define INT_SPILL_REGISTER REG_CLASS_R12
#define FLOAT_SPILL_REGISTER REG_CLASS_XMM7

#define INT_ALLOCATION_ORDER                                       \
  X(0, REG_CLASS_R13), X(1, REG_CLASS_R14), X(2, REG_CLASS_R15),   \
      X(3, REG_CLASS_B), X(4, REG_CLASS_R11), X(5, REG_CLASS_R10), \
      X(6, REG_CLASS_R9), X(7, REG_CLASS_R8), X(8, REG_CLASS_C),   \
      X(9, REG_CLASS_D), X(10, REG_CLASS_SI), X(11, REG_CLASS_DI), \
      X(12, REG_CLASS_A),

#define X(i, x) [i] = x
static RegClass int_alloc_index_to_reg[] = {INT_ALLOCATION_ORDER};
#undef X

#define X(i, x) [x] = i
static u32 int_reg_to_alloc_index[] = {INT_ALLOCATION_ORDER};
#undef X

#define FLOAT_ALLOCATION_ORDER                                          \
  X(0, REG_CLASS_XMM6), X(1, REG_CLASS_XMM5), X(2, REG_CLASS_XMM4),     \
      X(3, REG_CLASS_XMM3), X(4, REG_CLASS_XMM2), X(5, REG_CLASS_XMM1), \
      X(6, REG_CLASS_XMM0),

#define X(i, x) [i] = x
static RegClass float_alloc_index_to_reg[] = {FLOAT_ALLOCATION_ORDER};
#undef X

#define X(i, x) [x] = i
static u32 float_reg_to_alloc_index[] = {FLOAT_ALLOCATION_ORDER};
#undef X

typedef struct RegAllocParams
{
  u32 allocatable_regs;
  RegClass *alloc_index_to_reg;
  u32 *reg_to_alloc_index;
  RegClass spill_register;
} RegAllocParams;

static RegAllocParams reg_alloc_params[] = {
    [REG_TYPE_INTEGER] =
        {
            .allocatable_regs = STATIC_ARRAY_LENGTH(int_alloc_index_to_reg),
            .alloc_index_to_reg = int_alloc_index_to_reg,
            .reg_to_alloc_index = int_reg_to_alloc_index,
            .spill_register = INT_SPILL_REGISTER,
        },
    [REG_TYPE_FLOAT] =
        {
            .allocatable_regs = STATIC_ARRAY_LENGTH(float_alloc_index_to_reg),
            .alloc_index_to_reg = float_alloc_index_to_reg,
            .reg_to_alloc_index = float_reg_to_alloc_index,
            .spill_register = FLOAT_SPILL_REGISTER,
        },
};

#define CALLER_SAVE_REGS_BITMASK                                   \
  ((1 << REG_CLASS_A) | (1 << REG_CLASS_DI) | (1 << REG_CLASS_SI)  \
   | (1 << REG_CLASS_D) | (1 << REG_CLASS_C) | (1 << REG_CLASS_R8) \
   | (1 << REG_CLASS_R9) | (1 << REG_CLASS_R10) | (1 << REG_CLASS_R11))

typedef struct Pred
{
  u32 dest_offset;
  u32 src_offset;

  struct Pred *next;
} Pred;

static void find_preds(AsmBuilder *builder, Pool *pool);
static void compute_live_ranges(AsmBuilder *builder, Array(VReg) *vregs);
static int compare_live_range_start(const void *a, const void *b);
static bool is_def(AsmInstr *instr, u32 vreg_num, VReg *vreg);
static bool is_use(AsmInstr *instr, u32 vreg);
static bool references_vreg(AsmValue value, u32 vreg);
static void dump_vregs(Array(VReg) *vregs);

// @TODO: Save all caller save registers that are live across calls.
void allocate_registers(AsmBuilder *builder)
{
  Pool preds_pool;
  find_preds(builder, &preds_pool);

  Array(VReg) *vregs = &builder->virtual_registers;

  compute_live_ranges(builder, vregs);

  Array(AsmInstr) *body = builder->current_block;

  ASSERT(reg_alloc_params[REG_TYPE_INTEGER].allocatable_regs < 8 * sizeof(u32));
  ASSERT(reg_alloc_params[REG_TYPE_FLOAT].allocatable_regs < 8 * sizeof(u32));

  // Start with all regs free
  u32 free_regs_bitsets[] = {
      [REG_TYPE_INTEGER] =
          (1 << reg_alloc_params[REG_TYPE_INTEGER].allocatable_regs) - 1,
      [REG_TYPE_FLOAT] =
          (1 << reg_alloc_params[REG_TYPE_FLOAT].allocatable_regs) - 1,
  };

  // vregs isn't necessarily sorted by live range start.
  // i.e. if RAX is pre-allocated from a function call and we don't use it
  // until later. Linear scan depends on this ordering, so we sort it first.
  // We can't sort it in place because the indices are used to look vregs up
  // by vreg number.
  VReg **live_ranges = malloc(vregs->size * sizeof *live_ranges);
  u32 live_range_out_index = 0;
  for (u32 i = 0; i < vregs->size; i++) {
    VReg *vreg = ARRAY_REF(vregs, VReg, i);
    // This indicates that we assigned a vreg to something that wasn't
    // used, e.g. a pre-alloced RAX for the return value of a function.
    if (vreg->live_range_start == -1) {
      ASSERT(vreg->live_range_end == -1, "Live range has a start but no end");
      continue;
    }

    live_ranges[live_range_out_index] = vreg;
    live_range_out_index++;
  }
  u32 num_live_ranges = live_range_out_index;
  qsort(
      live_ranges, num_live_ranges, sizeof *live_ranges,
      compare_live_range_start);

  Array(VReg *) active_vregs;
  ARRAY_INIT(&active_vregs, VReg *, 16);
  for (u32 i = 0; i < num_live_ranges; i++) {
    VReg *vreg = live_ranges[i];
    RegAllocParams params = reg_alloc_params[vreg->type];
    u32 *free_regs_bitset = &free_regs_bitsets[vreg->type];

    while (active_vregs.size != 0) {
      VReg *active_vreg = *ARRAY_REF(&active_vregs, VReg *, 0);
      if (active_vreg->live_range_end >= vreg->live_range_start) break;

      switch (active_vreg->t) {
      case UNASSIGNED: UNREACHABLE;
      case IN_REG: {
        RegAllocParams params = reg_alloc_params[active_vreg->type];
        u32 *free_regs_bitset = &free_regs_bitsets[active_vreg->type];
        u32 alloc_index =
            params.reg_to_alloc_index[active_vreg->u.assigned_register];
        *free_regs_bitset |= 1 << alloc_index;
        break;
      }
      case ON_STACK:
        // @TODO: Reuse the spilled stack slot?
        break;
      }

      // @TODO: Remove all the invalidated vregs at once instead of
      // repeatedly shifting down.
      ARRAY_REMOVE(&active_vregs, VReg *, 0);
    }

    if (vreg->t == UNASSIGNED) {
      if (*free_regs_bitset == 0) {
        vreg->t = ON_STACK;
        vreg->u.assigned_stack_slot = builder->local_stack_usage;
        builder->local_stack_usage += 8;
      } else {
        u32 first_free_alloc_index = lowest_set_bit(*free_regs_bitset);
        vreg->t = IN_REG;
        vreg->u.assigned_register =
            params.alloc_index_to_reg[first_free_alloc_index];
        *free_regs_bitset &= ~(1 << first_free_alloc_index);
      }
    } else {
      // This register has already been assigned, e.g. part of a call
      // sequence. We don't need to allocate it, but we do need to keep
      // track of it so it doesn't get clobbered.
      ASSERT(vreg->t == IN_REG);
      ASSERT(vreg->pre_alloced);

      u32 alloc_index = params.reg_to_alloc_index[vreg->u.assigned_register];
      if ((*free_regs_bitset & (1 << alloc_index)) == 0) {
        // Already allocated to something else. We need to spill the
        // existing value, because we're pre-alloced and we need this
        // specific register.

        VReg *existing = NULL;
        for (u32 i = 0; i < active_vregs.size; i++) {
          VReg *active_vreg = *ARRAY_REF(&active_vregs, VReg *, i);
          if (active_vreg->t == IN_REG
              && active_vreg->u.assigned_register
                     == vreg->u.assigned_register) {
            existing = active_vreg;
            break;
          }
        }
        ASSERT(existing != NULL);

        // If the existing register is also pre-alloced, we have two
        // vregs with overlapping live ranges that need the same
        // physical register. This should never happen.
        ASSERT(!existing->pre_alloced);

        existing->t = ON_STACK;
        // @TODO: Alignment
        existing->u.assigned_stack_slot = builder->local_stack_usage;
        builder->local_stack_usage += 8;
      } else {
        *free_regs_bitset &= ~(1 << alloc_index);
      }
    }

    u32 insertion_point = active_vregs.size;
    for (u32 j = 0; j < active_vregs.size; j++) {
      VReg *active_vreg = *ARRAY_REF(&active_vregs, VReg *, j);
      if (active_vreg->live_range_end > vreg->live_range_end) {
        insertion_point = j;
        break;
      }
    }
    *ARRAY_INSERT(&active_vregs, VReg *, insertion_point) = vreg;
  }
  free(live_ranges);

  array_clear(&active_vregs);
  u32 vreg_index = 0;
  for (u32 i = 0; i < body->size; i++) {
    while (active_vregs.size != 0) {
      VReg *active_vreg = *ARRAY_REF(&active_vregs, VReg *, 0);
      if (active_vreg->live_range_end == -1) continue;
      if ((u32)active_vreg->live_range_end >= i) break;
      ASSERT(active_vreg->t == IN_REG);

      // @TODO: Remove all the invalidated vregs at once instead of
      // repeatedly shifting down.
      ARRAY_REMOVE(&active_vregs, VReg *, 0);
    }
    if (vreg_index != vregs->size) {
      VReg *next_vreg = ARRAY_REF(vregs, VReg, vreg_index);
      // Pre-alloced vregs aren't counted. Otherwise we'd think we need
      // to spill registers we just used to pass arguments.
      // @TODO: Perhaps this is too weak of a condition? It'd be nice if
      // we could still catch errors, where we accidentally pre-allocated
      // a caller-save register across a callsite.
      if (next_vreg->t == IN_REG && !next_vreg->pre_alloced
          && (u32)next_vreg->live_range_start == i) {
        u32 insertion_point = active_vregs.size;
        for (u32 j = 0; j < active_vregs.size; j++) {
          VReg *active_vreg = *ARRAY_REF(&active_vregs, VReg *, j);
          if (active_vreg->live_range_end == -1) continue;
          if ((u32)active_vreg->live_range_end < i) {
            insertion_point = j;
            break;
          }
        }
        *ARRAY_INSERT(&active_vregs, VReg *, insertion_point) = next_vreg;

        vreg_index++;
      }
    }

    AsmInstr *instr = ARRAY_REF(body, AsmInstr, i);
    if (instr->op == CALL) {
      for (u32 j = 0; j < active_vregs.size; j++) {
        VReg *vreg = *ARRAY_REF(&active_vregs, VReg *, j);
        if (vreg->t == IN_REG) {
          RegClass reg = vreg->u.assigned_register;
          if (((1 << reg) & CALLER_SAVE_REGS_BITMASK) == 0) continue;

          vreg->t = ON_STACK;
          vreg->u.assigned_stack_slot = builder->local_stack_usage;
          builder->local_stack_usage += 8;

          // @TODO: Remove all the spilled vregs at once instead of
          // repeatedly shifting down.
          ARRAY_REMOVE(&active_vregs, VReg *, j);
          j--;
        }
      }
    }
  }
  array_free(&active_vregs);

  if (flag_dump_register_assignments) {
    dump_vregs(vregs);
  }

  u32 curr_sp_diff = 0;
  for (u32 i = 0; i < body->size; i++) {
    AsmInstr *instr = ARRAY_REF(body, AsmInstr, i);

    if (instr->op == SUB && instr->args[0].t == ASM_VALUE_REGISTER
        && instr->args[0].u.reg.t == PHYS_REG
        && instr->args[0].u.reg.u.class == REG_CLASS_SP) {
      ASSERT(instr->args[1].t == ASM_VALUE_CONST);

      AsmConst c = instr->args[1].u.constant;
      ASSERT(c.t == ASM_CONST_IMMEDIATE);
      curr_sp_diff += c.u.immediate;
    }

    for (u32 j = 0; j < instr->arity; j++) {
      AsmValue *arg = instr->args + j;
      Register *reg = value_reg(arg);
      if (reg == NULL) continue;
      if (reg->t != V_REG) continue;

      u32 vreg_number = reg->u.vreg_number;
      VReg *vreg = ARRAY_REF(vregs, VReg, vreg_number);

      switch (vreg->t) {
      case IN_REG:
        reg->t = PHYS_REG;
        reg->u.class = vreg->u.assigned_register;
        break;
      case ON_STACK: {
        RegClass spill_register = reg_alloc_params[vreg->type].spill_register;
        reg->t = PHYS_REG;
        reg->u.class = spill_register;
        // @TODO: Insert all at once, rather than shifting along
        // every time.
        AsmOp op;
        switch (vreg->type) {
        case REG_TYPE_INTEGER: op = MOV; break;
        case REG_TYPE_FLOAT:
          op = arg->u.reg.value_width == 32 ? MOVSS : MOVSD;
          break;
        }

        // @TODO: Elide this when we just write to the register and
        // don't use the previous value
        *ARRAY_INSERT(body, AsmInstr, i) = (AsmInstr){
            .op = op,
            .arity = 2,
            .args[0] = asm_phys_reg(spill_register, arg->u.reg.width),
            .args[1] = asm_deref(asm_offset_reg(
                REG_CLASS_SP, 64,
                asm_const_imm(vreg->u.assigned_stack_slot + curr_sp_diff))),
        };
        // @TODO: Elide this when we just read the register and
        // don't write anything back.
        *ARRAY_INSERT(body, AsmInstr, i + 2) = (AsmInstr){
            .op = op,
            .arity = 2,
            .args[0] = asm_deref(asm_offset_reg(
                REG_CLASS_SP, 64,
                asm_const_imm(vreg->u.assigned_stack_slot + curr_sp_diff))),
            .args[1] = asm_phys_reg(spill_register, arg->u.reg.width),
        };
        break;
      }
      case UNASSIGNED: UNREACHABLE;
      }
    }
  }

  pool_free(&preds_pool);
}

// Liveness is a backwards analysis, so we need access to predecessors for
// each instruction. These are stored intrusively on AsmSymbol.
//
// @NOTE: I'm not sure if this will still work correctly if we add
// fallthrough, i.e.: eliminating redundant jumps like:
//     jmp a
// a:  ...
static void find_preds(AsmBuilder *builder, Pool *pool)
{
  pool_init(pool, 512);
  Array(AsmInstr) *body = builder->current_block;
  for (u32 i = 0; i < body->size; i++) {
    AsmInstr *instr = ARRAY_REF(body, AsmInstr, i);
    switch (instr->op) {
    case JMP:
    case JE:
    case JNE:
    case JG:
    case JGE:
    case JL:
    case JLE:
    case JA:
    case JAE:
    case JB:
    case JBE: break;
    default: continue;
    }

    ASSERT(instr->args[0].t == ASM_VALUE_CONST);
    AsmConst c = instr->args[0].u.constant;
    ASSERT(c.t == ASM_CONST_SYMBOL);
    AsmSymbol *target = c.u.symbol;

    if (target != builder->ret_label) {
      ASSERT(target->offset < body->size);

      Pred **location = &target->pred;
      Pred *pred = target->pred;
      while (pred != NULL && pred->next != NULL) {
        pred = pred->next;
        location = &pred->next;
      }

      Pred *new_pred = pool_alloc(pool, sizeof *pred);
      new_pred->src_offset = i;
      new_pred->dest_offset = target->offset;
      new_pred->next = NULL;

      *location = new_pred;
    }
  }
}

static void compute_live_ranges(AsmBuilder *builder, Array(VReg) *vregs)
{
  Array(AsmInstr) *body = builder->current_block;

  // This implements the algorithm described in Mohnen 2002, "A Graph-Free
  // Approach to Data-Flow Analysis". As the name suggests, we do without
  // constructing an explicit CFG. See Section 4 in the paper for details.
  // Note the paragraph at the end of Section 4 - in this case the analysis
  // is backwards and existential, so both of the adaptations in this
  // paragraph are applied to the algorithm in Figure 5. The preprocessing to
  // connect jumps to jump targets is done just above.
  BitSet liveness;
  bit_set_init(&liveness, body->size);
  BitSet working_set;
  bit_set_init(&working_set, body->size);

  for (u32 vreg_num = 0; vreg_num < vregs->size; vreg_num++) {
    VReg *vreg = ARRAY_REF(vregs, VReg, vreg_num);
    if (vreg->live_range_start != -1 && vreg->live_range_end != -1) continue;

    bit_set_clear_all(&liveness);
    bit_set_set_all(&working_set);
    u32 largest_working_set_elem = body->size - 1;

    while (!bit_set_is_empty(&working_set)) {
      u32 pc = largest_working_set_elem;

      for (;;) {
        bit_set_set_bit(&working_set, pc, false);
        if (pc == largest_working_set_elem) {
          for (i32 i = pc / 64; i >= 0; i--) {
            u64 bits = working_set.bits[i];
            if (bits != 0) {
              largest_working_set_elem = 64 * i + highest_set_bit(bits);
              break;
            }
          }
        }

        // We shouldn't be re-examining points that are already live.
        // If we've shown a vreg to be live at pc the analysis below
        // will add nothing.
        ASSERT(!bit_set_get_bit(&liveness, pc));

        // @TODO: We might be able to simplify this somewhat based on
        // the fact that (I think) our instruction selection always
        // produces SSA form for vregs.

        AsmInstr *instr = ARRAY_REF(body, AsmInstr, pc);
        i32 succ0 = -1, succ1 = -1;
        switch (instr->op) {
        case JE:
        case JNE:
        case JG:
        case JGE:
        case JL:
        case JLE:
        case JA:
        case JAE:
        case JB:
        case JBE:
          succ1 = pc + 1;
          // fallthrough.
        case JMP: {
          ASSERT(instr->args[0].t == ASM_VALUE_CONST);
          AsmConst c = instr->args[0].u.constant;
          ASSERT(c.t == ASM_CONST_SYMBOL);
          AsmSymbol *target = c.u.symbol;

          if (target != builder->ret_label) {
            ASSERT(target->offset < body->size);
            succ0 = target->offset;
          }
          break;
        }
        default:
          if (pc != body->size - 1) succ0 = pc + 1;
          break;
        }

        bool new;
        if (is_use(instr, vreg_num)) {
          new = true;
        } else if (!((succ0 != -1
                      && (bit_set_get_bit(&liveness, succ0)
                          && !is_def(
                              ARRAY_REF(body, AsmInstr, succ0), vreg_num,
                              vreg)))
                     || (succ1 != -1
                         && (bit_set_get_bit(&liveness, succ1)
                             && !is_def(
                                 ARRAY_REF(body, AsmInstr, succ1), vreg_num,
                                 vreg))))) {
          // The big hairy condition above checks if all of our
          // successors are either not live or a def. If so then
          // we're not live.
          new = false;
        } else {
          // Otherwise, we have a successor that is live and not a
          // def, hence we're live.
          new = true;
        }
        bit_set_set_bit(&liveness, pc, new);

        u32 prev;
        Pred *pred = NULL;
        if (instr->label != NULL) pred = instr->label->pred;

        if (pred != NULL && pred->dest_offset == pc) {
          prev = pred->src_offset;
        } else if (pc == 0) {
          break;
        } else {
          prev = pc - 1;
        }

        // Check if we need to add stuff to the working set to deal
        // with multiple predecessors. We only bother adding
        // predecessors to the working set if we've refined. If !new
        // then we definitely haven't, and we can skip the whole loop.
        // The second half of the refinement check is the liveness
        // check before adding to working_set below.
        if (new &&pred != NULL) {
          pred = pred->next;
          while (pred != NULL) {
            u32 src = pred->src_offset;
            u32 dest = pred->dest_offset;

            if (dest != pc) break;

            if (!bit_set_get_bit(&liveness, src)) {
              bit_set_set_bit(&working_set, src, true);
              if (src > largest_working_set_elem)
                largest_working_set_elem = src;
            }
          }
        }

        if (!(new && !bit_set_get_bit(&liveness, prev))) break;

        pc = prev;
      }
    }

    i32 lowest = bit_set_lowest_set_bit(&liveness);
    if (vreg->live_range_start == -1 || vreg->live_range_start > lowest)
      vreg->live_range_start = lowest;

    i32 highest = bit_set_highest_set_bit(&liveness);
    if (vreg->live_range_end == -1
        || (highest != -1 && vreg->live_range_end < highest))
      vreg->live_range_end = highest;
  }

  bit_set_free(&liveness);
  bit_set_free(&working_set);

  if (flag_dump_live_ranges) {
    printf("%s:\n", builder->current_function->name);
    for (u32 i = 0; i < body->size; i++) {
      AsmInstr *instr = ARRAY_REF(body, AsmInstr, i);
      dump_asm_instr_with_line_number(instr, i);
    }

    putchar('\n');
    dump_vregs(vregs);
  }
}

static int compare_live_range_start(const void *a, const void *b)
{
  VReg *live_range_a = *(VReg **)a, *live_range_b = *(VReg **)b;
  int x = live_range_a->live_range_start, y = live_range_b->live_range_start;
  if (x < y) return -1;
  if (x == y) return 0;
  return 1;
}

static bool is_def(AsmInstr *instr, u32 vreg_num, VReg *vreg)
{
  switch (instr->op) {
  // Special case for pre-allocated return value vregs.
  case CALL:
    return vreg->pre_alloced && vreg->u.assigned_register == REG_CLASS_A;

  case CDQ:
  case CQO:
    return vreg->pre_alloced
           && vreg->u.assigned_register == REG_CLASS_D
           // We're using "is_use" just to check that we're in the vreg_deps
           // for this instr - we don't want to return true for some random
           // register that is also pre-allocated to REG_CLASS_D.
           && is_use(instr, vreg_num);

  // Special case for zeroing a register by XOR'ing with itself
  case XOR:
    return references_vreg(instr->args[0], vreg_num)
           && references_vreg(instr->args[1], vreg_num);

  case MOV:
  case MOVSX:
  case MOVZX:
  case MOVSS:
  case MOVSD:
  case MOVAPS:
  case CVTSD2SI:
  case CVTSI2SD:
  case CVTSI2SS:
  case CVTSS2SI:
  case CVTSS2SD:
  case CVTSD2SS:
  case POP:
  case IMUL:
  case SETE:
  case SETNE:
  case SETG:
  case SETGE:
  case SETL:
  case SETLE: return references_vreg(instr->args[0], vreg_num);

  default: return false;
  }
}

static bool is_use(AsmInstr *instr, u32 vreg)
{
  for (u32 i = 0; i < instr->arity; i++) {
    if (references_vreg(instr->args[i], vreg)) {
      return true;
    }
  }
  for (u32 i = 0; i < instr->num_deps; i++) {
    if (instr->vreg_deps[i] == vreg) {
      return true;
    }
  }

  return false;
}

bool references_vreg(AsmValue value, u32 vreg)
{
  Register reg;
  switch (value.t) {
  case ASM_VALUE_REGISTER: reg = value.u.reg; break;
  case ASM_VALUE_OFFSET_REGISTER: reg = value.u.offset_register.reg; break;
  default: return false;
  }

  return reg.t == V_REG && reg.u.vreg_number == vreg;
}

static void dump_vregs(Array(VReg) *vregs)
{
  for (u32 i = 0; i < vregs->size; i++) {
    VReg *vreg = ARRAY_REF(vregs, VReg, i);
    printf("#%u%c", i, vreg->type == REG_TYPE_INTEGER ? 'i' : 'f');

    if (vreg->live_range_start != -1 && vreg->live_range_end != -1) {
      printf(": [%d, %d]", vreg->live_range_start, vreg->live_range_end);
    }

    switch (vreg->t) {
    case IN_REG: {
      RegClass reg = vreg->u.assigned_register;
      u8 width_to_dump = reg_class_is_gpr(reg) ? 64 : 128;
      fputs(" = (", stdout);
      dump_phys_reg(reg, width_to_dump);
      puts(")");
      break;
    }
    case ON_STACK: printf(" = [%d]\n", vreg->u.assigned_stack_slot); break;
    case UNASSIGNED: putchar('\n'); break;
    }
  }
  putchar('\n');
}