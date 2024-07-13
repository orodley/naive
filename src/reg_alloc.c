#include "reg_alloc.h"

#include <assert.h>
#include <stdlib.h>

#include "asm.h"
#include "asm_gen.h"
#include "bit_set.h"
#include "flags.h"
#include "misc.h"

// Reserved for spills and fills.
#define SPILL_REGISTER REG_CLASS_R12

#define ALLOCATION_ORDER                                           \
  X(0, REG_CLASS_R13), X(1, REG_CLASS_R14), X(2, REG_CLASS_R15),   \
      X(3, REG_CLASS_B), X(4, REG_CLASS_R11), X(5, REG_CLASS_R10), \
      X(6, REG_CLASS_R9), X(7, REG_CLASS_R8), X(8, REG_CLASS_C),   \
      X(9, REG_CLASS_D), X(10, REG_CLASS_SI), X(11, REG_CLASS_DI), \
      X(12, REG_CLASS_A),

#define X(i, x) [i] = x
static RegClass alloc_index_to_reg[] = {ALLOCATION_ORDER};
#undef X

#define X(i, x) [x] = i
static u32 reg_to_alloc_index[] = {ALLOCATION_ORDER};
#undef X

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

static void compute_live_ranges(AsmBuilder *builder);
static int compare_live_range_start(const void *a, const void *b);
static bool is_def(AsmInstr *instr, u32 vreg_num, VReg *vreg);
static bool is_use(AsmInstr *instr, u32 vreg);
static bool references_vreg(AsmValue value, u32 vreg);

// @TODO: Save all caller save registers that are live across calls.
void allocate_registers(AsmBuilder *builder)
{
  compute_live_ranges(builder);

  Array(AsmInstr) *body = builder->current_block;

  u32 free_regs_bitset;
  assert(STATIC_ARRAY_LENGTH(alloc_index_to_reg) < 8 * sizeof free_regs_bitset);

  // Start with all regs free
  free_regs_bitset = (1 << STATIC_ARRAY_LENGTH(alloc_index_to_reg)) - 1;

  // virtual_registers isn't necessarily sorted by live range start.
  // i.e. if RAX is pre-allocated from a function call and we don't use it
  // until later. Linear scan depends on this ordering, so we sort it first.
  // We can't sort it in place because the indices are used to look vregs up
  // by vreg number.
  VReg **live_ranges =
      malloc(builder->virtual_registers.size * sizeof *live_ranges);
  u32 live_range_out_index = 0;
  for (u32 i = 0; i < builder->virtual_registers.size; i++) {
    VReg *vreg = ARRAY_REF(&builder->virtual_registers, VReg, i);
    // This indicates that we assigned a vreg to something that wasn't
    // used, e.g. a pre-alloced RAX for the return value of a function.
    if (vreg->live_range_start == -1) {
      assert(vreg->live_range_end == -1);
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

    while (active_vregs.size != 0) {
      VReg *active_vreg = *ARRAY_REF(&active_vregs, VReg *, 0);
      if (active_vreg->live_range_end >= vreg->live_range_start) break;

      switch (active_vreg->t) {
      case UNASSIGNED: UNREACHABLE;
      case IN_REG: {
        u32 alloc_index = reg_to_alloc_index[active_vreg->u.assigned_register];
        free_regs_bitset |= 1 << alloc_index;
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
      if (free_regs_bitset == 0) {
        vreg->t = ON_STACK;
        vreg->u.assigned_stack_slot = builder->local_stack_usage;
        builder->local_stack_usage += 8;
      } else {
        u32 first_free_alloc_index = lowest_set_bit(free_regs_bitset);
        vreg->t = IN_REG;
        vreg->u.assigned_register = alloc_index_to_reg[first_free_alloc_index];
        free_regs_bitset &= ~(1 << first_free_alloc_index);
      }
    } else {
      // This register has already been assigned, e.g. part of a call
      // sequence. We don't need to allocate it, but we do need to keep
      // track of it so it doesn't get clobbered.
      assert(vreg->t == IN_REG);
      assert(vreg->pre_alloced);

      u32 alloc_index = reg_to_alloc_index[vreg->u.assigned_register];
      if ((free_regs_bitset & (1 << alloc_index)) == 0) {
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
        assert(existing != NULL);

        // If the existing register is also pre-alloced, we have two
        // vregs with overlapping live ranges that need the same
        // physical register. This should never happen.
        assert(!existing->pre_alloced);

        existing->t = ON_STACK;
        // @TODO: Alignment
        existing->u.assigned_stack_slot = builder->local_stack_usage;
        builder->local_stack_usage += 8;
      } else {
        free_regs_bitset &= ~(1 << alloc_index);
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
      assert(active_vreg->t == IN_REG);

      // @TODO: Remove all the invalidated vregs at once instead of
      // repeatedly shifting down.
      ARRAY_REMOVE(&active_vregs, VReg *, 0);
    }
    if (vreg_index != builder->virtual_registers.size) {
      VReg *next_vreg =
          ARRAY_REF(&builder->virtual_registers, VReg, vreg_index);
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

  // @TODO: Move register dumping stuff we we can dump the name here rather
  // than just a number
  if (flag_dump_register_assignments) {
    for (u32 i = 0; i < builder->virtual_registers.size; i++) {
      VReg *vreg = ARRAY_REF(&builder->virtual_registers, VReg, i);
      printf("#%u =", i);
      switch (vreg->t) {
      case IN_REG: printf(" (%d)\n", vreg->u.assigned_register); break;
      case ON_STACK: printf(" [%d]\n", vreg->u.assigned_stack_slot); break;
      case UNASSIGNED: UNREACHABLE;
      }
    }
  }

  u32 curr_sp_diff = 0;
  for (u32 i = 0; i < body->size; i++) {
    AsmInstr *instr = ARRAY_REF(body, AsmInstr, i);

    if (instr->op == SUB && instr->args[0].t == ASM_VALUE_REGISTER
        && instr->args[0].u.reg.t == PHYS_REG
        && instr->args[0].u.reg.u.class == REG_CLASS_SP) {
      assert(instr->args[1].t == ASM_VALUE_CONST);

      AsmConst c = instr->args[1].u.constant;
      assert(c.t == ASM_CONST_IMMEDIATE);
      curr_sp_diff += c.u.immediate;
    }

    for (u32 j = 0; j < instr->arity; j++) {
      AsmValue *arg = instr->args + j;
      Register *reg = value_reg(arg);
      if (reg == NULL) continue;

      if (reg->t == V_REG) {
        u32 vreg_number = reg->u.vreg_number;
        VReg *vreg = ARRAY_REF(&builder->virtual_registers, VReg, vreg_number);

        switch (vreg->t) {
        case IN_REG:
          reg->t = PHYS_REG;
          reg->u.class = vreg->u.assigned_register;
          break;
        case ON_STACK:
          reg->t = PHYS_REG;
          reg->u.class = SPILL_REGISTER;
          // @TODO: Insert all at once, rather than shifting along
          // every time.
          // @TODO: Elide this when we just write to the register and
          // don't use the previous value
          *ARRAY_INSERT(body, AsmInstr, i) = (AsmInstr){
              .op = MOV,
              .arity = 2,
              .args[0] = asm_phys_reg(SPILL_REGISTER, 64),
              .args[1] = asm_deref(asm_offset_reg(
                  REG_CLASS_SP, 64,
                  asm_const_imm(vreg->u.assigned_stack_slot + curr_sp_diff))),
          };
          // @TODO: Elide this when we just read the register and
          // don't write anything back.
          *ARRAY_INSERT(body, AsmInstr, i + 2) = (AsmInstr){
              .op = MOV,
              .arity = 2,
              .args[0] = asm_deref(asm_offset_reg(
                  REG_CLASS_SP, 64,
                  asm_const_imm(vreg->u.assigned_stack_slot + curr_sp_diff))),
              .args[1] = asm_phys_reg(SPILL_REGISTER, 64),
          };
          break;
        case UNASSIGNED: UNREACHABLE;
        }
      }
    }
  }
}

static void compute_live_ranges(AsmBuilder *builder)
{
  Array(AsmInstr) *body = builder->current_block;

  // Liveness is a backwards analysis, so we need access to predecessors for
  // each instruction. These are stored intrusively on AsmSymbol.
  //
  // @NOTE: I'm not sure if this will still work correctly if we add
  // fallthrough, i.e.: eliminating redundant jumps like:
  //     jmp a
  // a:  ...
  Pool preds_pool;
  pool_init(&preds_pool, 512);
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

    assert(instr->args[0].t == ASM_VALUE_CONST);
    AsmConst c = instr->args[0].u.constant;
    assert(c.t == ASM_CONST_SYMBOL);
    AsmSymbol *target = c.u.symbol;

    if (target != builder->ret_label) {
      assert(target->offset < body->size);

      Pred **location = &target->pred;
      Pred *pred = target->pred;
      while (pred != NULL && pred->next != NULL) {
        pred = pred->next;
        location = &pred->next;
      }

      Pred *new_pred = pool_alloc(&preds_pool, sizeof *pred);
      new_pred->src_offset = i;
      new_pred->dest_offset = target->offset;
      new_pred->next = NULL;

      *location = new_pred;
    }
  }

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

  for (u32 vreg_num = 0; vreg_num < builder->virtual_registers.size;
       vreg_num++) {
    VReg *vreg = ARRAY_REF(&builder->virtual_registers, VReg, vreg_num);
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
        assert(!bit_set_get_bit(&liveness, pc));

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
          assert(instr->args[0].t == ASM_VALUE_CONST);
          AsmConst c = instr->args[0].u.constant;
          assert(c.t == ASM_CONST_SYMBOL);
          AsmSymbol *target = c.u.symbol;

          if (target != builder->ret_label) {
            assert(target->offset < body->size);
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

  pool_free(&preds_pool);
  bit_set_free(&liveness);
  bit_set_free(&working_set);

  if (flag_dump_live_ranges) {
    printf("%s:\n", builder->current_function->name);
    for (u32 i = 0; i < body->size; i++) {
      AsmInstr *instr = ARRAY_REF(body, AsmInstr, i);
      printf("%u ", i);
      dump_asm_instr(instr);
    }

    for (u32 i = 0; i < builder->virtual_registers.size; i++) {
      VReg *vreg = ARRAY_REF(&builder->virtual_registers, VReg, i);
      printf("#%u: [%d, %d]", i, vreg->live_range_start, vreg->live_range_end);
      switch (vreg->t) {
      // @TODO: Move register dumping stuff we we can dump the name here
      // rather than just a number
      case IN_REG: printf(" (%d)", vreg->u.assigned_register); break;
      case ON_STACK: printf(" [%d]", vreg->u.assigned_stack_slot); break;
      case UNASSIGNED: break;
      }
      putchar('\n');
    }
    putchar('\n');
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