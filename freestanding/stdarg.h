#ifndef _STDARG_H
#define _STDARG_H

// As defined in the System V x86-64 ABI spec, in section 3.5.7
typedef struct
{
  unsigned int next_int_reg_offset;
  unsigned int next_vector_reg_offset;
  void *next_stack_arg;
  void *register_save_area;
} va_list[1];

#define va_start(list, last_arg) __builtin_va_start(list)
#define va_arg(list, type) __builtin_va_arg(list, type)
#define va_end(list) __builtin_va_end(list)

static unsigned long __builtin_va_arg_uint64(va_list list)
{
  unsigned long result;
  if (list->next_int_reg_offset >= 48) {
    result = *(unsigned long *)list->next_stack_arg;
    list->next_stack_arg = (char *)list->next_stack_arg + 8;
  } else {
    result =
        *(unsigned long
              *)((char *)list->register_save_area + list->next_int_reg_offset);
    list->next_int_reg_offset += 8;
  }

  return result;
}

#endif
