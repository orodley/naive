#ifndef _STDARG_H
#define _STDARG_H

// As defined in the System V x86-64 ABI spec, in section 3.5.7
typedef struct {
	unsigned int next_int_reg_offset;
	unsigned int next_vector_reg_offset;
	void *next_stack_arg;
	void *register_save_area;
} va_list[1];

#define va_start(list, last_arg) __builtin_va_start(list)
#define va_arg(list, type) __builtin_va_arg(list, type)
#define va_end(args) __builtin_va_end(list)

static int __builtin_va_arg_int(va_list list)
{
	int result;
	if (list->next_int_reg_offset >= 48) {
		result = (int)*(long *)list->next_stack_arg;
		list->next_stack_arg = (char *)list->next_stack_arg + 8;
	} else {
		result = (int)*(long *)
			((char *)list->register_save_area + list->next_int_reg_offset);
		list->next_int_reg_offset += 8;
	}

	return result;
}

#endif
