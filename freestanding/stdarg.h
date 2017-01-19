#ifndef _STDARG_H
#define _STDARG_H

typedef int va_list;

#define va_start(list, last_arg) __builtin_va_start(&(list))
#define va_arg(list, type) (*((type *)__builtin_va_arg(&(list), sizeof(type))))
#define va_end(args)

#endif
