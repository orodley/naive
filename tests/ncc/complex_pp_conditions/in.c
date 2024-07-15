// FLAGS: -E
// clang-format off

#define FOO +1

#if 0 FOO
YES
#else
NO
#endif

#if !((25000 * 3 / defined(FOO)) && 192837239L)
NO
#else
YES
#endif
