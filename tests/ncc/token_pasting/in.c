// FLAGS: -E

#define FOO(a, b) BAR(a, b)
#define BAR(a, b) a##b
#define BAZ(a, b) \
  a /* foo  */    \
      ##b

FOO(1, 2)
BAR(abc, 342)
FOO(foo, bar)
BAZ(wh, at)
