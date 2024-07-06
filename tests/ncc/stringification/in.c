// FLAGS: -E

#define S(x) #x
#define T(x) S(x)
#define U(x)     \
#/*  foobar */ \
      x

S(foo)
S("foo")
T(bar)
U(123)
