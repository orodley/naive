// FLAGS: -fsyntax-only -dump-tokens

#if 0
#if 0
#define FOO no
#endif
#else
#define FOO yes
#endif

#if 1
#define BAR yep
#else
#define BAR nope
#endif

#define ONE 1
#if ONE
#define BAZ ok
#else
#define BAZ not_ok;
#endif

int FOO;
int BAR;
int BAZ;
