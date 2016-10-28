#ifndef _ASSERT_H
#define _ASSERT_H

#define assert(x) if (!(x)) __assert_fail()

// @TODO: void in argument list. Right now the below actually means no
// arguments because we haven't implemented non-strict prototypes.
void __assert_fail();

#endif
