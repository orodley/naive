// @TODO: Make this into a macro once we can support that in the compiler.
// @TODO: Call abort instead of just crashing.

void assert(int condition)
{
	if (!condition)
		*(volatile char *)0;
}
