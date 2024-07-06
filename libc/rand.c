static unsigned long __seed = 0x22DD9F15693587B8;

int rand(void)
{
  __seed ^= (__seed << 21);
  __seed ^= (__seed >> 35);
  __seed ^= (__seed << 4);

  return (int)__seed;
}

// Both in the same TU because they're very frequently used together, and srand
// is very small anyway.
void srand(unsigned int seed) { __seed = seed; }
