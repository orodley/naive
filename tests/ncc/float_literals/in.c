// FLAGS: -dump-tokens -fsyntax-only

int main(void)
{
  return (
      2., 3.f, .4, .5F, 0.123, 1.13, 2.1E2L, .1e-1l, 3e+2, 0x1p1, 0x1.7FP-1,
      0x.3p1L, 0x0.fcp1f, 1000000000000000000000000000000.0l,
      0x100000000000000000000p0, 0);
}