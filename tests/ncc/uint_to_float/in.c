int main(void)
{
  unsigned char a = 1;
  unsigned short b = 2;
  unsigned int c = 3;
  unsigned long d = 4;

  float x = (float)a + (float)b + (float)c + (float)d;
  double y = (double)a + (double)b + (double)c + (double)d;
  return (int)x + (int)y - 20;
}
