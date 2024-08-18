int main(void)
{
  float a = 1.0f;
  float b = 2.0f;
  double c = 3.0;
  double d = 4.0;

  return (a == a) + (a != b) + (a < b) + (a <= b) + (b > a) + (b >= a)
         + (a == b) + (a != a) + (a < a) + (b <= a) + (a > a) + (a >= b)
         + (c == c) + (c != d) + (c < d) + (c <= d) + (d > c) + (d >= c)
         + (c == d) + (c != c) + (c < c) + (d <= c) + (c > c) + (c >= d) - 12;
}