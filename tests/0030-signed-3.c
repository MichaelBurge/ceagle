#lang ceagle

int main()
{
  __builtin_set_test_result(256);
  unsigned int b = -8;
  return 1 << -b;
}
