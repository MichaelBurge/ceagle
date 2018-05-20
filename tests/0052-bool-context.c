#lang ceagle

int main()
{
  __builtin_set_test_result(1);
  return 2 == 2 && (2 & 2);
}
