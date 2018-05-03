#lang ceagle

int main()
{
  __builtin_set_test_result(-1);
  __bits 8 x = -1;
  return (x+1)-1;
}
