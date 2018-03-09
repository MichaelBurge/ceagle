#lang ceagle

int main() {
  __builtin_set_test_result(5);
  int x = 0;
 loop:;
  if (x == 5)
    goto term;
  x++;
  goto loop;
 term:;
  return x;
}
