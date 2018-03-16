#lang ceagle

int main() {
  __builtin_set_test_result(5);
  int x;
  int *y = &x;
  *y = 5;
  return x;
}
