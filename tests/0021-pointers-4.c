#lang ceagle

int main() {
  int x = 5;
  __builtin_set_test_result(5);
  int *y = &x;
  return *y++;
}
