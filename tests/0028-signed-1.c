#lang ceagle

int main() {
  __builtin_set_test_result(1);
  int x = -1;
  return x < 0;
}
