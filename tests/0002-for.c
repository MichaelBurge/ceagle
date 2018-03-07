#lang ceagle

int main() {
  __builtin_set_test_result(120);
  int acc = 1;
  for (int i = 5; i > 1; i--) {
    acc *= i;
  }
  return acc;
}
