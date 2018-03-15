#lang ceagle

int x() { return 5; }

int main() {
  __builtin_set_test_result(5);
  return x();
}
