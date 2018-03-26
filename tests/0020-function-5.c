#lang ceagle

int sum(int a, int b) { return a + b; }

int main() {
  __builtin_set_test_result(15);
  return sum(5, 10);
}
