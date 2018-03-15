#lang ceagle

int x(int a) { return a + 5; }

int main() {
  __builtin_set_test_result(15);
  return x(10);
}
