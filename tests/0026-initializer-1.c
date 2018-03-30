#lang ceagle

int main() {
  static const unsigned char xs[] = { 10, 20, 30, 40 };
  __builtin_set_test_result(30);
  return xs[2];
}
