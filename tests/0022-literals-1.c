#lang ceagle

int f() { return 0x0F0F0F0F0F0F0F0FULL; }

int main() {
  __builtin_set_test_result(1085102592571150095);
  return f();
}
