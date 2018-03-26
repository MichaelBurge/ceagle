#lang ceagle

int x = 0;

void f() { x++; }

int main() {
  __builtin_set_test_result(3);
  f();
  f();
  f();
  return x;
}
