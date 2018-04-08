#lang ceagle

typedef unsigned int uint;

uint f(int x) {
  return x < 0;
}

int main() {
  __builtin_set_test_result(1);
  return f(-1);
}
