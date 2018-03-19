#lang ceagle

typedef union pair {
  unsigned int a;
  unsigned int b;
} pair;

int main() {
  __builtin_set_test_result(100);
  pair p;
  p.a = 100;
  return p.b;
}
