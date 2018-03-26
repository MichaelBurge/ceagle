#lang ceagle

typedef struct pair {
  unsigned int a;
  unsigned int b;
} pair;

int main() {
  pair p;
  __builtin_set_test_result(300);
  p.a = 100;
  p.b = 200;
  return (p.a) + (p.b);
}
