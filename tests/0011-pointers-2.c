#lang ceagle

typedef struct pair {
  unsigned int a;
  unsigned int b;
} pair;

pair make_pair() {
  pair x;
  x.a = 100;
  x.b = 200;
  return x;
}

int main() {
  __builtin_set_test_result(700);
  pair p = make_pair();
  int *x = &(p.b);
  *x += 500;
  return p.b;
}
