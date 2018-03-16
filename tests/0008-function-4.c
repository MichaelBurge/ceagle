#lang ceagle

typedef struct pair {
  unsigned int a;
  unsigned int b;
} pair;

pair make_pair() {
  pair ret;
  ret.a = 100;
  ret.b = 200;
  return ret;
}

int main() {
  __builtin_set_test_result(300);
  pair p = make_pair();
  return (p.a) + (p.b);
}
