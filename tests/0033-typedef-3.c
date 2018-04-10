#lang ceagle

typedef unsigned int uint64_t;
typedef int bool;

typedef struct x {
  int a;
} x;

typedef x y;

int main() {
  __builtin_set_test_result(100);
  y a;
  a.a = 100;
  a = a;
  return a.a;
}
