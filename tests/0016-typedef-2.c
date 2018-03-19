#lang ceagle

typedef unsigned int uint256_t;

typedef struct pair {
  uint256_t a;
  uint256_t b;
} pair;

int main() {
  __builtin_set_test_result(100);
  pair p;
  p.a = 100;
  return p.a;
}
