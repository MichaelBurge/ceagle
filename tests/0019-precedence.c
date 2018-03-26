#lang ceagle

// http://en.cppreference.com/w/c/language/operator_precedence

typedef struct pair {
  unsigned int *l;
  unsigned int r;
} pair;

int main() {
  unsigned int x = 1;
  pair p;
  p.l = &x;
  p.r = 10;
  unsigned int *y = &p.r;
  __builtin_set_test_result(20);

  // A: p    = { l: &x, r: 0 }
  // B: B.l  = &x
  // C: C++  = &x
  // D: *C   = x = 1
  // E: +3*D = 3
  // F: -2+E = 1
  // G: 2<<F = 4
  // J: 4==G = 1
  // K: J&0xFFFFFFFF = 1
  // L: K&&1 = 1
  // M: 10,20 = 20
  // N: *y %= 3 = 1
  // O: L?M:N = 20
  // RESULT: 20
  return 4 == 2 << -2 + +3 * *p.l++ &
    0xFFFFFFFF &&
    1 ? 10, 20 : (*y %= 3);
}
