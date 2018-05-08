#lang ceagle

int xs[] = { 10, 20, 30, 40, 50 };

int main() {
  __builtin_set_test_result(150);
  return xs[0] + xs[1] + xs[2] + xs[3] + xs[4];
}
