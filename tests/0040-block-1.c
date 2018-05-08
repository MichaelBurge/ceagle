#lang ceagle

int main() {
  __builtin_set_test_result(1);
  int x = 1;
  {
    int x = 0;
  }
  return x;
}
