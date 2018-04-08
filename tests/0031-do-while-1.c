#lang ceagle

int main() {
  __builtin_set_test_result(10);
  do {
    return 10;
  } while (0);
  return 5;
}
