#lang ceagle

int main() {
  __builtin_set_test_result(5);
  int i = 0;
  do {
    i++;
  } while (i != 5);
  return i;
}
