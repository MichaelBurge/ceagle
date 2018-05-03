#lang ceagle

int main() {
  __builtin_set_test_result(0);
  unsigned __bits 64 x = 18446744073709551616-1;
  return x+1;
}
