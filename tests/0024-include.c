#lang ceagle

#require <ceagle/test-support/0024.c>

int main() {
  __builtin_set_test_result(5);
  return f();
}
