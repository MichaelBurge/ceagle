#lang ceagle

// BEGIN C++ COMPATIBILITY
typedef unsigned __bits 64 uint64_t;

static uint64_t bit(uint64_t idx) {
  if (idx >= 64 || idx < 0) {
    return 0;
  }
  return ((uint64_t)(1) << idx);
}

int main() {
  __builtin_set_test_result(1234);
  return bit(27);
}
