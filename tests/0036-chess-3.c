#lang ceagle

int x = (1 << 255) << 2;
//int y = 1 << 256;
//__bits 64 RAY_A8H1 = ((__bits 64)(1) << 63) << 2;

int main() {
  __builtin_set_test_result(1234);
  __builtin_set_max_iterations(1000000000);
  __builtin_set_max_simulator_memory(20000000);
  return 1234;
}
