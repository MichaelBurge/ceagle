#lang ceagle

int f(int x) {
  switch (x) {
  case 5: return 70;
  case 10: return 80;
  default: return 50;
  }
}

int main() {
  __builtin_set_test_result(200);
  return
    f(5)  + //   70
    f(10) + // + 80 = 150
    f(15)   // + 50 = 200
    ;
}
