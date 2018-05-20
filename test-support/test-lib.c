#lang ceagle

void assert_equal_bb(const char* message, uint64_t x, uint64_t y)
{
  if (x != y) {
    // printf("ASSERTION FAILURE: %s\n", message);
    // printf("== expected\n");
    // print_bitboard(x);
    // printf("== actual\n");
    // print_bitboard(y);
    __builtin_print_string(message);
    __builtin_print_word(x);
    __builtin_print_word(y);
    __builtin_trap();
  }
}

void assert_equal_int(const char* message, int x, int y)
{
  assert_equal_bb(message, x, y);
}

void assert_equal_u64(const char* message, uint64_t x, uint64_t y)
{
  assert_equal_bb(message, x, y);
}

void assert(const char* message, int cond)
{
  if (!cond) {
    __builtin_print_string(message);
    __builtin_trap();
  }
}
