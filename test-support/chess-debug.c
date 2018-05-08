#lang ceagle

void putchar(char c) {
  __builtin_print_char(c);
}

void puts(const char* s) {
  __builtin_print_string(s);
}

static void print_bitboard(uint64_t bb)
{
  for (int r = 8; r --> 0;) {
    for (int f = 0; f < 8; f++) {
      char c = is_bit_set(bb, mkPosition(f, r)) ? '1' : '0';
      putchar(c);
    }
    putchar('\n');
  }
}
