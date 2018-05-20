#lang ceagle

#include <ceagle/test-support/chess-engine.c>
#include <ceagle/test-support/test-lib.c>
#include <ceagle/test-support/chess-debug.c>

void test_knight()
{
  // Knight in left bottom
  {
    uint64_t center = mkPosition(1,0);
    gamestate g = zerostate();
    g.knights_bb = bit(center);
    g.current_piece_bb = g.knights_bb;

    uint64_t expected =
      bit(mkPosition(0,2)) |
      bit(mkPosition(2,2)) |
      bit(mkPosition(3,1));
    uint64_t actual = valid_knight_moves(g, center);
    assert_equal_bb("test_knight_1", expected, actual);
  }
  // Knight in right bottom
  {
    uint64_t center = mkPosition(6,0);
    gamestate g = zerostate();
    g.knights_bb = bit(center);
    g.current_piece_bb = g.knights_bb;

    uint64_t expected =
      bit(mkPosition(7,2)) |
      bit(mkPosition(5,2)) |
      bit(mkPosition(4,1));
    uint64_t actual = valid_knight_moves(g, center);
    assert_equal_bb("test_knight_2", expected, actual);
  }
}

int main()
{
  __builtin_set_test_result(1);
  __builtin_set_max_simulator_memory(200000000);
  __builtin_set_max_iterations(1000000000);
  test_knight();
  return 1;
}
