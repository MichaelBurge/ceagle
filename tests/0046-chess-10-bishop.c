#lang ceagle

#include <ceagle/test-support/chess-engine.c>
#include <ceagle/test-support/test-lib.c>
#include <ceagle/test-support/chess-debug.c>

void test_bishop()
{
  // Bishop southwest blocker
  {
    gamestate g = zerostate();
    g.pawns_bb = bit(mkPosition(3,1));
    g.kings_bb = bit(mkPosition(2,0));
    g.current_piece_bb = bit(mkPosition(7,5));
    g.bishops_bb = bit(mkPosition(7,5));

    {
      uint64_t expected =
        bit(mkPosition(6,6)) |
        bit(mkPosition(5,7)) |
        bit(mkPosition(6,4)) |
        bit(mkPosition(5,3)) |
        bit(mkPosition(4,2)) |
        bit(mkPosition(3,1));
      uint64_t actual = valid_bishop_moves(g, mkPosition(7,5));
      assert_equal_bb("test_bishop_1", expected, actual);
    }
    // King not in check from bishop
    g.current_piece_bb ^= all_pieces(g);
    assert("test_bishop_1_check", ! is_in_check(g));
  }
  // Bishop northeast blocker
  {
    gamestate g = zerostate();
    g.pawns_bb = bit(mkPosition(3,1));
    g.kings_bb = bit(mkPosition(2,0));
    g.current_piece_bb = all_pieces(g);
    g.bishops_bb = bit(mkPosition(7,5));
    {
      uint64_t expected =
        bit(mkPosition(6,6)) |
        bit(mkPosition(5,7)) |
        bit(mkPosition(6,4)) |
        bit(mkPosition(5,3)) |
        bit(mkPosition(4,2));
      uint64_t actual = valid_bishop_moves(g, mkPosition(7,5));
      assert_equal_bb("test_bishop_2", expected, actual);
    }
    // King not in check from bishop
    assert("test_bishop_2_check", ! is_in_check(g));
  }
}

int main()
{
  __builtin_set_test_result(1);
  __builtin_set_max_simulator_memory(200000000);
  __builtin_set_max_iterations(1000000000);
  test_bishop();
  return 1;
}
