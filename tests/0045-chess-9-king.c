#lang ceagle

#include <ceagle/test-support/chess-engine.c>
#include <ceagle/test-support/test-lib.c>
#include <ceagle/test-support/chess-debug.c>

void test_king()
{
  // King in middle
  {
    uint64_t center = mkPosition(3,3);
    gamestate g = zerostate();
    g.kings_bb = bit(center);
    g.current_piece_bb = g.kings_bb;

    uint64_t expected =
      bit(mkPosition(4,3)) |
      bit(mkPosition(4,4)) |
      bit(mkPosition(3,4)) |
      bit(mkPosition(2,4)) |
      bit(mkPosition(2,3)) |
      bit(mkPosition(2,2)) |
      bit(mkPosition(3,2)) |
      bit(mkPosition(4,2));
    uint64_t actual = valid_king_moves(g, center);
    assert_equal_bb("test_king_1", expected, actual);
  }
  // King blocked on 3 sides in left corner
  {
    uint64_t center = mkPosition(0,0);
    gamestate g = zerostate();
    g.kings_bb = bit(center);
    g.pawns_bb =
      bit(mkPosition(1,0)) |
      bit(mkPosition(1,1)) |
      bit(mkPosition(0,1));
    g.current_piece_bb = all_pieces(g);

    uint64_t expected = 0;
    uint64_t actual = valid_king_moves(g, center);
    assert_equal_bb("test_king_2", expected, actual);
  }
}

int main()
{
  __builtin_set_test_result(1);
  __builtin_set_max_simulator_memory(200000000);
  __builtin_set_max_iterations(1000000000);
  test_king();
  return 1;
}
