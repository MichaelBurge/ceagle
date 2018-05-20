#lang ceagle

#include <ceagle/test-support/chess-engine.c>
#include <ceagle/test-support/test-lib.c>
#include <ceagle/test-support/chess-debug.c>

void test_apply_move()
{
  // Knight in middle
  {
    uint64_t center = mkPosition(3,3);
    gamestate g = zerostate();
    g.knights_bb = bit(center);
    g.current_piece_bb = g.knights_bb;

    iterator i = mkIterator(g);
    move m = dereference_iterator(i);
    assert_equal_int("test_apply_move_from", center, m.from);
    assert_equal_int("test_apply_move_to", mkPosition(2,1), m.to);
    gamestate g2 = swap_board(apply_move(g, m));

    assert_equal_bb("test_apply_move", bit(mkPosition(2,1)), g2.knights_bb);
  }
  // Doesn't affect other pieces
  {
    uint64_t center = mkPosition(3,3);
    gamestate g = zerostate();
    g.knights_bb = bit(center);
    g.current_piece_bb = g.knights_bb;
    g.knights_bb |=
      bit(mkPosition(1,0)) |
      bit(mkPosition(6,0));

    iterator i = mkIterator(g);
    move m = dereference_iterator(i);
    assert_equal_int("test_apply_move_from", center, m.from);
    assert_equal_int("test_apply_move_to", mkPosition(2,1), m.to);
    gamestate g2 = swap_board(apply_move(g, m));

    uint64_t expected = bit(m.to) | bit(mkPosition(1,0)) | bit(mkPosition(6,0));
    assert_equal_bb("test_apply_move_2", expected, g2.knights_bb);
  }
}

int main()
{
  __builtin_set_test_result(1);
  __builtin_set_max_simulator_memory(200000000);
  __builtin_set_max_iterations(1000000000);
  test_apply_move();
  return 1;
}
