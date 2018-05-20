#lang ceagle

#include <ceagle/test-support/chess-engine.c>
#include <ceagle/test-support/test-lib.c>
#include <ceagle/test-support/chess-debug.c>

void test_check()
{
  // King in check 1
  {
    uint64_t center = mkPosition(3,3);
    gamestate g = zerostate();
    g.kings_bb = bit(center);
    g.rooks_bb = bit(center+2);
    g.current_player_bb = g.kings_bb;

    assert("test_check_1", is_in_check(g));
  }
  // King in check 2
  {
    uint64_t center = mkPosition(3,3);
    gamestate g = zerostate();
    g.kings_bb = bit(center);
    g.rooks_bb = bit(center-2*RANK);
    g.current_player_bb = g.kings_bb;

    assert("test_check_2", is_in_check(g));
  }
  // King blocked by rooks
  {
    uint64_t center = mkPosition(3,3);
    gamestate g = zerostate();
    g.kings_bb = bit(center);
    g.rooks_bb =
      bit(mkPosition(0,4)) |
      bit(mkPosition(0,2)) |
      bit(mkPosition(2,0));
    //bit(mkPosition(0,2));
    g.current_player_bb = g.kings_bb;
    assert_equal_int("test_check_3", 1, num_legal_moves(g));
  }
  // King can safely move to a square protected by own unit.
  {
    gamestate g = zerostate();
    g.pawns_bb =
      bit(mkPosition(3,1)) |
      bit(mkPosition(4,0)) |
      bit(mkPosition(4,1)) |
      bit(mkPosition(2,1));
    g.kings_bb = bit(mkPosition(3,0));
    g.bishops_bb =
      bit(mkPosition(0,2)) |
      bit(mkPosition(0,5));
    g.knights_bb =
      bit(mkPosition(1,0)) |
      bit(mkPosition(6,0));
    g.current_piece_bb = all_pieces(g);
    g.bishops_bb |= bit(mkPosition(7,5));
    {
      uint64_t expected =
        bit(mkPosition(6,6)) |
        bit(mkPosition(5,7)) |
        bit(mkPosition(6,4)) |
        bit(mkPosition(5,3)) |
        bit(mkPosition(4,2));
      uint64_t actual = valid_bishop_moves(g, mkPosition(7,5));
      assert_equal_bb("test_check_4_bishop", expected, actual);
    }
    // King not in check from bishop
    assert("test_check_4_check", ! is_in_check(g));
    {
      int from = mkPosition(3,0);
      int target = mkPosition(2,0);
      uint64_t expected = bit(target);
      uint64_t actual = piece_legal_movepoints(g, from);
      assert_equal_bb("test_check_4_king_valid", bit(target), valid_king_moves(g, from));
      iterator i = zerostate();
      int piece = get_piece(g, from);
      i = set_piece_bb(i, piece, bit(from));
      i = reset_iterator_moves(g,i);
      i = legalize(g,i);

      assert_equal_bb("test_check_4_next", bit(target), i.current_piece_bb);
      assert_equal_bb("test_check_4_moves", expected, actual);
    }
  }
}

int main()
{
  __builtin_set_test_result(1);
  __builtin_set_max_simulator_memory(2000000000);
  __builtin_set_max_iterations(100000000000);
  test_check();
  return 1;
}
