#lang ceagle

#include <ceagle/test-support/chess-engine.c>
#include <ceagle/test-support/test-lib.c>
#include <ceagle/test-support/chess-debug.c>

void test_pawn()
{
  // Left edge
  {
    uint64_t center = mkPosition(0,1);
    gamestate g = zerostate();
    g.pawns_bb = bit(center);
    g.current_piece_bb = g.pawns_bb;

    uint64_t expected = bit(mkPosition(0,2)) | bit(mkPosition(0,3));
    uint64_t actual = valid_pawn_moves(g, center);
    assert_equal_bb("test_pawn_1", expected, actual);

  }
  // Right edge
  {
    uint64_t center = mkPosition(7,1);
    gamestate g = zerostate();
    g.pawns_bb = bit(center);
    g.current_piece_bb = g.pawns_bb;

    uint64_t expected = bit(mkPosition(7,2)) | bit(mkPosition(7,3));
    uint64_t actual = valid_pawn_moves(g, center);
    assert_equal_bb("test_pawn_2", expected, actual);
  }
  // Middle
  {
    uint64_t center = mkPosition(3,1);
    gamestate g = zerostate();
    g.pawns_bb = bit(center);
    g.current_piece_bb = g.pawns_bb;

    uint64_t expected = bit(mkPosition(3,2)) | bit(mkPosition(3,3));
    uint64_t actual = valid_pawn_moves(g, center);
    assert_equal_bb("test_pawn_3", expected, actual);
  }
  // Capture
  {
    uint64_t center = mkPosition(7,1);
    gamestate g = zerostate();
    g.pawns_bb = bit(center);
    g.current_piece_bb = g.pawns_bb;
    g.pawns_bb |= bit(move_direction(center, DIRECTION_NORTHWEST));

    uint64_t expected = bit(mkPosition(7,2)) | bit(mkPosition(7,3)) | bit(mkPosition(6,2));
    uint64_t actual = valid_pawn_moves(g, center);
    assert_equal_bb("test_pawn_4", expected, actual);
  }
  // Blocked by another piece
  {
    uint64_t center = mkPosition(0,1);
    gamestate g = zerostate();
    g.pawns_bb = bit(center) | bit(center + RANK);
    g.current_piece_bb = g.pawns_bb;

    uint64_t expected = 0;
    uint64_t actual = valid_pawn_moves(g, center);
    assert_equal_bb("test_pawn_5", expected, actual);
  }
  // Blocked by another piece when double-jumping
  {
    uint64_t center = mkPosition(0,1);
    gamestate g = zerostate();
    g.pawns_bb = bit(center);
    g.current_piece_bb = g.pawns_bb;
    g.pawns_bb |=  bit(mkPosition(0,3));

    uint64_t expected = bit(mkPosition(0,2));
    uint64_t actual = valid_pawn_moves(g, center);
    assert_equal_bb("test_pawn_6", expected, actual);
  }
  // En Passant
  {
    gamestate g = zerostate();
    g.pawns_bb =
      bit(mkPosition(0,1)) |
      bit(mkPosition(1,3));
    g.current_piece_bb = bit(mkPosition(0,1));

    iterator i = mkIterator(g);
    {
      uint64_t expected =
        bit(mkPosition(0,2)) |
        bit(mkPosition(0,3));
      assert_equal_bb("test_pawn_en_passant_1", expected, i.current_piece_bb);
    }
    // Double jump allows en passant
    move m; m.from = mkPosition(0,1); m.to = mkPosition(0,3);
    g = apply_move(g, m);
    i = mkIterator(g);
    {
      uint64_t expected =
        bit(mkPosition(0,5)) |
        bit(mkPosition(1,5));
      assert_equal_int("test_pawn_en_passant_target", mkPosition(0,5), g.en_passant_sq);
      assert_equal_bb("test_pawn_en_passant_2", expected, i.current_piece_bb);
    }
    // The actual capture
    move m2; m2.from = mkPosition(1,3); m2.to = mkPosition(0,2);
    g = apply_move(g, swap_move(m2));
    {
      assert_equal_int("test_pawn_en_passant_target_2", POSITION_INVALID, g.en_passant_sq);
      assert_equal_bb("test_pawn_en_passant_3", bit(mkPosition(0,2)), g.pawns_bb);
    }
  }
}

int main()
{
  __builtin_set_test_result(1);
  __builtin_set_max_simulator_memory(200000000);
  __builtin_set_max_iterations(1000000000);
  test_pawn();
  return 1;
}
