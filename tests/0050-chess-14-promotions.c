#lang ceagle

#include <ceagle/test-support/chess-engine.c>
#include <ceagle/test-support/test-lib.c>
#include <ceagle/test-support/chess-debug.c>

void test_promotions()
{
  gamestate g = zerostate();
  g.pawns_bb = bit(mkPosition(0,6));
  g.current_player_bb = all_pieces(g);
  // Promotion to Rook
  {
    move m = mkPromotion(mkPosition(0,6), PIECE_ROOK);
    gamestate g2 = swap_board(apply_move(g, m));
    assert_equal_bb("test_promotions_1_pawns", 0, g2.pawns_bb);
    assert_equal_bb("test_promotions_1_rooks", bit(mkPosition(0,7)), g2.rooks_bb);
  }
  // Promotion to Knight
  {
    move m = mkPromotion(mkPosition(0,6), PIECE_KNIGHT);
    gamestate g2 = swap_board(apply_move(g, m));
    assert_equal_bb("test_promotions_2_pawns", 0, g2.pawns_bb);
    assert_equal_bb("test_promotions_2_knights", bit(mkPosition(0,7)), g2.knights_bb);

  }
  // Promotion to Bishop
  {
    move m = mkPromotion(mkPosition(0,6), PIECE_BISHOP);
    gamestate g2 = swap_board(apply_move(g, m));
    assert_equal_bb("test_promotions_3_pawns", 0, g2.pawns_bb);
    assert_equal_bb("test_promotions_3_bishops", bit(mkPosition(0,7)), g2.bishops_bb);

  }
  // Promotion to Queen
  {
    move m = mkPromotion(mkPosition(0,6), PIECE_QUEEN);
    gamestate g2 = swap_board(apply_move(g, m));
    assert_equal_bb("test_promotions_4_pawns", 0, g2.pawns_bb);
    assert_equal_bb("test_promotions_4_queens", bit(mkPosition(0,7)), g2.queens_bb);
  }
  // Iterator generates promotions
  {
    assert_equal_int("test_promotions_5_num_moves", 4, num_legal_moves(g));
    iterator i = mkLegalIterator(g);
    move m1 = dereference_iterator(i); i = advance_iterator_legal(g,i);
    move m2 = dereference_iterator(i); i = advance_iterator_legal(g,i);
    move m3 = dereference_iterator(i); i = advance_iterator_legal(g,i);
    move m4 = dereference_iterator(i); i = advance_iterator_legal(g,i);
    assert_equal_int("test_promotions_5_m1_pc", PIECE_QUEEN, promotion_piece(m1));
    assert_equal_int("test_promotions_5_m2_pc", PIECE_BISHOP, promotion_piece(m2));
    assert_equal_int("test_promotions_5_m3_pc", PIECE_KNIGHT, promotion_piece(m3));
    assert_equal_int("test_promotions_5_m4_pc", PIECE_ROOK, promotion_piece(m4));
  }
  // Iterator generates promotions 2
  {
    gamestate g = zerostate();
    g.pawns_bb = bit(mkPosition(1,6));
    g.current_player_bb = all_pieces(g);
    g.bishops_bb =
      bit(mkPosition(0,7)) |
      bit(mkPosition(2,7));

    iterator i = mkLegalIterator(g);
    move m1 = dereference_iterator(i); i = advance_iterator_legal(g,i);
    move m2 = dereference_iterator(i); i = advance_iterator_legal(g,i);
    move m3 = dereference_iterator(i); i = advance_iterator_legal(g,i);
    move m4 = dereference_iterator(i); i = advance_iterator_legal(g,i);
    assert_equal_int("test_promotions_6_num_moves", 12, num_legal_moves(g));
  }
}

int main()
{
  __builtin_set_test_result(1);
  __builtin_set_max_simulator_memory(2000000000);
  __builtin_set_max_iterations(100000000000);
  test_promotions();
  return 1;
}
