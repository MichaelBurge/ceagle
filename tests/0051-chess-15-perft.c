#lang ceagle

#include <ceagle/test-support/chess-engine.c>
#include <ceagle/test-support/test-lib.c>
#include <ceagle/test-support/chess-debug.c>

uint64_t perft_from(gamestate g, int depth)
{
  return perft(g, depth);
}

gamestate castling_position()
{
  gamestate g;
  g.rooks_bb = 9295429630892703873;
  g.knights_bb = 37452115083264;
  g.bishops_bb = 18015498021115904;
  g.queens_bb = 4503599629467648;
  g.kings_bb = 1152921504606846992;
  g.pawns_bb = 12754369552377600;
  g.current_player_bb = 103350075281;
  g.en_passant_sq = 255;
  g.castle_flags = 15;
  g.is_white = 1;
  return g;
}

gamestate promotion_position()
{
  gamestate g;
  g.rooks_bb = 0;
  g.knights_bb = 11529215046068469765;
  g.bishops_bb = 0;
  g.queens_bb = 0;
  g.kings_bb = 4503599627372544;
  g.pawns_bb = 63050394783188736;
  g.current_player_bb = 63050394783188997;
  g.en_passant_sq = 255;
  g.castle_flags = 0;
  g.is_white = 0;
  return g;
}

void test_perft()
{
  // Initial position
  {
    gamestate g = new_game();
    /* printf("perft(1): %lu\n", perft(g,1)); */
    /* printf("perft(2): %lu\n", perft(g,2)); */
    /* printf("perft(3): %lu\n", perft(g,3)); */
    /* printf("perft(4): %lu\n", perft(g,4)); */
    /* printf("perft(5): %lu\n", perft(g,5)); */
    assert_equal_u64("Perft(1)", 20, perft(g, 1));
    /* assert_equal_u64("Perft(2)", 400, perft(g,2)); */
    /* assert_equal_u64("Perft(3)", 8902, perft(g,3)); */
    /* assert_equal_u64("Perft(4)", 197281, perft(g,4)); */
    /* assert_equal_u64("Perft(5)", 4865609, perft(g,5)); */
  }
  // Initial position(read from string)
  {
    //gamestate g = parse_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
    /* assert_equal_u64("Perft(1)", 20,      perft_from(g,1)); */
    /* assert_equal_u64("Perft(2)", 400,     perft_from(g,2)); */
    /* assert_equal_u64("Perft(3)", 8902,    perft_from(g,3)); */
    /* assert_equal_u64("Perft(4)", 197281,  perft_from(g,4)); */
    /* assert_equal_u64("Perft(5)", 4865609, perft_from(g,5)); */
    /* assert_equal_u64("Perft(6)", 119060324, perft_from(g,6)); */
    /* assert_equal_u64("Perft(7)", 3195901860, perft_from(g,7)); */

  }
  // Castling position
  {
    //gamestate g = parse_fen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1");
    gamestate g = castling_position();
    // 1: e5f7 - (88799, 88825)
    /* move m; m.from = mkPosition(4,4); m.to = mkPosition(5,6); */
    /* g = swap_board(apply_move(g, m)); */
    /* // 2: a1b1 - (2126,2127) */
    /* move m2; m2.from = mkPosition(0,0); m2.to = mkPosition(1,0); */
    /* g = swap_board(apply_move(g, m2)); */
    /* // 3: f7h8 - (40, 41) */
    /* move m3; m3.from = mkPosition(5,6); m3.to = mkPosition(7,7); */
    /* g = swap_board(apply_move(g, m3)); */
    /* perft_divide(g, 1); */
    /* /\* print_gamestate(g); *\/ */
    /* print_fen(g); */
    /* assert_equal_u64("castling_perft_1", 48,          perft_from(g,1)); */
    /* assert_equal_u64("castling_perft_2", 2039,        perft_from(g,2)); */
    /* assert_equal_u64("castling_perft_3", 97862,       perft_from(g,3)); */
    /* assert_equal_u64("castling_perft_4", 4085603,     perft_from(g,4)); */
    /* assert_equal_u64("castling_perft_5", 193690690,   perft_from(g,5)); */
    /* assert_equal_u64("good_perft_6", 8031647685,  perft_from(g,6)); */
  }
  // Promotion position
  {
    //gamestate g = parse_fen("n1n5/PPPk4/8/8/8/8/4Kppp/5N1N b - - 0 1");
    gamestate g = promotion_position();
    /* assert_equal_u64("promotion_perft_1", 24,          perft_from(g,1)); */
    /* assert_equal_u64("promotion_perft_2", 496,         perft_from(g,2)); */
    /* assert_equal_u64("promotion_perft_3", 9483,        perft_from(g,3)); */
    /* assert_equal_u64("promotion_perft_4", 182838,      perft_from(g,4)); */
    /* assert_equal_u64("promotion_perft_5", 3605103,     perft_from(g,5)); */
    /* assert_equal_u64("promotion_perft_6", 71179139,     perft_from(g,6)); */
  }
}

int main()
{
  __builtin_set_test_result(1);
  __builtin_set_max_simulator_memory(20000000000);
  __builtin_set_max_iterations(1000000000000);
  test_perft();
  return 1;
}
