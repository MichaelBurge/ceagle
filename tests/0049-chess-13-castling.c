#lang ceagle

#include <ceagle/test-support/chess-engine.c>
#include <ceagle/test-support/test-lib.c>
#include <ceagle/test-support/chess-debug.c>

void test_castling()
{
  // Kingside castle 1
  {
    gamestate g = zerostate();
    g.kings_bb = bit(mkPosition(4,0));
    g.rooks_bb = bit(mkPosition(7,0));
    g.castle_flags = CASTLE_WHITE_KINGSIDE;
    g.current_player_bb = all_pieces(g);

    uint64_t expected =
      bit(mkPosition(3,0)) |
      bit(mkPosition(3,1)) |
      bit(mkPosition(4,1)) |
      bit(mkPosition(5,1)) |
      bit(mkPosition(5,0)) |
      bit(mkPosition(6,0));
    uint64_t actual = valid_king_moves(g, mkPosition(4,0));
    assert_equal_bb("test_castling_1_moves", expected, actual);
    move m; m.from = mkPosition(4,0); m.to = CASTLE_KINGSIDE_KPOS;
    gamestate g2 = swap_board(apply_move(g, m));
    assert_equal_bb("test_castling_1_king", bit(CASTLE_KINGSIDE_KPOS), g2.kings_bb);
    assert_equal_bb("test_castling_1_rook", bit(CASTLE_KINGSIDE_RPOS), g2.rooks_bb);
    assert_equal_u64("test_castling_1_flags", 0, g2.castle_flags);
  }
  // Queenside castle 1
  {
    gamestate g = zerostate();
    g.kings_bb = bit(mkPosition(4,0));
    g.rooks_bb = bit(mkPosition(0,0));
    g.castle_flags = CASTLE_WHITE_QUEENSIDE;
    g.current_player_bb = all_pieces(g);

    uint64_t expected =
      bit(mkPosition(3,0)) |
      bit(mkPosition(3,1)) |
      bit(mkPosition(4,1)) |
      bit(mkPosition(5,1)) |
      bit(mkPosition(5,0)) |
      bit(mkPosition(2,0));
    uint64_t actual = valid_king_moves(g, mkPosition(4,0));
    assert_equal_bb("test_castling_2_moves", expected, actual);
    move m; m.from = mkPosition(4,0); m.to = mkPosition(2,0);
    gamestate g2 = swap_board(apply_move(g, m));
    assert_equal_bb("test_castling_2_king", bit(mkPosition(2,0)), g2.kings_bb);
    assert_equal_bb("test_castling_2_rook", bit(mkPosition(3,0)), g2.rooks_bb);
    assert_equal_u64("test_castling_2_flags", 0, g2.castle_flags);
  }
  // Kingside castle blocked by check 1
  {
    gamestate g = zerostate();
    g.kings_bb = bit(mkPosition(4,0));
    g.rooks_bb = bit(mkPosition(7,0));
    g.castle_flags = CASTLE_WHITE_KINGSIDE;
    g.current_player_bb = all_pieces(g);
    g.rooks_bb |= bit(mkPosition(5,7));

    uint64_t expected =
      bit(mkPosition(3,0)) |
      bit(mkPosition(3,1)) |
      bit(mkPosition(4,1));
    uint64_t actual = piece_legal_movepoints(g, mkPosition(4,0));
    move not_castle; not_castle.from = mkPosition(4,0); not_castle.to = mkPosition(4,1);
    move castle_kingside; castle_kingside.from = mkPosition(4,0); castle_kingside.to = mkPosition(6,0);
    assert("test_castling_3_not_kingside_1", ! is_kingside_castle(g, not_castle));
    assert("test_castling_3_not_kingside_2", ! is_queenside_castle(g, not_castle));
    assert("test_castling_3_not_kingside_3", ! results_in_check(g, not_castle));
    assert("test_castling_3_not_kingside_4", is_legal(g, not_castle));
    assert("test_castling_3_illegal", ! is_legal(g, castle_kingside));
    assert_equal_bb("test_castling_3_moves", expected, actual);
  }
  // Queenside castle blocked by check 1
  {
    gamestate g = zerostate();
    g.kings_bb = bit(mkPosition(4,0));
    g.rooks_bb = bit(mkPosition(0,0));
    g.castle_flags = CASTLE_WHITE_QUEENSIDE;
    g.current_player_bb = all_pieces(g);
    g.rooks_bb |= bit(mkPosition(3,7));

    uint64_t expected =
      bit(mkPosition(4,1)) |
      bit(mkPosition(5,1)) |
      bit(mkPosition(5,0));
    uint64_t actual = piece_legal_movepoints(g, mkPosition(4,0));
    assert_equal_bb("test_castling_4_moves", expected, actual);
  }
  // Kingside castle blocked by check 2
  {
    gamestate g = zerostate();
    g.kings_bb = bit(mkPosition(4,0));
    g.rooks_bb = bit(mkPosition(7,0));
    g.castle_flags = CASTLE_WHITE_KINGSIDE;
    g.current_player_bb = all_pieces(g);
    g.rooks_bb |= bit(mkPosition(4,7));

    uint64_t expected =
      bit(mkPosition(3,0)) |
      bit(mkPosition(3,1)) |
      bit(mkPosition(5,1)) |
      bit(mkPosition(5,0));
    uint64_t actual = piece_legal_movepoints(g, mkPosition(4,0));
    assert_equal_bb("test_castling_5_moves", expected, actual);
  }
  // Queenside castle blocked by check 2
  {
    gamestate g = zerostate();
    g.kings_bb = bit(mkPosition(4,0));
    g.rooks_bb = bit(mkPosition(0,0));
    g.castle_flags = CASTLE_WHITE_QUEENSIDE;
    g.current_player_bb = all_pieces(g);
    g.rooks_bb |= bit(mkPosition(4,7));

    uint64_t expected =
      bit(mkPosition(3,0)) |
      bit(mkPosition(3,1)) |
      bit(mkPosition(5,1)) |
      bit(mkPosition(5,0));
    uint64_t actual = piece_legal_movepoints(g, mkPosition(4,0));
    assert_equal_bb("test_castling_6_moves", expected, actual);
  }
  // Kingside castle blocked by unit
  {
    gamestate g = zerostate();
    g.kings_bb = bit(mkPosition(4,0));
    g.rooks_bb = bit(mkPosition(7,0));
    g.bishops_bb |= bit(mkPosition(5,0));
    g.castle_flags = CASTLE_WHITE_KINGSIDE;
    g.current_player_bb = all_pieces(g);

    uint64_t expected =
      bit(mkPosition(3,0)) |
      bit(mkPosition(3,1)) |
      bit(mkPosition(4,1)) |
      bit(mkPosition(5,1));
    uint64_t actual = valid_king_moves(g, mkPosition(4,0));
    assert_equal_bb("test_castling_7_moves", expected, actual);
  }
  // Queenside castle blocked by unit
  {
    gamestate g = zerostate();
    g.kings_bb = bit(mkPosition(4,0));
    g.rooks_bb = bit(mkPosition(0,0));
    g.bishops_bb = bit(mkPosition(3,0));
    g.castle_flags = CASTLE_WHITE_QUEENSIDE;
    g.current_player_bb = all_pieces(g);

    uint64_t expected =
      bit(mkPosition(3,1)) |
      bit(mkPosition(4,1)) |
      bit(mkPosition(5,1)) |
      bit(mkPosition(5,0));
    uint64_t actual = valid_king_moves(g, mkPosition(4,0));
    assert_equal_bb("test_castling_8_moves", expected, actual);
  }
  // Castle flags swap
  {
    {
      gamestate g = zerostate();
      g.castle_flags = CASTLE_WHITE_KINGSIDE;
      g = swap_board(g);
      assert_equal_u64("test_castling_9_flag_wk", CASTLE_BLACK_KINGSIDE, g.castle_flags);
    }
    {
      gamestate g = zerostate();
      g.castle_flags = CASTLE_WHITE_QUEENSIDE;
      g = swap_board(g);
      assert_equal_u64("test_castling_9_flag_wk", CASTLE_BLACK_QUEENSIDE, g.castle_flags);
    }
    {
      gamestate g = zerostate();
      g.castle_flags = CASTLE_BLACK_KINGSIDE;
      g = swap_board(g);
      assert_equal_u64("test_castling_9_flag_wk", CASTLE_WHITE_KINGSIDE, g.castle_flags);
    }
    {
      gamestate g = zerostate();
      g.castle_flags = CASTLE_BLACK_QUEENSIDE;
      g = swap_board(g);
      assert_equal_u64("test_castling_9_flag_wk", CASTLE_WHITE_QUEENSIDE, g.castle_flags);
    }
  }
  // Moving kingside rook deletes flag
  {
    gamestate g = zerostate();
    g.rooks_bb = bit(mkPosition(7,0));
    g.kings_bb = bit(mkPosition(4,0));
    g.castle_flags = CASTLE_WHITE_KINGSIDE;
    move m; m.from = mkPosition(7,0); m.to = mkPosition(6,0);
    gamestate g2 = apply_move(g, m);
    assert_equal_u64("test_castling_10_flag_kingrook_delete", 0, g2.castle_flags);
  }
  // Moving queenside rook deletes flag
  {
    gamestate g = zerostate();
    g.rooks_bb = bit(mkPosition(0,0));
    g.kings_bb = bit(mkPosition(4,0));
    g.castle_flags = CASTLE_WHITE_QUEENSIDE;
    move m; m.from = mkPosition(0,0); m.to = mkPosition(2,0);
    gamestate g2 = apply_move(g, m);
    assert_equal_u64("test_castling_11_flag_queenrook_delete", 0, g2.castle_flags);
  }
  // Moving king deletes both flags
  {
    gamestate g = zerostate();
    g.kings_bb = bit(mkPosition(4,0));
    g.rooks_bb =
      bit(mkPosition(7,0)) |
      bit(mkPosition(0,0));
    g.castle_flags = CASTLE_WHITE_KINGSIDE | CASTLE_WHITE_QUEENSIDE;
    move m; m.from = mkPosition(4,0); m.to = mkPosition(5,0);
    gamestate g2 = apply_move(g, m);
    assert_equal_u64("test_castling_12_flag_king_delete", 0, g2.castle_flags);
  }
  // Can't castle with unit blocking rook queenside
  {
    gamestate g = zerostate();
    g.kings_bb = bit(mkPosition(4,0));
    g.rooks_bb = bit(mkPosition(0,0));
    g.castle_flags = CASTLE_WHITE_QUEENSIDE;
    g.knights_bb = bit(mkPosition(1,0));
    g.current_piece_bb = all_pieces(g);

    uint64_t expected =
      bit(mkPosition(3,0)) |
      bit(mkPosition(3,1)) |
      bit(mkPosition(4,1)) |
      bit(mkPosition(5,1)) |
      bit(mkPosition(5,0));
    uint64_t actual = piece_legal_movepoints(g, mkPosition(4,0));
    assert_equal_bb("test_castling_13_knight_blocks_rook_queenside", expected, actual);
  }
  // Eating a rook cancels its flag
  {
    gamestate g = zerostate();
    g.rooks_bb =
      bit(mkPosition(7,0)) |
      bit(mkPosition(0,0));
    g.current_piece_bb = g.rooks_bb;
    g.rooks_bb |=
      bit(mkPosition(7,7)) |
      bit(mkPosition(0,7));
    g.castle_flags =
      CASTLE_BLACK_QUEENSIDE |
      CASTLE_BLACK_KINGSIDE;
    // West Rook
    {
      move m; m.from = mkPosition(7,0); m.to = mkPosition(7,7);
      gamestate g2 = swap_board(apply_move(g, m));
      assert_equal_u64("test_castling_13_remove_flag_on_eat_rook_1", CASTLE_BLACK_QUEENSIDE, g2.castle_flags);
    }
    // East Rook
    {
      move m; m.from = mkPosition(0,0); m.to = mkPosition(0,7);
      gamestate g2 = swap_board(apply_move(g, m));
      assert_equal_u64("test_castling_13_remove_flag_on_eat_rook_2", CASTLE_BLACK_KINGSIDE, g2.castle_flags);
    }
  }
}

int main()
{
  __builtin_set_test_result(1);
  __builtin_set_max_simulator_memory(20000000000);
  __builtin_set_max_iterations(1000000000000);
  test_castling();
  return 1;
}
