#lang ceagle

#include <ceagle/test-support/chess-engine.c>
#include <ceagle/test-support/test-lib.c>
#include <ceagle/test-support/chess-debug.c>

static gamestate parse_fen2(const char* s)
{
  const char *orig_s = s;
  // Pieces
  gamestate g = zerostate();
  for (int r = 8; r --> 0;) {
    for (int f = 0;; f++) {
      int pos = mkPosition(f,r);
      char c = *s++;
      switch (c) {
      case 'R': g.current_piece_bb |= bit(pos);
      case 'r': g.rooks_bb |= bit(pos); break;
      case 'N': g.current_piece_bb |= bit(pos);
      case 'n': g.knights_bb |= bit(pos); break;
      case 'B': g.current_piece_bb |= bit(pos);
      case 'b': g.bishops_bb |= bit(pos); break;
      case 'Q': g.current_piece_bb |= bit(pos);
      case 'q': g.queens_bb |= bit(pos); break;
      case 'K': g.current_piece_bb |= bit(pos);
      case 'k': g.kings_bb |= bit(pos); break;
      case 'P': g.current_piece_bb |= bit(pos);
      case 'p': g.pawns_bb |= bit(pos); break;
      case '8': f++;
      case '7': f++;
      case '6': f++;
      case '5': f++;
      case '4': f++;
      case '3': f++;
      case '2': f++;
      case '1': break;
      case '/': goto end_row;
      case ' ': goto end_loop;
      default: __builtin_print_string("case 1"); __builtin_trap(); goto end_loop;
      }
    }
  end_row:;
  }
 end_loop:
  // Color
  switch (*s++) {
  case 'w': g.is_white = true; break;
  case 'b': g.is_white = false; break;
  default: __builtin_print_string("case 2"); __builtin_trap();
  }
  s++;
  // Castle flags
  while (*s != ' ') {
    switch (*s++) {
    case 'K': g.castle_flags |= CASTLE_WHITE_KINGSIDE; break;
    case 'Q': g.castle_flags |= CASTLE_WHITE_QUEENSIDE; break;
    case 'k': g.castle_flags |= CASTLE_BLACK_KINGSIDE; break;
    case 'q': g.castle_flags |= CASTLE_BLACK_QUEENSIDE; break;
    case '-': break;
    default: __builtin_print_string("case 3"); __builtin_trap();
    }
  }
  if (!g.is_white) {
    g = swap_board(g);
  }
  return g;
}

int main()
{
  __builtin_set_test_result(1);
  __builtin_set_max_simulator_memory(2000000000);
  __builtin_set_max_iterations(100000000000);
  gamestate g = parse_fen2("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
  return 1;
}
