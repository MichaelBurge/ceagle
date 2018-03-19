#lang ceagle

typedef unsigned int uint64_t;
typedef unsigned int bool;

typedef struct gamestate {
  uint64_t rooks_bb;
  uint64_t knights_bb;
  uint64_t bishops_bb;
  uint64_t queens_bb;
  uint64_t kings_bb;
  uint64_t pawns_bb;
  union {
    uint64_t current_player_bb;
    uint64_t current_piece_bb;
  };
  int en_passant_sq;
  union {
    uint64_t castle_flags;
    uint64_t promotion_piece;
  };
  bool is_white;
} gamestate;

int main() {
  __builtin_set_test_result(100);

  gamestate g;
  g.current_player_bb = 100;
  return g.current_piece_bb;
  /* g.x.current_player_bb = 100; */
  /* return g.x.current_piece_bb; */
}
