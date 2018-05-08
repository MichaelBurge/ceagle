#lang ceagle

typedef unsigned __bits 64 uint64_t;

typedef struct gamestate {
  uint64_t rooks_bb;
  uint64_t knights_bb;
  uint64_t bishops_bb;
  uint64_t queens_bb;
  uint64_t pawns_bb;
} gamestate;

static gamestate zerostate()
{
  gamestate x;
  x.rooks_bb = 0;
  x.knights_bb = 0;
  x.bishops_bb = 0;
  x.queens_bb = 0;
  x.pawns_bb = 0;
  return x;
}

static uint64_t all_pieces(gamestate x)
{
  return x.pawns_bb;
}

int main() {
  __builtin_set_test_result(5);
  gamestate g = zerostate();
  g.pawns_bb = 5;
  uint64_t pieces = all_pieces(g);
  return pieces;
}
