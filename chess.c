typedef unsigned int uint64_t;

// bb is 'bitboard'
typedef struct gamestate {
  uint64_t rooks_bb;
  uint64_t knights_bb;
  uint64_t bishops_bb;
  uint64_t queens_bb;
  uint64_t kings_bb;
  uint64_t pawns_bb;
  union {
    uint64_t current_player_bb;
    uint64_t current_piece_bb; // For iterators
  };
  int en_passant_sq;
  union {
    uint64_t castle_flags;
    uint64_t promotion_piece; // For iterators
  };
  bool is_white;
} gamestate;

struct move {
  int from;
  int to;
};

typedef gamestate iterator;
typedef int piece;
typedef int position;

const piece PIECE_EMPTY  = 0;
const piece PIECE_ROOK   = 1;
const piece PIECE_KNIGHT = 2;
const piece PIECE_BISHOP = 3;
const piece PIECE_QUEEN  = 4;
const piece PIECE_KING   = 5;
const piece PIECE_PAWN   = 6;

const int VALUE_PAWN   = 100;
const int VALUE_KNIGHT = 300;
const int VALUE_BISHOP = 300;
const int VALUE_ROOK   = 500;
const int VALUE_QUEEN  = 900;
const int VALUE_AVAILABLE_MOVE = 5; // Points for each move available
const int VALUE_CHECKMATE = -1000000; 
const int VALUE_NEGAMAX_START = 0x80000000;
const int VALUE_CENTER = 10; // Points for holding the center
const int VALUE_ATTACK = 20; // Points for being able to attack enemy pieces.

const int DIRECTION_EAST  = 0;
const int DIRECTION_WEST  = 1;
const int DIRECTION_NORTH = 2;
const int DIRECTION_SOUTH = 3;
const int DIRECTION_NORTHEAST = 4;
const int DIRECTION_NORTHWEST = 5;
const int DIRECTION_SOUTHEAST = 6;
const int DIRECTION_SOUTHWEST = 7;

// CASTLE FLAGS
const int CASTLE_WHITE_KINGSIDE = 0x1;
const int CASTLE_WHITE_QUEENSIDE = 0x2;
const int CASTLE_BLACK_KINGSIDE = 0x4;
const int CASTLE_BLACK_QUEENSIDE = 0x8;

const int CASTLE_KINGSIDE_KPOS = 6;
const int CASTLE_QUEENSIDE_KPOS = 2;
const int CASTLE_KINGSIDE_RPOS = 5;
const int CASTLE_QUEENSIDE_RPOS = 3;

const int NUM_BOARD_SQUARES = 8*8;

const int RANK = 8;

const int POSITION_INVALID = 255;

const uint64_t RAY_A1A8 = 0x0101010101010101;
const uint64_t RAY_H1H8 = RAY_A1A8 << (RANK-1);

// Static functions

static uint64_t bit(uint64_t idx) {
  if (idx >= 64 || idx < 0) {
    return 0;
  }
  return ((uint64_t)(1) << idx);
}
static uint64_t clear_bit(uint64_t x, uint64_t idx) { return x & ~bit(idx); }
static bool is_bit_set(uint64_t x, uint64_t idx) {
  if (idx >= 64) { return false; }
  return x & bit(idx);
}
static uint64_t set_bit(uint64_t x, uint64_t idx) { return x | bit(idx); }
static uint64_t lsb_first_set(uint64_t x) { return __builtin_ctzll(x); }
static uint64_t msb_first_set(uint64_t x) { return (63 - __builtin_clzll(x)); }

static int rank(int x) { return x / 8; }
static int file(int x) { return x % 8; }
static uint64_t offset(uint64_t x, int os)
{
 if (os < 0) {
    return (x >> -os);
  } else {
    return (x << os);
  }
}

static gamestate zerostate()
{
  gamestate x;
  x.current_piece_bb = 0;
  x.rooks_bb = 0;
  x.knights_bb = 0;
  x.bishops_bb = 0;
  x.queens_bb = 0;
  x.kings_bb = 0;
  x.pawns_bb = 0;
  x.en_passant_sq = POSITION_INVALID;
  x.castle_flags = 0;
  x.is_white = true;
  return x;
}


static uint64_t mkPosition(int file, int rank)
{
  return rank * RANK + file;
}

const uint64_t RAY_A1H8 =
  bit(mkPosition(0,0)) |
  bit(mkPosition(1,1)) |
  bit(mkPosition(2,2)) |
  bit(mkPosition(3,3)) |
  bit(mkPosition(4,4)) |
  bit(mkPosition(5,5)) |
  bit(mkPosition(6,6)) |
  bit(mkPosition(7,7))
  ;

// https://chessprogramming.wikispaces.com/On+an+empty+Board#RayAttacks
static uint64_t diagonal_mask(int center)
{
   const uint64_t maindia = 0x8040201008040201;
   int diag =8*(center & 7) - (center & 56);
   int nort = -diag & ( diag >> 31);
   int sout =  diag & (-diag >> 31);
   return (maindia >> sout) << nort;
}

static uint64_t antidiagonal_mask(int center)
{
  const uint64_t maindia = 0x0102040810204080;
  int diag =56- 8*(center&7) - (center&56);
  int nort = -diag & ( diag >> 31);
  int sout =  diag & (-diag >> 31);
  return (maindia >> sout) << nort;
}

// https://chessprogramming.wikispaces.com/Flipping+Mirroring+and+Rotating
// See 'flipDiagA1H8'
static uint64_t flipDiagA1H8(uint64_t x)
{
  uint64_t t;
  const uint64_t k1 = 0x5500550055005500;
  const uint64_t k2 = 0x3333000033330000;
  const uint64_t k4 = 0x0f0f0f0f00000000;
  t  = k4 & (x ^ (x << 28));
  x ^=       t ^ (t >> 28) ;
  t  = k2 & (x ^ (x << 14));
  x ^=       t ^ (t >> 14) ;
  t  = k1 & (x ^ (x <<  7));
  x ^=       t ^ (t >>  7) ;
  return x;
}

static uint64_t mirror_horizontal (uint64_t x) {
   const uint64_t k1 = 0x5555555555555555;
   const uint64_t k2 = 0x3333333333333333;
   const uint64_t k4 = 0x0f0f0f0f0f0f0f0f;
   x = ((x >> 1) & k1) +  2*(x & k1);
   x = ((x >> 2) & k2) +  4*(x & k2);
   x = ((x >> 4) & k4) + 16*(x & k4);
   return x;
}

static uint64_t flip_vertical(uint64_t x)
{
  return
    ( (x << 56)                        ) |
    ( (x << 40) & (0x00ff000000000000) ) |
    ( (x << 24) & (0x0000ff0000000000) ) |
    ( (x <<  8) & (0x000000ff00000000) ) |
    ( (x >>  8) & (0x00000000ff000000) ) |
    ( (x >> 24) & (0x0000000000ff0000) ) |
    ( (x >> 40) & (0x000000000000ff00) ) |
    ( (x >> 56)                        );
}

// We use a monochrome board, so white is always the one to move, and we flip the board after each move.
static uint64_t flip_bb(uint64_t x)
{
  return __builtin_bswap64(x);
  //return mirror_horizontal(__builtin_bswap64(x));
}

static uint64_t rotate_bb(uint64_t x)
{
  return flip_vertical(flipDiagA1H8(x));
}

const uint64_t RAY_A8H1 = rotate_bb(RAY_A1H8);

static uint64_t get_piece_bb(gamestate x, int piece)
{
  switch (piece) {
  case PIECE_ROOK:
    return x.rooks_bb;
  case PIECE_KNIGHT:
    return x.knights_bb;
  case PIECE_BISHOP:
    return x.bishops_bb;
  case PIECE_QUEEN:
    return x.queens_bb;
  case PIECE_KING:
    return x.kings_bb;
  case PIECE_PAWN:
    return x.pawns_bb;
  }
  __builtin_trap();
}

static gamestate set_piece_bb(gamestate x, int piece, uint64_t bb)
{
  switch (piece) {
  case PIECE_ROOK:
    x.rooks_bb = bb;
    return x;
  case PIECE_KNIGHT:
    x.knights_bb = bb;
    return x;
  case PIECE_BISHOP:
    x.bishops_bb = bb;
    return x;
  case PIECE_QUEEN:
    x.queens_bb = bb;
    return x;
  case PIECE_KING:
    x.kings_bb = bb;
    return x;
  case PIECE_PAWN:
    x.pawns_bb = bb;
    return x;
  default:
    __builtin_trap();
  }
}

static int next_iterator_piece(iterator x)
{
  if (x.rooks_bb)
    return PIECE_ROOK;
  if (x.knights_bb)
    return PIECE_KNIGHT;
  if (x.bishops_bb)
    return PIECE_BISHOP;
  if (x.queens_bb)
    return PIECE_QUEEN;
  if (x.kings_bb)
    return PIECE_KING;
  if (x.pawns_bb)
    return PIECE_PAWN;
  return PIECE_EMPTY;
}

static int iterator_position(iterator x)
{
  int piece = next_iterator_piece(x);
  if (piece == PIECE_EMPTY) {
    return POSITION_INVALID;
  } 
  uint64_t piece_bb = get_piece_bb(x, piece);
  int idx = lsb_first_set(piece_bb);
  return idx;
}

static int promotion_piece(move m)
{
  return (m.to >> 6);
}

static move dereference_iterator(iterator i)
{
  move m;
  m.from = iterator_position(i);
  m.to = lsb_first_set(i.current_piece_bb);
  m.to |= (i.promotion_piece << 6);
  return m;
}

static uint64_t advance_bb_iterator(uint64_t x)
{
  return x & (x-1);
}

static bool is_iterator_finished(iterator x)
{
  return
    ! x.rooks_bb &&
    ! x.knights_bb &&
    ! x.bishops_bb &&
    ! x.queens_bb &&
    ! x.kings_bb &&
    ! x.pawns_bb &&
    ! x.current_piece_bb
    ;
}

// When a piece is moving west, it may wrap around to be east.
// So we mask out the eastmost column, since it's impossible anyways.
static uint64_t guard_west(uint64_t x) {
  return x & ~RAY_H1H8;
}

static uint64_t guard_east(uint64_t x) {
  return x & ~RAY_A1A8;
}

static int move_direction(int idx, int direction)
{
  if (idx < 0 || idx >= 64) {
    return POSITION_INVALID;
  }
  switch (direction) {
  case DIRECTION_EAST:
    if (idx % 8 == 7)
      return POSITION_INVALID;
    return idx + 1;
  case DIRECTION_WEST:
    if (idx % 8 == 0)
      return POSITION_INVALID;
    return idx - 1;
  case DIRECTION_NORTH:
    return idx + RANK;
  case DIRECTION_SOUTH:
    return idx - RANK;
  case DIRECTION_NORTHEAST:
    if (file(idx) == 7) {
      return POSITION_INVALID;
    }
    return (idx + RANK + 1);
  case DIRECTION_NORTHWEST:
    if (file(idx) == 0) {
      return POSITION_INVALID;
    }
    return (idx + RANK - 1);
  case DIRECTION_SOUTHEAST:
    if (file(idx) == 7) {
      return POSITION_INVALID;
    }
    return (idx - RANK + 1);
  case DIRECTION_SOUTHWEST:
    if (file(idx) == 0) {
      return POSITION_INVALID;
    }
    return (idx - RANK - 1);
  default:
    __builtin_trap();
  }
}

static uint64_t all_pieces(gamestate x)
{
  return
    x.rooks_bb |
    x.knights_bb |
    x.bishops_bb |
    x.queens_bb |
    x.kings_bb |
    x.pawns_bb
    ;
}

static uint64_t enemy_pieces(gamestate x)
{
  return all_pieces(x) ^ x.current_player_bb;
}

static uint64_t valid_pawn_moves(gamestate x, int center)
{
  uint64_t moves_bb = 0;
  // Non-captures
  if (! is_bit_set(all_pieces(x), center + RANK)) {
    uint64_t noncaptures_bb = bit(center + RANK);
    uint64_t pcs = all_pieces(x);
    if (rank(center) == 1 && ! is_bit_set(pcs, center + RANK) && ! is_bit_set(pcs, center + RANK*2)) {
      noncaptures_bb |= bit(center + RANK + RANK);
    }
    moves_bb |= noncaptures_bb;
  }
  // Captures
  {
    uint64_t captures_bb = 0;
    if (file(center) > 0) {
      captures_bb |= bit(move_direction(center + RANK, DIRECTION_WEST));
    }
    if (file(center) < 7) {
      captures_bb |= bit(move_direction(center + RANK, DIRECTION_EAST));
    }
    captures_bb &= all_pieces(x) ^ x.current_player_bb;
    if ((file(center) < 7 && x.en_passant_sq == center + RANK + 1) ||
        (file(center) > 0 && x.en_passant_sq == center + RANK - 1)) {
      captures_bb |= bit(x.en_passant_sq);
    }
    moves_bb |= captures_bb;
  }
  return moves_bb;
}

static uint64_t valid_knight_moves(gamestate x, int idx)
{
  uint64_t moves_bb = 0;
  moves_bb |= guard_west(bit(move_direction(idx + 2 * RANK, DIRECTION_WEST)));
  moves_bb |= guard_east(bit(move_direction(idx + 2 * RANK, DIRECTION_EAST)));
  
  moves_bb |= guard_west(bit(move_direction(idx - 2 * RANK, DIRECTION_WEST)));
  moves_bb |= guard_east(bit(move_direction(idx - 2 * RANK, DIRECTION_EAST)));
  
  moves_bb |= bit(move_direction(move_direction(move_direction(idx, DIRECTION_EAST), DIRECTION_EAST), DIRECTION_NORTH));
  moves_bb |= bit(move_direction(move_direction(move_direction(idx, DIRECTION_EAST), DIRECTION_EAST), DIRECTION_SOUTH));

  moves_bb |= bit(move_direction(move_direction(move_direction(idx, DIRECTION_WEST), DIRECTION_WEST), DIRECTION_NORTH));
  moves_bb |= bit(move_direction(move_direction(move_direction(idx, DIRECTION_WEST), DIRECTION_WEST), DIRECTION_SOUTH));

  moves_bb &= ~x.current_player_bb;
  return moves_bb;
}

static uint64_t bitrange(int start, int end)
{
  uint64_t end_bb = bit(end+1)-1;
  uint64_t start_bb = bit(start)-1;
  return end_bb ^ start_bb;
}

static uint64_t mkRay(int center, int direction)
{
  int rOs, fOs;
  switch (direction) {
  case DIRECTION_NORTHWEST: rOs =  1; fOs = -1; break;
  case DIRECTION_NORTH:     rOs =  1; fOs =  0; break;
  case DIRECTION_NORTHEAST: rOs =  1; fOs =  1; break;
  case DIRECTION_EAST:      rOs =  0; fOs =  1; break;
  case DIRECTION_SOUTHEAST: rOs = -1; fOs =  1; break;
  case DIRECTION_SOUTH:     rOs = -1; fOs =  0; break;
  case DIRECTION_SOUTHWEST: rOs = -1; fOs = -1; break;
  case DIRECTION_WEST:      rOs =  0; fOs = -1; break;
  default: __builtin_trap();
  }
  uint64_t ray;
  uint64_t next = bit(center);
  do {
    ray = next;
    next = offset(next, RANK * rOs);
    next = offset(next, fOs);
    switch (fOs) {
    case 1:  next &= ~RAY_A1A8; break;
    case -1: next &= ~RAY_H1H8; break;
    default: break;
    }
    next |= ray;
  } while (ray != next);
  ray = clear_bit(ray, center);
  return ray;
}

static int closest_blocker(uint64_t blockers_ray, int direction)
{
  if (blockers_ray == 0)
    return POSITION_INVALID;
  switch (direction) {
  case DIRECTION_NORTHWEST:
  case DIRECTION_NORTH:
  case DIRECTION_NORTHEAST:
  case DIRECTION_EAST:
    return lsb_first_set(blockers_ray);
  case DIRECTION_SOUTHEAST:
  case DIRECTION_SOUTH:
  case DIRECTION_SOUTHWEST:
  case DIRECTION_WEST:
    return msb_first_set(blockers_ray);
  default:
    __builtin_trap();
  }  
}

static uint64_t shoot_ray_until_blocker(gamestate state, int idx, int direction)
{
  uint64_t pieces = all_pieces(state);
  uint64_t base_ray = mkRay(idx, direction);
  uint64_t blockers = base_ray & pieces;
  int blocker = closest_blocker(blockers, direction);
  if (blocker == POSITION_INVALID) {
    return base_ray;
  } else {
    uint64_t blocker_ray = mkRay(blocker, direction);
    uint64_t movable_squares_without_capture = base_ray ^ blocker_ray;
    bool allow_capture = ! is_bit_set(state.current_player_bb, blocker);
    if (allow_capture)
      return movable_squares_without_capture | bit(blocker);
    else
      return movable_squares_without_capture & ~bit(blocker);
  }
}

static uint64_t valid_bishop_moves(gamestate x, int idx)
{
  return
    shoot_ray_until_blocker(x, idx, DIRECTION_NORTHEAST) |
    shoot_ray_until_blocker(x, idx, DIRECTION_NORTHWEST) |
    shoot_ray_until_blocker(x, idx, DIRECTION_SOUTHEAST) |
    shoot_ray_until_blocker(x, idx, DIRECTION_SOUTHWEST)
    ;
}

static uint64_t valid_rook_moves(gamestate x, int idx)
{
  return
    shoot_ray_until_blocker(x, idx, DIRECTION_NORTH) |
    shoot_ray_until_blocker(x, idx, DIRECTION_WEST) |
    shoot_ray_until_blocker(x, idx, DIRECTION_EAST) |
    shoot_ray_until_blocker(x, idx, DIRECTION_SOUTH)
    ;
}

static uint64_t valid_king_moves(gamestate g, int idx)
{
  uint64_t ret =
    bit(move_direction(idx, DIRECTION_EAST)) |
    bit(move_direction(idx, DIRECTION_WEST)) |
    bit(move_direction(idx, DIRECTION_NORTH)) |
    bit(move_direction(idx, DIRECTION_SOUTH)) |
    bit(move_direction(idx, DIRECTION_NORTHEAST)) |
    bit(move_direction(idx, DIRECTION_SOUTHEAST)) |
    bit(move_direction(idx, DIRECTION_NORTHWEST)) |
    bit(move_direction(idx, DIRECTION_SOUTHWEST))
    ;
  if ((g.castle_flags & CASTLE_WHITE_KINGSIDE)) {
    uint64_t pcs = all_pieces(g);
    if (is_bit_set(pcs, CASTLE_KINGSIDE_KPOS) ||
        is_bit_set(pcs, CASTLE_KINGSIDE_RPOS)) {
      goto skip_kingside;
    }
    ret |= bit(CASTLE_KINGSIDE_KPOS);
  }
 skip_kingside:
  if ((g.castle_flags & CASTLE_WHITE_QUEENSIDE)) {
    uint64_t pcs = all_pieces(g);
    if (is_bit_set(pcs, CASTLE_QUEENSIDE_RPOS) ||
        is_bit_set(pcs, CASTLE_QUEENSIDE_KPOS) ||
        is_bit_set(pcs, CASTLE_QUEENSIDE_KPOS-1)) {
      goto skip_queenside;
    }
    ret |= bit(CASTLE_QUEENSIDE_KPOS);
  }
 skip_queenside:
  ret &= ~ g.current_piece_bb;
  return ret;
}

static uint64_t valid_queen_moves(gamestate x, int idx)
{
  return
    valid_bishop_moves(x, idx) |
    valid_rook_moves(x, idx)
    ;
}

static int get_piece(gamestate x, int idx)
{
  if (is_bit_set(x.rooks_bb, idx))
    return PIECE_ROOK;
  if (is_bit_set(x.knights_bb, idx))
    return PIECE_KNIGHT;
  if (is_bit_set(x.bishops_bb, idx))
    return PIECE_BISHOP;
  if (is_bit_set(x.queens_bb, idx))
    return PIECE_QUEEN;
  if (is_bit_set(x.kings_bb, idx))
    return PIECE_KING;
  if (is_bit_set(x.pawns_bb, idx))
    return PIECE_PAWN;
  return PIECE_EMPTY;
}

static uint64_t valid_piece_moves(gamestate x, int idx)
{
  int piece = get_piece(x, idx);
  switch (piece) {
  case PIECE_ROOK:
    return valid_rook_moves(x, idx);
  case PIECE_KNIGHT:
    return valid_knight_moves(x, idx);
  case PIECE_BISHOP:
    return valid_bishop_moves(x, idx);
  case PIECE_QUEEN:
    return valid_queen_moves(x, idx);
  case PIECE_KING:
    return valid_king_moves(x, idx);
  case PIECE_PAWN:
    return valid_pawn_moves(x, idx);
  default:
    __builtin_trap();
  }
}

static iterator reset_iterator_promotion_piece(gamestate g, iterator i)
{
  if (is_iterator_finished(i)) {
    return i;
  }
  int idx = iterator_position(i);
  int piece = get_piece(g, idx);
  if (piece == PIECE_PAWN &&
      rank(idx) == 6 &&
      i.current_piece_bb != 0) {
    i.promotion_piece = PIECE_QUEEN;
  }
  return i;  
}

static iterator reset_iterator_moves(gamestate g, iterator i)
{
  if (is_iterator_finished(i)) {
    return zerostate();
  } else {
    int idx = iterator_position(i);
    uint64_t moves = valid_piece_moves(g, idx);
    i.current_piece_bb = moves;
    i = reset_iterator_promotion_piece(g, i);
    return i;
  }
}

static iterator advance_iterator(gamestate g, iterator i)
{
  if (i.current_piece_bb) {
    if (i.promotion_piece) {
      i.promotion_piece--;
    }
    if (!i.promotion_piece) {
      i.current_piece_bb = advance_bb_iterator(i.current_piece_bb);
      i = reset_iterator_promotion_piece(g, i);
    }
  }

  while (! is_iterator_finished(i) && ! i.current_piece_bb) {
    if (i.rooks_bb) {
      i.rooks_bb = advance_bb_iterator(i.rooks_bb);
    } else if (i.knights_bb) {
      i.knights_bb = advance_bb_iterator(i.knights_bb);
    } else if (i.bishops_bb) {
      i.bishops_bb = advance_bb_iterator(i.bishops_bb);
    } else if (i.queens_bb) {
      i.queens_bb = advance_bb_iterator(i.queens_bb);
    } else if (i.kings_bb) {
      i.kings_bb = advance_bb_iterator(i.kings_bb);
    } else if (i.pawns_bb) {
      i.pawns_bb = advance_bb_iterator(i.pawns_bb);
    }
    i = reset_iterator_moves(g, i);
  }
  if (is_iterator_finished(i) && i.current_piece_bb != 0) {
    __builtin_trap();
  }
  
  return i;
}

static gamestate switch_sides(gamestate x)
{
  uint64_t enemy_pieces_bb = enemy_pieces(x);
  x.current_player_bb = enemy_pieces_bb;

  x.rooks_bb   = __builtin_bswap64(x.rooks_bb);
  x.knights_bb = __builtin_bswap64(x.knights_bb);
  x.bishops_bb = __builtin_bswap64(x.bishops_bb);
  x.queens_bb  = __builtin_bswap64(x.queens_bb);
  x.kings_bb   = __builtin_bswap64(x.kings_bb);
  x.pawns_bb   = __builtin_bswap64(x.pawns_bb);
  return x;
}
                 
static iterator mkIterator(gamestate g)
{
  iterator x = g;
  x.rooks_bb   &= x.current_player_bb;
  x.knights_bb &= x.current_player_bb;
  x.bishops_bb &= x.current_player_bb;
  x.queens_bb  &= x.current_player_bb;
  x.kings_bb   &= x.current_player_bb;
  x.pawns_bb   &= x.current_player_bb;
  x.promotion_piece = 0;
  
  x = reset_iterator_moves(g, x);
  if (! x.current_piece_bb) {
    x = advance_iterator(g, x);
  }
  
  return x;
}

static uint64_t all_rotations(uint64_t bb)
{
  bb |= rotate_bb(bb);
  bb |= rotate_bb(bb);
  bb |= rotate_bb(bb);
  return bb;
}

static gamestate new_game()
{
  gamestate x;
  x.rooks_bb =
    bit(mkPosition(0,0)) |
    bit(mkPosition(7,0)) |
    bit(mkPosition(0,7)) |
    bit(mkPosition(7,7));
  x.knights_bb =
    bit(mkPosition(1,0)) |
    bit(mkPosition(6,0)) |
    bit(mkPosition(1,7)) |
    bit(mkPosition(6,7));
  x.bishops_bb =
    bit(mkPosition(2,0)) |
    bit(mkPosition(5,0)) |
    bit(mkPosition(2,7)) |
    bit(mkPosition(5,7));
  x.queens_bb =
    bit(mkPosition(3,0)) |
    bit(mkPosition(3,7));
  x.kings_bb =
    bit(mkPosition(4,0)) |
    bit(mkPosition(4,7));
  x.pawns_bb =
    ((uint64_t)0xFF << RANK) |
    ((uint64_t)0xFF << (6*RANK));
  x.current_player_bb = 0xFFFF;
  x.en_passant_sq = POSITION_INVALID;
  x.castle_flags = 0xF;
  x.is_white = true;
  return x;
}

// Flips the board so white is black and black is white.
// In our model, it is always white's turn to move.
static gamestate swap_board(gamestate g)
{
  g.current_player_bb ^= all_pieces(g);
  g.rooks_bb = flip_bb(g.rooks_bb);
  g.knights_bb = flip_bb(g.knights_bb);
  g.bishops_bb = flip_bb(g.bishops_bb);
  g.queens_bb = flip_bb(g.queens_bb);
  g.kings_bb = flip_bb(g.kings_bb);
  g.pawns_bb = flip_bb(g.pawns_bb);
  g.current_player_bb = flip_bb(g.current_player_bb);
  if (g.en_passant_sq != POSITION_INVALID) {
    g.en_passant_sq = mkPosition(file(g.en_passant_sq), 7 - rank(g.en_passant_sq));
  }
  {
    int flags = 0;
    if (g.castle_flags & CASTLE_WHITE_KINGSIDE)
      flags |= CASTLE_BLACK_KINGSIDE;
    if (g.castle_flags & CASTLE_WHITE_QUEENSIDE)
      flags |= CASTLE_BLACK_QUEENSIDE;
    if (g.castle_flags & CASTLE_BLACK_KINGSIDE)
      flags |= CASTLE_WHITE_KINGSIDE;
    if (g.castle_flags & CASTLE_BLACK_QUEENSIDE)
      flags |= CASTLE_WHITE_QUEENSIDE;
    g.castle_flags = flags;
  }
  return g;
}

static move swap_move(move m)
{
  m.from = mkPosition(file(m.from), 7 - rank(m.from));
  m.to = mkPosition(file(m.to), 7 - rank(m.to));
  return m;
}

static bool is_kingside_castle(gamestate g, move m)
{
  int from_piece = get_piece(g, m.from);
  return
    from_piece == PIECE_KING &&
    m.to == CASTLE_KINGSIDE_KPOS &&
    (g.castle_flags & CASTLE_WHITE_KINGSIDE);
}

static bool is_queenside_castle(gamestate g, move m)
{
  int from_piece = get_piece(g, m.from);
  return
    from_piece == PIECE_KING &&
    m.to == CASTLE_QUEENSIDE_KPOS &&
    (g.castle_flags & CASTLE_WHITE_QUEENSIDE);
}

static bool is_promotion(move m) { return promotion_piece(m) == PIECE_EMPTY; }

static gamestate apply_move(gamestate g, move m)
{
  gamestate g_orig = g;
  int from_piece = get_piece(g, m.from);
  int prom_piece = promotion_piece(m);
  m.to &= 0x3F;
  int to_piece = get_piece(g, m.to);
  if (to_piece != PIECE_EMPTY) {
    uint64_t to_bb = get_piece_bb(g, to_piece);
    g = set_piece_bb(g, to_piece, clear_bit(to_bb, m.to));
  }
  // Non-promotion moves
  if (prom_piece == PIECE_EMPTY) {
    uint64_t from_bb = get_piece_bb(g, from_piece);
    g = set_piece_bb(g, from_piece, clear_bit(from_bb, m.from) | bit(m.to));
    // Promotions
  } else {
    uint64_t piece_bb = get_piece_bb(g, prom_piece);
    g.pawns_bb = clear_bit(g.pawns_bb, m.from);
    g = set_piece_bb(g, prom_piece, piece_bb | bit(m.to));
  }
  // Capture the pawn properly during En Passant capture
  if (from_piece == PIECE_PAWN && m.to == g.en_passant_sq) {
    g.pawns_bb = clear_bit(g.pawns_bb, g.en_passant_sq - RANK);
  }
  // Set En Passant target square on double-jump
  if (from_piece == PIECE_PAWN && rank(m.to) - rank(m.from) == 2) {
    g.en_passant_sq = m.from + RANK;
  } else {
    g.en_passant_sq = POSITION_INVALID;
  }
  // Check for kingside castle.
  if (is_kingside_castle(g_orig, m)) {
    g.castle_flags &= ~CASTLE_WHITE_KINGSIDE;
    g.rooks_bb = clear_bit(g.rooks_bb, mkPosition(7,0));
    g.current_piece_bb = clear_bit(g.current_piece_bb, mkPosition(7,0));
    g.rooks_bb = set_bit(g.rooks_bb, CASTLE_KINGSIDE_RPOS);
    g.current_piece_bb = set_bit(g.current_piece_bb, CASTLE_KINGSIDE_RPOS);
  }
  // Check for queenside castle
  if (is_queenside_castle(g_orig,m)) {
    g.castle_flags &= ~CASTLE_WHITE_QUEENSIDE;
    g.rooks_bb = clear_bit(g.rooks_bb, mkPosition(0,0));
    g.current_piece_bb = clear_bit(g.current_piece_bb, mkPosition(0,0));
    g.rooks_bb = set_bit(g.rooks_bb, CASTLE_QUEENSIDE_RPOS);
    g.current_piece_bb = set_bit(g.current_piece_bb, CASTLE_QUEENSIDE_RPOS);
  }
  // Clear kingside rook flags
  if (g.castle_flags & CASTLE_WHITE_KINGSIDE &&
      from_piece == PIECE_ROOK &&
      m.from == mkPosition(7,0)) {
    g.castle_flags &= ~CASTLE_WHITE_KINGSIDE;
  }
  // Clear queenside rook flags
  if (g.castle_flags & CASTLE_WHITE_QUEENSIDE &&
      from_piece == PIECE_ROOK &&
      m.from == mkPosition(0,0)) {
    g.castle_flags &= ~CASTLE_WHITE_QUEENSIDE;
  }
  // Clear eaten kingside rook flags
  if (to_piece == PIECE_ROOK &&
      m.to == mkPosition(7,7)) {
    g.castle_flags &= ~CASTLE_BLACK_KINGSIDE;
  }
  // Clear eaten queenside rook flags
  if (to_piece == PIECE_ROOK &&
      m.to == mkPosition(0,7)) {
    g.castle_flags &= ~CASTLE_BLACK_QUEENSIDE;
  }
  // Clear king's castling flags
  if (from_piece == PIECE_KING) {
    g.castle_flags &= ~(CASTLE_WHITE_KINGSIDE | CASTLE_WHITE_QUEENSIDE);
  }
  // Set colors
  g.current_player_bb = clear_bit(g.current_player_bb, m.from) | bit(m.to);

  g.is_white = ! g.is_white;

  g = swap_board(g);
  
  return g;
}

static uint64_t mkRank(int idx) { return ((uint64_t)0xFF << (idx*RANK)); }
static uint64_t mkFile(int idx) {
  return
    bit(mkPosition(idx,0)) |
    bit(mkPosition(idx,1)) |
    bit(mkPosition(idx,2)) |
    bit(mkPosition(idx,3)) |
    bit(mkPosition(idx,4)) |
    bit(mkPosition(idx,5)) |
    bit(mkPosition(idx,6)) |
    bit(mkPosition(idx,7));
}

static bool iter_lt(iterator x, iterator y)
{
  return
    (x.rooks_bb < y.rooks_bb ||
     (x.rooks_bb == y.rooks_bb &&
      (x.knights_bb < y.knights_bb ||
       (x.knights_bb == y.knights_bb &&
        (x.bishops_bb < y.bishops_bb ||
         (x.bishops_bb == y.bishops_bb &&
          (x.queens_bb < y.queens_bb ||
           (x.queens_bb == y.queens_bb &&
            (x.kings_bb < y.kings_bb ||
             (x.kings_bb == y.kings_bb &&
              (x.pawns_bb < y.pawns_bb ||
               (x.pawns_bb == y.pawns_bb &&
                (x.current_piece_bb < y.current_piece_bb ||
                 (x.current_piece_bb == y.current_piece_bb &&
                  (x.promotion_piece < y.promotion_piece)))))))))))))));
}

static uint64_t movepoints(gamestate g)
{
  uint64_t movepoints = 0;
  iterator i = mkIterator(g);
  while (! is_iterator_finished(i)) {
    movepoints |= i.current_piece_bb;
    i.current_piece_bb = 0;
    i = advance_iterator(g, i);
  }
  return movepoints;
}

static uint64_t vulnerables(gamestate g)
{
  g = swap_board(g);
  uint64_t ret = movepoints(g) & enemy_pieces(g);
  ret = flip_bb(ret);
  return ret;
}

static bool is_in_check(gamestate g)
{
  uint64_t white_kings = g.kings_bb & g.current_player_bb;
  return ((white_kings & vulnerables(g)) != 0);
}

static bool results_in_check(gamestate g, move m)
{
  gamestate g2 = swap_board(apply_move(g,m));
  return is_in_check(g2);
}

static bool is_legal(gamestate g, move m)
{
  if (results_in_check(g,m)) {
    return false;
  }
  move kingside_castle_part; kingside_castle_part.from = mkPosition(4,0); kingside_castle_part.to = mkPosition(5,0);
  move queenside_castle_part; queenside_castle_part.from = mkPosition(4,0); queenside_castle_part.to = mkPosition(3,0);
  bool valid_kingside_castle =
    is_kingside_castle(g, m)
    ? (! results_in_check(g, kingside_castle_part) &&
       ! is_in_check(g))
    : true;
  bool valid_queenside_castle =
    is_queenside_castle(g, m)
    ? (! results_in_check(g, queenside_castle_part) &&
       ! is_in_check(g))
    : true;
  return
    valid_kingside_castle &&
    valid_queenside_castle;
}

// Like advance_iterator, but skip moves that leave the king in check
static iterator advance_iterator_legal(gamestate g, iterator i)
{
  while (1) {
    i = advance_iterator(g, i);
    if (is_iterator_finished(i)) {
      break;
    }
    move m = dereference_iterator(i);
    if (is_legal(g, m)) {
      break;
    }
  }
  return i;
}

static iterator mkLegalIterator(gamestate g)
{
  iterator i = mkIterator(g);
  move m = dereference_iterator(i);
  if (! is_legal(g, m)) {
    i = advance_iterator_legal(g, i);
  }
  return i;
}

static uint64_t legal_movepoints(gamestate g)
{
  uint64_t movepoints = 0;
  iterator i = mkLegalIterator(g);
  while (! is_iterator_finished(i)) {
    move m = dereference_iterator(i);
    movepoints |= bit(m.to);
    i = advance_iterator_legal(g, i);
  }
  return movepoints;
}

static int num_legal_moves(gamestate g)
{
  iterator i = mkLegalIterator(g);
  int count = 0;
  while (! is_iterator_finished(i)) {
    i = advance_iterator_legal(g, i);
    count++;
  }
  return count;
}

static iterator legalize(gamestate g, iterator i)
{
  while (! is_iterator_finished(i)) {
    move m = dereference_iterator(i);
    if (is_legal(g, m)) {
      break;
    }
    i = advance_iterator(g, i);
  }
  return i;
}

static uint64_t piece_legal_movepoints(gamestate g, int idx)
{
  iterator i = zerostate();
  int piece = get_piece(g, idx);
  i = set_piece_bb(i, piece, bit(idx));
  i = reset_iterator_moves(g, i);
  i = legalize(g, i);
  uint64_t ret = 0;
  while (! is_iterator_finished(i)) {
    move m = dereference_iterator(i);
    ret |= bit(m.to);
    i = advance_iterator_legal(g, i);
  }
  return ret;
}

static move mkPromotion(int from, int piece)
{
  if (rank(from) != 6)
    __builtin_trap();
  move m;
  m.from = from;
  m.to = from + RANK;
  m.to |= piece << 6;
  return m;
}

// Public functions
// we could use __builtin_popcountl, but we'd need -march=native to eliminate library call which is less portable.
static uint64_t popcount64(uint64_t x)
{
    x = (x & 0x5555555555555555ULL) + ((x >> 1) & 0x5555555555555555ULL);
    x = (x & 0x3333333333333333ULL) + ((x >> 2) & 0x3333333333333333ULL);
    x = (x & 0x0F0F0F0F0F0F0F0FULL) + ((x >> 4) & 0x0F0F0F0F0F0F0F0FULL);
    return (x * 0x0101010101010101ULL) >> 56;
}
static int num_bits(uint64_t x) { return popcount64(x); }

static int score_pieces(gamestate g)
{
  return
    num_bits(g.pawns_bb   & g.current_player_bb) * VALUE_PAWN +
    num_bits(g.knights_bb & g.current_player_bb) * VALUE_KNIGHT +
    num_bits(g.bishops_bb & g.current_player_bb) * VALUE_BISHOP +
    num_bits(g.rooks_bb   & g.current_player_bb) * VALUE_ROOK +
    num_bits(g.queens_bb  & g.current_player_bb) * VALUE_QUEEN;
}

static int score_availability(gamestate g)
{
  int num_moves = num_legal_moves(g);
  if (num_moves == 0)
    return VALUE_CHECKMATE;
  return VALUE_AVAILABLE_MOVE * num_moves;
}

static int score_attacking(gamestate g)
{
  return num_bits(enemy_pieces(g) & movepoints(g));
}

static uint64_t center()
{
  return
    bit(mkPosition(2,2)) |
    bit(mkPosition(2,3)) |
    bit(mkPosition(2,4)) |
    bit(mkPosition(2,5)) |
    
    bit(mkPosition(3,2)) |
    bit(mkPosition(3,3)) |
    bit(mkPosition(3,4)) |
    bit(mkPosition(3,5)) |
    
    bit(mkPosition(4,2)) |
    bit(mkPosition(4,3)) |
    bit(mkPosition(4,4)) |
    bit(mkPosition(4,5)) |
    
    bit(mkPosition(5,2)) |
    bit(mkPosition(5,3)) |
    bit(mkPosition(5,4)) |
    bit(mkPosition(5,5));    
}

static int score_center(gamestate g)
{
  int num_centers = num_bits(g.current_player_bb & center());
  return num_centers * VALUE_CENTER;
}

static int evaluate(gamestate g)
{
  return score_pieces(g) + score_availability(g) + score_attacking(g);
}


typedef struct negamax_ret {
  move m;
  int score;
} negamax_ret;


static int negamax(gamestate g, int depth, int color)
{
  if (depth == 0)
    return color*evaluate(g);
  int max = VALUE_NEGAMAX_START;
  for (iterator i = mkLegalIterator(g); ! is_iterator_finished(i); i = advance_iterator_legal(g, i))  {
    move m = dereference_iterator(i);
    gamestate g2 = apply_move(g, m);
    int score = -negamax(g2, depth-1, color*-1);
    if (score > max)
      max = score;
  }
  return max;
}

static move best_move(gamestate g, int depth)
{
  int max = VALUE_NEGAMAX_START;
  move ret; ret.from = POSITION_INVALID; ret.to = POSITION_INVALID;
  for (iterator i = mkLegalIterator(g); ! is_iterator_finished(i); i = advance_iterator_legal(g, i)) {
    move m = dereference_iterator(i);
    gamestate g2 = apply_move(g, m);
    int score = -negamax(g2, depth, -1);
    if (score > max) {
      max = score;
      ret = m;
    }
  }
  return ret;
}

static uint64_t perft(gamestate g, int depth)
{
  if (depth == 0) { return 1; }
  uint64_t n = 0;
  for (iterator i = mkLegalIterator(g); ! is_iterator_finished(i); i = advance_iterator_legal(g, i)) {
    move m = dereference_iterator(i);
    gamestate g2 = apply_move(g, m);
    n += perft(g2, depth-1);
  }
  return n;
}

typedef struct packed_move {
  union {
    move m;
    uint64_t packed;
  };
} packed_move;


static gamestate parse_fen(const char* s)
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
      default: __builtin_trap(); goto end_loop;
      }
    }
  end_row:;
  }
 end_loop:
  // Color
  switch (*s++) {
  case 'w': g.is_white = true; break;
  case 'b': g.is_white = false; break;
  default: __builtin_trap();
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
    default: __builtin_trap();
    }
  }
  if (!g.is_white) {
    g = swap_board(g);
  }
  return g;
}

static char piece_char(int p, bool is_white) {
  switch (p) {
  case PIECE_EMPTY: return '.';
  case PIECE_ROOK: return is_white ? 'R' : 'r';
  case PIECE_KNIGHT: return is_white ? 'N' : 'n';
  case PIECE_BISHOP: return is_white ? 'B' : 'b';
  case PIECE_QUEEN: return is_white ? 'Q' : 'q';
  case PIECE_KING: return is_white ? 'K' : 'k';
  case PIECE_PAWN: return is_white ? 'P' : 'p';
  default: __builtin_trap();
  }
}

static void print_pos(int pos, char *buffer)
{
  *buffer++ = file(pos) + 'a';
  *buffer++ = rank(pos) + '1';
}

static void print_fen(gamestate g, char *buffer)
{
  bool swap = ! g.is_white;
  if (swap)
    g = swap_board(g);
  int num_empty_squares = 0;
  for (int r = 8; r --> 0;) {
    for (int f = 0; f < 8; f++) {
      int pos = mkPosition(f, r);
      int piece = get_piece(g, pos);
      if (piece == PIECE_EMPTY) {
        num_empty_squares++;
        continue;
      } else if (num_empty_squares > 0) {
        *buffer++ = (num_empty_squares + '0');
        num_empty_squares = 0;
      }
      bool is_white = is_bit_set(g.current_player_bb, pos);
      char c = piece_char(piece, is_white);
      *buffer++ = c;
    }
    if (num_empty_squares > 0) {
      *buffer++ = (num_empty_squares + '0');
      num_empty_squares = 0;
    }
    if (r > 0) {
      *buffer++ = '/';
    }
  }
 skip_loop:
  *buffer++ = ' ';
  *buffer++ = g.is_white
    ? 'w'
    : 'b';
  *buffer++ = ' ';
  // Castle Flags
  {
    if (g.castle_flags & CASTLE_WHITE_KINGSIDE)
      *buffer++ = 'K';
    if (g.castle_flags & CASTLE_WHITE_QUEENSIDE)
      *buffer++ = 'Q';
    if (g.castle_flags & CASTLE_BLACK_KINGSIDE)
      *buffer++ = 'k';
    if (g.castle_flags & CASTLE_BLACK_QUEENSIDE)
      *buffer++ = 'q';
    if (! g.castle_flags)
      *buffer++ = '-';
  }
  *buffer++ = ' ';
  if (g.en_passant_sq == POSITION_INVALID) {
    *buffer++ = '-';
  } else {
    //printf("%d---", g.en_passant_sq);
    print_pos(g.en_passant_sq, buffer);
    buffer += 2;
  }
  *buffer++ = ' ';
  *buffer++ = '0';
  *buffer++ = ' ';
  *buffer++ = '1';
  *buffer++ = 0;
}

// Buffer must have at least 7 characters available
static void print_move(move m, char *buffer)
{
  print_pos(m.from, buffer); buffer+=2;
  print_pos(m.to & 0x3F, buffer); buffer+=2;
  int piece = promotion_piece(m);
  switch (piece) {
  case PIECE_EMPTY: break;
  case PIECE_ROOK: *buffer++ = '/'; *buffer++ = 'R'; break;
  case PIECE_KNIGHT: *buffer++ = '/'; *buffer++ = 'N'; break;
  case PIECE_BISHOP: *buffer++ = '/'; *buffer++ = 'B'; break;
  case PIECE_QUEEN: *buffer++ = '/'; *buffer++ = 'Q'; break;
  default: __builtin_trap();
  }
  *buffer++ = 0;
}

static int parse_pos(const char* buffer)
{
  int file = *buffer++ - 'a';
  int rank = *buffer++ - '1';
  return mkPosition(file, rank);
}

static move parse_move(const char* buffer)
{
  move m;
  m.from = parse_pos(buffer);
  buffer += 2;
  m.to = parse_pos(buffer);
  buffer += 2;
  if (*buffer) {
    buffer++;
    switch (*buffer++) {
    case 'R': m.to |= (PIECE_ROOK << 6); break;
    case 'N': m.to |= (PIECE_KNIGHT << 6); break;
    case 'B': m.to |= (PIECE_BISHOP << 6); break;
    case 'Q': m.to |= (PIECE_QUEEN << 6); break;
    default: __builtin_trap();
    }
  }
  return m;
}

// Used when generating pf_best_move
/* extern "C" void custom_main(const char *g_str, char *m_dest, int depth) */
/* { */
/*   gamestate g = parse_fen(g_str); */
/*   move m = best_move(g, depth); */
/*   if (! g.is_white) */
/*     m = swap_move(m); */
/*   print_move(m, m_dest); */
/* } */

// Used when generating pf_apply_move
extern "C" void custom_main(const char *g_str, const char *m_str, char *g_dest)
{
  gamestate g = parse_fen(g_str);
  move m = parse_move(m_str);
  if (! g.is_white)
    m = swap_move(m);
  gamestate g2 = apply_move(g, m);
  print_fen(g2, g_dest);
}
