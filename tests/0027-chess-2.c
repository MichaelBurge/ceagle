#lang ceagle

typedef unsigned int uint64_t;

const int RANK = 8;
const int DIRECTION_EAST  = 0;
const int DIRECTION_WEST  = 1;
const int DIRECTION_NORTH = 2;
const int DIRECTION_SOUTH = 3;
const int DIRECTION_NORTHEAST = 4;
const int DIRECTION_NORTHWEST = 5;
const int DIRECTION_SOUTHEAST = 6;
const int DIRECTION_SOUTHWEST = 7;

const uint64_t RAY_A1A8 = 0x0101010101010101;
const uint64_t RAY_H1H8 = RAY_A1A8 << (RANK-1);

static uint64_t mkPosition(int file, int rank)
{
  return rank * RANK + file;
}

static uint64_t bit(uint64_t idx) {
  if (idx >= 64 || idx < 0) {
    return 0;
  }
  return ((uint64_t)(1) << idx);
}
static uint64_t clear_bit(uint64_t x, uint64_t idx) { return x & ~bit(idx); }
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

int main() {
  __builtin_set_test_result(1);
  __builtin_set_max_iterations(10000000);
  uint64_t center = mkPosition(3,3);
  return mkRay(center, DIRECTION_NORTHWEST) ==
    (bit(mkPosition(0,6)) |
     bit(mkPosition(1,5)) |
     bit(mkPosition(2,4)));
}
