#lang ceagle

#include <ceagle/test-support/chess-engine.c>
#include <ceagle/test-support/test-lib.c>
#include <ceagle/test-support/chess-debug.c>

int main()
{
  __builtin_set_test_result(1);
  __builtin_set_max_simulator_memory(200000000);
  __builtin_set_max_iterations(1000000000);
// Rays with blockers
  {
    uint64_t center = mkPosition(3,3);
    // W
    {
      gamestate g = zerostate();
      g.pawns_bb =
        bit(mkPosition(1,3)) |
        bit(mkPosition(0,3));
      uint64_t expected =
        bit(mkPosition(2,3)) |
        bit(mkPosition(1,3));

      uint64_t actual = shoot_ray_until_blocker(g, center, DIRECTION_WEST);
      /* puts("== expected:\n"); __builtin_print_word(expected); putchar('\n'); */
      /* puts("== actual:\n"); __builtin_print_word(actual); putchar('\n'); */
      /* uint64_t base_ray = mkRay(center, DIRECTION_EAST); */
      /* puts("== base_ray:\n"); __builtin_print_word(base_ray); putchar('\n'); */
      /* uint64_t pieces = all_pieces(g); */
      /* puts("== all_pieces:\n"); __builtin_print_word(pieces); putchar('\n'); */
      /* uint64_t blockers = base_ray & pieces; */
      /* puts("== blockers:\n"); __builtin_print_word(blockers); putchar('\n'); */
      /* int blocker = closest_blocker(blockers, DIRECTION_EAST); */
      /* puts("== blocker: "); __builtin_print_word(blocker); putchar('\n'); */
      /* uint64_t blocker_ray = mkRay(blocker, DIRECTION_EAST); */
      /* puts("== blocker_ray:\n"); __builtin_print_word(blocker_ray); */
      /* uint64_t movable_squares_without_capture = base_ray ^ blocker_ray ^ bit(blocker); */
      /* puts("== movable_squares_without_capture\n"); __builtin_print_word(movable_squares_without_capture); putchar('\n'); */
      assert_equal_int("w_blocker", mkPosition(1,3), closest_blocker(g.pawns_bb, DIRECTION_WEST));
      assert_equal_bb("w", expected, actual);
    }
    // NW
    {
      gamestate g = zerostate();
      g.pawns_bb =
        bit(mkPosition(1,5)) |
        bit(mkPosition(0,6));
      uint64_t expected =
        bit(mkPosition(2,4)) |
        bit(mkPosition(1,5));
      uint64_t actual = shoot_ray_until_blocker(g, center, DIRECTION_NORTHWEST);
      assert_equal_int("nw_blocker", mkPosition(1,5), closest_blocker(g.pawns_bb, DIRECTION_NORTHWEST));
      assert_equal_bb("nw", expected, actual);
    }
    // N
    {
      gamestate g = zerostate();
      g.pawns_bb =
        bit(mkPosition(3,6)) |
        bit(mkPosition(3,7));
      uint64_t expected =
        bit(mkPosition(3,4)) |
        bit(mkPosition(3,5)) |
        bit(mkPosition(3,6));
      uint64_t actual = shoot_ray_until_blocker(g, center, DIRECTION_NORTH);
      assert_equal_int("n_blocker", mkPosition(3,6), closest_blocker(g.pawns_bb, DIRECTION_NORTH));
      assert_equal_bb("n", expected, actual);
    }
    // NE
    {
      gamestate g = zerostate();
      g.pawns_bb =
        bit(mkPosition(6,6)) |
        bit(mkPosition(7,7));
      uint64_t expected =
        bit(mkPosition(4,4)) |
        bit(mkPosition(5,5)) |
        bit(mkPosition(6,6));
      uint64_t actual = shoot_ray_until_blocker(g, center, DIRECTION_NORTHEAST);
      assert_equal_int("ne_blocker", mkPosition(6,6), closest_blocker(g.pawns_bb, DIRECTION_NORTHEAST));
      assert_equal_bb("ne", expected, actual);
    }
    // NE 2
    {
      uint64_t center = mkPosition(0,2);
      gamestate g = zerostate();
      g.pawns_bb =
        bit(mkPosition(4,6)) |
        bit(mkPosition(5,7));
      uint64_t expected =
        bit(mkPosition(1,3)) |
        bit(mkPosition(2,4)) |
        bit(mkPosition(3,5)) |
        bit(mkPosition(4,6));
      uint64_t actual = shoot_ray_until_blocker(g, center, DIRECTION_NORTHEAST);
      assert_equal_bb("ne_2", expected, actual);
    }
    // NE 3
    {
      uint64_t bb = bit(mkPosition(4,6)) | bit(mkPosition(5,7));
      int i = closest_blocker(bb, DIRECTION_NORTHEAST);
      assert_equal_int("ne_3", mkPosition(4,6), i);
    }
    // E
    {
      gamestate g = zerostate();
      g.pawns_bb =
        bit(mkPosition(6,3)) |
        bit(mkPosition(7,3));
      uint64_t expected =
        bit(mkPosition(4,3)) |
        bit(mkPosition(5,3)) |
        bit(mkPosition(6,3));
      uint64_t actual = shoot_ray_until_blocker(g, center, DIRECTION_EAST);

      assert_equal_int("e_blocker", mkPosition(6,3), closest_blocker(g.pawns_bb, DIRECTION_EAST));
      assert_equal_bb("e", expected, actual);
    }
    // SE
    {
      gamestate g = zerostate();
      g.pawns_bb =
        bit(mkPosition(5,1)) |
        bit(mkPosition(6,0));
      uint64_t expected =
        bit(mkPosition(4,2)) |
        bit(mkPosition(5,1));
      uint64_t actual = shoot_ray_until_blocker(g, center, DIRECTION_SOUTHEAST);
      assert_equal_int("se_blocker", mkPosition(5,1), closest_blocker(g.pawns_bb, DIRECTION_SOUTHEAST));
      assert_equal_bb("se", expected, actual);
    }
    // S
    {
      gamestate g = zerostate();
      g.pawns_bb =
        bit(mkPosition(3,1)) |
        bit(mkPosition(3,0));
      uint64_t expected =
        bit(mkPosition(3,2)) |
        bit(mkPosition(3,1));
      uint64_t actual = shoot_ray_until_blocker(g, center, DIRECTION_SOUTH);
      assert_equal_int("s_blocker", mkPosition(3,1), closest_blocker(g.pawns_bb, DIRECTION_SOUTH));
      assert_equal_bb("s", expected, actual);
    }
    // SW
    {
      gamestate g = zerostate();
      g.pawns_bb =
        bit(mkPosition(1,1)) |
        bit(mkPosition(0,0));
      uint64_t expected =
        bit(mkPosition(2,2)) |
        bit(mkPosition(1,1));
      uint64_t actual = shoot_ray_until_blocker(g, center, DIRECTION_SOUTHWEST);
      assert_equal_int("sw_blocker", mkPosition(1,1), closest_blocker(g.pawns_bb, DIRECTION_SOUTHWEST));
      assert_equal_bb("sw", expected, actual);
    }
  }
  return 1;
}
