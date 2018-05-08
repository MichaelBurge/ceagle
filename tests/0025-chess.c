#lang ceagle

#include <ceagle/test-support/chess-engine.c>
#include <ceagle/test-support/test-lib.c>

void test_ray()
{
  // Base rays
  {
    uint64_t center = mkPosition(3,3);
    uint64_t nw_ray =
      bit(mkPosition(0,6)) |
      bit(mkPosition(1,5)) |
      bit(mkPosition(2,4));
    uint64_t n_ray =
      bit(mkPosition(3,4)) |
      bit(mkPosition(3,5)) |
      bit(mkPosition(3,6)) |
      bit(mkPosition(3,7));
    uint64_t ne_ray =
      bit(mkPosition(4,4)) |
      bit(mkPosition(5,5)) |
      bit(mkPosition(6,6)) |
      bit(mkPosition(7,7));
    uint64_t e_ray =
      bit(mkPosition(4,3)) |
      bit(mkPosition(5,3)) |
      bit(mkPosition(6,3)) |
      bit(mkPosition(7,3));
    uint64_t se_ray =
      bit(mkPosition(4,2)) |
      bit(mkPosition(5,1)) |
      bit(mkPosition(6,0));
    uint64_t s_ray =
      bit(mkPosition(3,2)) |
      bit(mkPosition(3,1)) |
      bit(mkPosition(3,0));
    uint64_t sw_ray =
      bit(mkPosition(2,2)) |
      bit(mkPosition(1,1)) |
      bit(mkPosition(0,0));
    uint64_t w_ray =
      bit(mkPosition(2,3)) |
      bit(mkPosition(1,3))|
      bit(mkPosition(0,3));

    assert_equal_bb("nw", nw_ray, mkRay(center, DIRECTION_NORTHWEST));
    assert_equal_bb("n",  n_ray,  mkRay(center, DIRECTION_NORTH));
    assert_equal_bb("ne", ne_ray, mkRay(center, DIRECTION_NORTHEAST));
    assert_equal_bb("e",  e_ray,  mkRay(center, DIRECTION_EAST));
    assert_equal_bb("se", se_ray, mkRay(center, DIRECTION_SOUTHEAST));
    assert_equal_bb("s",  s_ray,  mkRay(center, DIRECTION_SOUTH));
    assert_equal_bb("sw", sw_ray, mkRay(center, DIRECTION_SOUTHWEST));
    assert_equal_bb("w",  w_ray,  mkRay(center, DIRECTION_WEST));
  }
}

/* int main() { */
/*   test_ray(); */
/*   return 0; */
/* } */

int main() {
  __builtin_set_test_result(1234);
  __builtin_set_max_iterations(1000000000);
  __builtin_set_max_simulator_memory(20000000);
  test_ray();
  return 1234;
}
