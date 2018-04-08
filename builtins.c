#lang ceagle

// https://github.com/nemequ/portable-snippets/blob/master/builtin/builtin.h

typedef unsigned int psnip_uint64_t;

psnip_uint64_t psnip_builtin_bswap64(psnip_uint64_t v) {
  return
    ((v & (((psnip_uint64_t) 0xff) << 56)) >> 56) |
    ((v & (((psnip_uint64_t) 0xff) << 48)) >> 40) |
    ((v & (((psnip_uint64_t) 0xff) << 40)) >> 24) |
    ((v & (((psnip_uint64_t) 0xff) << 32)) >>  8) |
    ((v & (((psnip_uint64_t) 0xff) << 24)) <<  8) |
    ((v & (((psnip_uint64_t) 0xff) << 16)) << 24) |
    ((v & (((psnip_uint64_t) 0xff) <<  8)) << 40) |
    ((v & (((psnip_uint64_t) 0xff)      )) << 56);
}

int psnip_builtin_ctz64(psnip_uint64_t v) {
  static const unsigned char MultiplyDeBruijnBitPosition[] = {
     0,  1, 56,  2, 57, 49, 28,  3, 61, 58, 42, 50, 38, 29, 17,  4,
    62, 47, 59, 36, 45, 43, 51, 22, 53, 39, 33, 30, 24, 18, 12,  5,
    63, 55, 48, 27, 60, 41, 37, 16, 46, 35, 44, 21, 52, 32, 23, 11,
    54, 26, 40, 15, 34, 20, 31, 10, 25, 14, 19,  9, 13,  8,  7,  6
  };

  return
    MultiplyDeBruijnBitPosition[((psnip_uint64_t)((v & -v) * 0x03f79d71b4ca8b09ULL)) >> 58];
}

int psnip_builtin_clz64( psnip_uint64_t x )
{
  if (x == 0) return 64;
  for (int n = 0; ((x & 0x8000000000000000) == 0); n++, x <<= 1);
  return n;
}
