#include <inttypes.h>
#include <stddef.h>

#ifdef __x86_64__
#include <immintrin.h>
#endif

#include "HsFFI.h"

HsInt _hs_bitvec_popcount(const uint32_t *src, HsInt len) {
    HsInt count = 0;
    #pragma omp simd
    for (size_t i = 0; i < len; i++) {
        uint32_t x = src[i];
        // count += popcount(t);
        // https://bits.stephan-brumme.com/countBits.html
        x = x - ((x >> 1) & 0x55555555);
        x = (x & 0x33333333) + ((x >> 2) & 0x33333333);
        x = (x + (x >> 4)) & 0x0f0f0f0f;
        count += (x * 0x01010101) >> 24;
    }
    return count;
}

void _hs_bitvec_com(uint8_t *dest, uint8_t *src, HsInt len) {
    #pragma omp simd
    for (size_t i = 0; i < len; i++) {
        dest[i] = ~src[i];
    }
}

void _hs_bitvec_and(uint8_t *dest, const uint8_t *src1, const uint8_t *src2, HsInt len) {
    #pragma omp simd
    for (size_t i = 0; i < len; i++) {
        dest[i] = src1[i] & src2[i];
    }
}

void _hs_bitvec_ior(uint8_t *dest, const uint8_t *src1, const uint8_t *src2, HsInt len) {
    #pragma omp simd
    for (size_t i = 0; i < len; i++) {
        dest[i] = src1[i] | src2[i];
    }
}

void _hs_bitvec_xor(uint8_t *dest, const uint8_t *src1, const uint8_t *src2, HsInt len) {
    #pragma omp simd
    for (size_t i = 0; i < len; i++) {
        dest[i] = src1[i] ^ src2[i];
    }
}

void _hs_bitvec_andn(uint8_t *dest, const uint8_t *src1, const uint8_t *src2, HsInt len) {
    #pragma omp simd
    for (size_t i = 0; i < len; i++) {
        dest[i] = src1[i] & (~src2[i]);
    }
}

void _hs_bitvec_iorn(uint8_t *dest, const uint8_t *src1, const uint8_t *src2, HsInt len) {
    #pragma omp simd
    for (size_t i = 0; i < len; i++) {
        dest[i] = src1[i] | (~src2[i]);
    }
}

void _hs_bitvec_nand(uint8_t *dest, const uint8_t *src1, const uint8_t *src2, HsInt len) {
    #pragma omp simd
    for (size_t i = 0; i < len; i++) {
        dest[i] = ~(src1[i] & src2[i]);
    }
}

void _hs_bitvec_nior(uint8_t *dest, const uint8_t *src1, const uint8_t *src2, HsInt len) {
    #pragma omp simd
    for (size_t i = 0; i < len; i++) {
        dest[i] = ~(src1[i] | src2[i]);
    }
}

void _hs_bitvec_xnor(uint8_t *dest, const uint8_t *src1, const uint8_t *src2, HsInt len) {
    #pragma omp simd
    for (size_t i = 0; i < len; i++) {
        dest[i] = ~(src1[i] ^ src2[i]);
    }
}


#ifdef __x86_64__
static void reverse_bits_sse(uint64_t *dest, const uint64_t *src, HsInt len) {
    __m128i mask1l  = _mm_set1_epi32(0x55555555);
    __m128i mask1r  = _mm_set1_epi32(0xaaaaaaaa);
    __m128i mask2l  = _mm_set1_epi32(0x33333333);
    __m128i mask2r  = _mm_set1_epi32(0xcccccccc);
    __m128i mask4l  = _mm_set1_epi32(0x0f0f0f0f);
    __m128i mask4r  = _mm_set1_epi32(0xf0f0f0f0);
    __m128i mask8l  = _mm_set1_epi32(0x00ff00ff);
    __m128i mask8r  = _mm_set1_epi32(0xff00ff00);
    __m128i mask16l = _mm_set1_epi32(0x0000ffff);
    __m128i mask16r = _mm_set1_epi32(0xffff0000);

    size_t i = 0;
    for (; i < (len & (~0x1)); i += 2) {
        __m128i x = _mm_loadu_si128((const __m128i *) (src + i));

        // reverse each word
        x = _mm_or_si128(_mm_slli_epi32(_mm_and_si128(x, mask1l),   1), _mm_srli_epi32(_mm_and_si128(x, mask1r),   1));
        x = _mm_or_si128(_mm_slli_epi32(_mm_and_si128(x, mask2l),   2), _mm_srli_epi32(_mm_and_si128(x, mask2r),   2));
        x = _mm_or_si128(_mm_slli_epi32(_mm_and_si128(x, mask4l),   4), _mm_srli_epi32(_mm_and_si128(x, mask4r),   4));
        x = _mm_or_si128(_mm_slli_epi32(_mm_and_si128(x, mask8l),   8), _mm_srli_epi32(_mm_and_si128(x, mask8r),   8));
        x = _mm_or_si128(_mm_slli_epi32(_mm_and_si128(x, mask16l), 16), _mm_srli_epi32(_mm_and_si128(x, mask16r), 16));

        // reverse order of words
        x = _mm_shuffle_epi32(x, 0x1b);

        _mm_storeu_si128((__m128i *) (dest + len - 2 - i), x);
    }
    for (; i < len; i++) {
        uint64_t x = src[i];
        x = ((x & 0x5555555555555555) <<  1) | ((x & 0xaaaaaaaaaaaaaaaa) >>  1);
        x = ((x & 0x3333333333333333) <<  2) | ((x & 0xcccccccccccccccc) >>  2);
        x = ((x & 0x0f0f0f0f0f0f0f0f) <<  4) | ((x & 0xf0f0f0f0f0f0f0f0) >>  4);
        x = ((x & 0x00ff00ff00ff00ff) <<  8) | ((x & 0xff00ff00ff00ff00) >>  8);
        x = ((x & 0x0000ffff0000ffff) << 16) | ((x & 0xffff0000ffff0000) >> 16);
        x = ((x & 0x00000000ffffffff) << 32) | ((x & 0xffffffff00000000) >> 32);
        dest[len - 1 - i] = x;
    }
}

__attribute__((target("avx2")))
static void reverse_bits_avx(uint64_t *dest, const uint64_t *src, HsInt len) {
    __m256i mask1l  = _mm256_set1_epi32(0x55555555);
    __m256i mask1r  = _mm256_set1_epi32(0xaaaaaaaa);
    __m256i mask2l  = _mm256_set1_epi32(0x33333333);
    __m256i mask2r  = _mm256_set1_epi32(0xcccccccc);
    __m256i mask4l  = _mm256_set1_epi32(0x0f0f0f0f);
    __m256i mask4r  = _mm256_set1_epi32(0xf0f0f0f0);
    __m256i mask8l  = _mm256_set1_epi32(0x00ff00ff);
    __m256i mask8r  = _mm256_set1_epi32(0xff00ff00);
    __m256i mask16l = _mm256_set1_epi32(0x0000ffff);
    __m256i mask16r = _mm256_set1_epi32(0xffff0000);

    size_t i = 0;
    for (; i < (len & (~0x3)); i += 4) {
        __m256i x = _mm256_loadu_si256((const __m256i *) (src + i));

        // reverse each word
        x = _mm256_or_si256(_mm256_slli_epi32(_mm256_and_si256(x, mask1l),   1), _mm256_srli_epi32(_mm256_and_si256(x, mask1r),   1));
        x = _mm256_or_si256(_mm256_slli_epi32(_mm256_and_si256(x, mask2l),   2), _mm256_srli_epi32(_mm256_and_si256(x, mask2r),   2));
        x = _mm256_or_si256(_mm256_slli_epi32(_mm256_and_si256(x, mask4l),   4), _mm256_srli_epi32(_mm256_and_si256(x, mask4r),   4));
        x = _mm256_or_si256(_mm256_slli_epi32(_mm256_and_si256(x, mask8l),   8), _mm256_srli_epi32(_mm256_and_si256(x, mask8r),   8));
        x = _mm256_or_si256(_mm256_slli_epi32(_mm256_and_si256(x, mask16l), 16), _mm256_srli_epi32(_mm256_and_si256(x, mask16r), 16));

        // reverse order of words
        x = _mm256_permutevar8x32_epi32(x, _mm256_setr_epi32(7, 6, 5, 4, 3, 2, 1, 0));

        _mm256_storeu_si256((__m256i *) (dest + len - 4 - i), x);
    }
    for (; i < len; i++) {
        uint64_t x = src[i];
        x = ((x & 0x5555555555555555) <<  1) | ((x & 0xaaaaaaaaaaaaaaaa) >>  1);
        x = ((x & 0x3333333333333333) <<  2) | ((x & 0xcccccccccccccccc) >>  2);
        x = ((x & 0x0f0f0f0f0f0f0f0f) <<  4) | ((x & 0xf0f0f0f0f0f0f0f0) >>  4);
        x = ((x & 0x00ff00ff00ff00ff) <<  8) | ((x & 0xff00ff00ff00ff00) >>  8);
        x = ((x & 0x0000ffff0000ffff) << 16) | ((x & 0xffff0000ffff0000) >> 16);
        x = ((x & 0x00000000ffffffff) << 32) | ((x & 0xffffffff00000000) >> 32);
        dest[len - 1 - i] = x;
    }
}
#endif

void _hs_bitvec_reverse_bits(HsWord *dest, const HsWord *src, HsInt len) {
#ifdef __x86_64__
    if (__builtin_cpu_supports("avx2")) {
        reverse_bits_avx(dest, src, len);
    } else {
        reverse_bits_sse(dest, src, len);
    }
#else
    if (sizeof(HsWord) == 8) {
        // 64 bit
        for (size_t i = 0; i < len; i++) {
            uint64_t x = src[i];
            x = ((x & 0x5555555555555555) <<  1) | ((x & 0xaaaaaaaaaaaaaaaa) >>  1);
            x = ((x & 0x3333333333333333) <<  2) | ((x & 0xcccccccccccccccc) >>  2);
            x = ((x & 0x0f0f0f0f0f0f0f0f) <<  4) | ((x & 0xf0f0f0f0f0f0f0f0) >>  4);
            x = ((x & 0x00ff00ff00ff00ff) <<  8) | ((x & 0xff00ff00ff00ff00) >>  8);
            x = ((x & 0x0000ffff0000ffff) << 16) | ((x & 0xffff0000ffff0000) >> 16);
            x = ((x & 0x00000000ffffffff) << 32) | ((x & 0xffffffff00000000) >> 32);
            dest[len - 1 - i] = x;
        }
    } else {
        // 32 bit
        for (size_t i = 0; i < len; i++) {
            uint32_t x = src[i];
            x = ((x & 0x55555555) <<  1) | ((x & 0xaaaaaaaa) >>  1);
            x = ((x & 0x33333333) <<  2) | ((x & 0xcccccccc) >>  2);
            x = ((x & 0x0f0f0f0f) <<  4) | ((x & 0xf0f0f0f0) >>  4);
            x = ((x & 0x00ff00ff) <<  8) | ((x & 0xff00ff00) >>  8);
            x = ((x & 0x0000ffff) << 16) | ((x & 0xffff0000) >> 16);
            dest[len - 1 - i] = x;
        }
    }
#endif
}


#ifdef __x86_64__
static HsInt bit_index_sse(const uint64_t *src, HsInt len, HsBool bit) {
    __m128i zero = _mm_setzero_si128();
    __m128i bit_mask_128;
    uint64_t bit_mask_64;
    if (bit) {
        bit_mask_128 = zero;
        bit_mask_64 = 0;
    } else {
        bit_mask_128 = _mm_set1_epi64x(0xffffffffffffffff);
        bit_mask_64 = 0xffffffffffffffff;
    }
    size_t i = 0;
    for (; i < (len & (~0x1)); i += 2) {
        __m128i x = _mm_xor_si128(_mm_loadu_si128((const __m128i *) (src + i)), bit_mask_128);
        uint16_t mask = ~_mm_movemask_epi8(_mm_cmpeq_epi32(x, zero));
        if (mask != 0) {
            size_t idx = __builtin_ctz(mask) >> 3;
            uint64_t x = src[i + idx] ^ bit_mask_64;
            return ((i + idx) << 6) + __builtin_ctzll(x);
        }
    }
    for (; i < len; i++) {
        uint64_t x = src[i] ^ bit_mask_64;
        if (x != 0) {
            return (i << 6) + __builtin_ctzll(x);
        }
    }
    return -1;
}

__attribute__((target("avx2")))
static HsInt bit_index_avx(const uint64_t *src, HsInt len, HsBool bit) {
    __m256i zero = _mm256_setzero_si256();
    __m256i bit_mask_256;
    uint64_t bit_mask_64;
    if (bit) {
        bit_mask_256 = zero;
        bit_mask_64 = 0;
    } else {
        bit_mask_256 = _mm256_set1_epi64x(0xffffffffffffffff);
        bit_mask_64 = 0xffffffffffffffff;
    }
    size_t i = 0;
    for (; i < (len & (~0x3)); i += 4) {
        __m256i x = _mm256_xor_si256(_mm256_loadu_si256((const __m256i *) (src + i)), bit_mask_256);
        uint32_t mask = ~_mm256_movemask_epi8(_mm256_cmpeq_epi32(x, zero));
        if (mask != 0) {
            size_t idx = __builtin_ctzl(mask) >> 3;
            uint64_t x = src[i + idx] ^ bit_mask_64;
            return ((i + idx) << 6) + __builtin_ctzll(x);
        }
    }
    for (; i < len; i++) {
        uint64_t x = src[i] ^ bit_mask_64;
        if (x != 0) {
            return (i << 6) + __builtin_ctzll(x);
        }
    }
    return -1;
}
#endif

HsInt _hs_bitvec_bit_index(const HsWord *src, HsInt len, HsBool bit) {
#ifdef __x86_64__
    if (__builtin_cpu_supports("avx2")) {
        return bit_index_avx(src, len, bit);
    } else {
        return bit_index_sse(src, len, bit);
    }
#else
    HsWord bit_mask;
    if (bit) {
        bit_mask = 0;
    } else {
        bit_mask = -1;
    }
    for (size_t i = 0; i < len; i++) {
        HsWord x = src[i] ^ bit_mask;
        if (x != 0) {
            return (i << 3) * sizeof(HsWord) + __builtin_ctzll(x);
        }
    }
    return -1;
#endif
}


#ifdef __x86_64__
__attribute__((target("popcnt")))
static HsInt nth_bit_index_popcnt(const uint64_t *src, HsInt len, HsBool bit, HsInt n) {
    uint64_t bit_mask;
    if (bit) {
        bit_mask = 0;
    } else {
        bit_mask = -1;
    }
    for (size_t i = 0; i < len; i++) {
        uint64_t x = src[i] ^ bit_mask;

        HsInt count = _mm_popcnt_u64(x);
        if (n <= count) {
            for (size_t i = 0; i < n - 1; i++) {
                // clear lowest set bit
                x &= x - 1;
            }
            return (i << 6) + __builtin_ctzll(x);
        } else {
            n -= count;
        }
    }
    return -1;
}
#endif

HsInt _hs_bitvec_nth_bit_index(const HsWord *src, HsInt len, HsBool bit, HsInt n) {
#ifdef __x86_64__
    if (__builtin_cpu_supports("popcnt")) {
        return nth_bit_index_popcnt(src, len, bit, n);
    }
#endif
    HsWord bit_mask;
    if (bit) {
        bit_mask = 0;
    } else {
        bit_mask = -1;
    }
    for (size_t i = 0; i < len; i++) {
        HsWord x = src[i] ^ bit_mask;

        // popcount
        HsWord count = x - ((x >> 1) & 0x5555555555555555);
        count = (count & 0x3333333333333333) + ((count >> 2) & 0x3333333333333333);
        count = (count + (count >> 4)) & 0x0f0f0f0f0f0f0f0f;
        count = (count * 0x101010101010101) >> 56;

        if (n <= count) {
            for (size_t i = 0; i < n - 1; i++) {
                // clear lowest set bit
                x &= x - 1;
            }
            return (i << 3) * sizeof(HsWord) + __builtin_ctzll(x);
        } else {
            n -= count;
        }
    }
    return -1;
}


#ifdef __x86_64__
__attribute__((target("popcnt,bmi2")))
static HsInt select_bits_pext(uint64_t *dest, const uint64_t *src, const uint64_t *mask, HsInt len, HsBool exclude) {
    uint64_t bit_mask;
    if (exclude) {
        bit_mask = -1;
    } else {
        bit_mask = 0;
    }
    HsInt off = 0; // offset in bits into `dest`
    for (size_t i = 0; i < len; i++) {
        uint64_t x = src[i];
        uint64_t m = mask[i] ^ bit_mask;
        HsInt count = _mm_popcnt_u64(m);
        uint64_t y = _pext_u64(x, m);
        HsInt off_words = off >> 6;
        HsInt off_bits = off & 0x3f;
        if (off_bits == 0) {
            dest[off_words] = y;
        } else {
            dest[off_words] |= y << off_bits;
            dest[off_words + 1] = y >> (64 - off_bits);
        }
        off += count;
    }
    return off;
}
#endif

HsInt _hs_bitvec_select_bits(HsWord *dest, const HsWord *src, const HsWord *mask, HsInt len, HsBool exclude) {
#ifdef __x86_64__
    if (__builtin_cpu_supports("popcnt") && __builtin_cpu_supports("bmi2")) {
        return select_bits_pext(dest, src, mask, len, exclude);
    }
#endif
    HsWord bit_mask;
    if (exclude) {
        bit_mask = -1;
    } else {
        bit_mask = 0;
    }
    HsInt off = 0; // offset in bits into `dest`
    for (size_t i = 0; i < len; i++) {
        HsWord x = src[i];
        HsWord m = mask[i] ^ bit_mask;

        // pext
        HsWord y = 0;
        HsInt count = 0;
        if (m == -1) {
            y = x;
            count = sizeof(HsWord) * 8;
        } else {
            HsWord bb = 1;
            for (; m != 0; bb <<= 1) {
                if (x & m & -m) {
                    y |= bb;
                }
                m &= m - 1;
            }
            if (sizeof(HsWord) == 8) {
                count = __builtin_ctzll(bb);
            } else {
                count = __builtin_ctzl(bb);
            }
        }

        if (sizeof(HsWord) == 8) {
            // 64 bit
            HsInt off_words = off >> 6;
            HsInt off_bits = off & 0x3f;
            if (off_bits == 0) {
                dest[off_words] = y;
            } else {
                dest[off_words] |= y << off_bits;
                dest[off_words + 1] = y >> (64 - off_bits);
            }
            off += count;
        } else {
            // 32 bit
            HsInt off_words = off >> 5;
            HsInt off_bits = off & 0x1f;
            if (off_bits == 0) {
                dest[off_words] = y;
            } else {
                dest[off_words] |= y << off_bits;
                dest[off_words + 1] = y >> (32 - off_bits);
            }
            off += count;
        }
    }
    return off;
}
