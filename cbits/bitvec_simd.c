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
static void reverse_bits_sse(uint32_t *dest, const uint32_t *src, HsInt len) {
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
    for (; i < (len & (~0x3)); i += 4) {
        __m128i x = _mm_loadu_si128((const __m128i *) (src + i));

        // reverse each word
        x = _mm_or_si128(_mm_slli_epi32(_mm_and_si128(x, mask1l),   1), _mm_srli_epi32(_mm_and_si128(x, mask1r),   1));
        x = _mm_or_si128(_mm_slli_epi32(_mm_and_si128(x, mask2l),   2), _mm_srli_epi32(_mm_and_si128(x, mask2r),   2));
        x = _mm_or_si128(_mm_slli_epi32(_mm_and_si128(x, mask4l),   4), _mm_srli_epi32(_mm_and_si128(x, mask4r),   4));
        x = _mm_or_si128(_mm_slli_epi32(_mm_and_si128(x, mask8l),   8), _mm_srli_epi32(_mm_and_si128(x, mask8r),   8));
        x = _mm_or_si128(_mm_slli_epi32(_mm_and_si128(x, mask16l), 16), _mm_srli_epi32(_mm_and_si128(x, mask16r), 16));

        // reverse order of words
        x = _mm_shuffle_epi32(x, 0x1b);

        _mm_storeu_si128((__m128i *) (dest + len - 4 - i), x);
    }
    for (; i < len; i++) {
        uint32_t x = src[i];
        x = ((x & 0x55555555) <<  1) | ((x & 0xaaaaaaaa) >>  1);
        x = ((x & 0x33333333) <<  2) | ((x & 0xcccccccc) >>  2);
        x = ((x & 0x0f0f0f0f) <<  4) | ((x & 0xf0f0f0f0) >>  4);
        x = ((x & 0x00ff00ff) <<  8) | ((x & 0xff00ff00) >>  8);
        x = ((x & 0x0000ffff) << 16) | ((x & 0xffff0000) >> 16);
        dest[len - 1 - i] = x;
    }
}

__attribute__((target("avx2")))
static void reverse_bits_avx(uint32_t *dest, const uint32_t *src, HsInt len) {
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
    for (; i < (len & (~0x7)); i += 8) {
        __m256i x = _mm256_loadu_si256((const __m256i *) (src + i));

        // reverse each word
        x = _mm256_or_si256(_mm256_slli_epi32(_mm256_and_si256(x, mask1l),   1), _mm256_srli_epi32(_mm256_and_si256(x, mask1r),   1));
        x = _mm256_or_si256(_mm256_slli_epi32(_mm256_and_si256(x, mask2l),   2), _mm256_srli_epi32(_mm256_and_si256(x, mask2r),   2));
        x = _mm256_or_si256(_mm256_slli_epi32(_mm256_and_si256(x, mask4l),   4), _mm256_srli_epi32(_mm256_and_si256(x, mask4r),   4));
        x = _mm256_or_si256(_mm256_slli_epi32(_mm256_and_si256(x, mask8l),   8), _mm256_srli_epi32(_mm256_and_si256(x, mask8r),   8));
        x = _mm256_or_si256(_mm256_slli_epi32(_mm256_and_si256(x, mask16l), 16), _mm256_srli_epi32(_mm256_and_si256(x, mask16r), 16));

        // reverse order of words
        x = _mm256_permutevar8x32_epi32(x, _mm256_setr_epi32(7, 6, 5, 4, 3, 2, 1, 0));

        _mm256_storeu_si256((__m256i *) (dest + len - 8 - i), x);
    }
    for (; i < len; i++) {
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

void _hs_bitvec_reverse_bits(uint32_t *dest, const uint32_t *src, HsInt len) {
#ifdef __x86_64__
    if (__builtin_cpu_supports("avx2")) {
        reverse_bits_avx(dest, src, len);
    } else {
        reverse_bits_sse(dest, src, len);
    }
#else
    for (size_t i = 0; i < len; i++) {
        uint32_t x = src[i];
        x = ((x & 0x55555555) <<  1) | ((x & 0xaaaaaaaa) >>  1);
        x = ((x & 0x33333333) <<  2) | ((x & 0xcccccccc) >>  2);
        x = ((x & 0x0f0f0f0f) <<  4) | ((x & 0xf0f0f0f0) >>  4);
        x = ((x & 0x00ff00ff) <<  8) | ((x & 0xff00ff00) >>  8);
        x = ((x & 0x0000ffff) << 16) | ((x & 0xffff0000) >> 16);
        dest[len - 1 - i] = x;
    }
#endif
}
