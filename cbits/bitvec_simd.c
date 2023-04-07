#include <inttypes.h>
#include <stddef.h>

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
