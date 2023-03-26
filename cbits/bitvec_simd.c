#include <inttypes.h>
#include <stddef.h>

void _hs_bitvec_com(uint8_t *dest, uint8_t *src, size_t len) {
    #pragma omp simd
    for (size_t i = 0; i < len; i++) {
        dest[i] = ~src[i];
    }
}

void _hs_bitvec_and(uint8_t *dest, const uint8_t *src1, const uint8_t *src2, size_t len) {
    #pragma omp simd
    for (size_t i = 0; i < len; i++) {
        dest[i] = src1[i] & src2[i];
    }
}

void _hs_bitvec_ior(uint8_t *dest, const uint8_t *src1, const uint8_t *src2, size_t len) {
    #pragma omp simd
    for (size_t i = 0; i < len; i++) {
        dest[i] = src1[i] | src2[i];
    }
}

void _hs_bitvec_xor(uint8_t *dest, const uint8_t *src1, const uint8_t *src2, size_t len) {
    #pragma omp simd
    for (size_t i = 0; i < len; i++) {
        dest[i] = src1[i] ^ src2[i];
    }
}

void _hs_bitvec_andn(uint8_t *dest, const uint8_t *src1, const uint8_t *src2, size_t len) {
    #pragma omp simd
    for (size_t i = 0; i < len; i++) {
        dest[i] = src1[i] & (~src2[i]);
    }
}

void _hs_bitvec_iorn(uint8_t *dest, const uint8_t *src1, const uint8_t *src2, size_t len) {
    #pragma omp simd
    for (size_t i = 0; i < len; i++) {
        dest[i] = src1[i] | (~src2[i]);
    }
}

void _hs_bitvec_nand(uint8_t *dest, const uint8_t *src1, const uint8_t *src2, size_t len) {
    #pragma omp simd
    for (size_t i = 0; i < len; i++) {
        dest[i] = ~(src1[i] & src2[i]);
    }
}

void _hs_bitvec_nior(uint8_t *dest, const uint8_t *src1, const uint8_t *src2, size_t len) {
    #pragma omp simd
    for (size_t i = 0; i < len; i++) {
        dest[i] = ~(src1[i] | src2[i]);
    }
}

void _hs_bitvec_xnor(uint8_t *dest, const uint8_t *src1, const uint8_t *src2, size_t len) {
    #pragma omp simd
    for (size_t i = 0; i < len; i++) {
        dest[i] = ~(src1[i] ^ src2[i]);
    }
}
