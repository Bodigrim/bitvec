# 1.1.0.0

* Fix a grave bug in `bitIndex`.
* Remove `integer-gmp` flag.

# 1.0.3.0

* Add `Bits (Vector Bit)` instance.
* Add `castFromWords8`, `castToWords8`, `cloneToWords8`
  to facilitate interoperation with `ByteString`.

# 1.0.2.0

* Fix out-of-bounds writes in mutable interface.
* Improve thread-safety of mutable interface.
* Add extended GCD for `F2Poly`.
* Change `Show` instance of `F2Poly`.

# 1.0.1.2

* Fix more bugs in `F2Poly` multiplication.

# 1.0.1.1

* Fix bugs in `F2Poly` multiplication.
* Performance improvements.

# 1.0.1.0

* Implement arithmetic of binary polynomials.
* Add `invertBits` and `reverseBits` functions.
* Add `Num`, `Real`, `Integral`, `Fractional` and `NFData` instances.
* Performance improvements.

# 1.0.0.1

* Performance improvements.

# 1.0.0.0

* Redesign API from the scratch.
* Add a thread-safe implementation.
* Add `nthBitIndex` function.

# 0.2.0.1

* Fix `Read` instance.

# 0.2.0.0

* Remove hand-written `Num`, `Real`, `Integral`, `Bits` instances.
* Derive `Bits` and `FiniteBits` instances.
* Expose `Bit` constructor directly and remove `fromBool` function.
* Rename `toBool` to `unBit`.

# 0.1.1.0

* Fix bugs in `MVector` and `Vector` instances of `Bit`.
* Speed up `MVector` and `Vector` instances of `Bit`.
