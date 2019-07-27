# bitvec [![Build Status](https://travis-ci.org/Bodigrim/bitvec.svg)](https://travis-ci.org/Bodigrim/bitvec) [![Hackage](http://img.shields.io/hackage/v/bitvec.svg)](https://hackage.haskell.org/package/bitvec) [![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/bitvec/badge)](https://matrix.hackage.haskell.org/package/bitvec) [![Stackage LTS](http://stackage.org/package/bitvec/badge/lts)](http://stackage.org/lts/package/bitvec) [![Stackage Nightly](http://stackage.org/package/bitvec/badge/nightly)](http://stackage.org/nightly/package/bitvec)

Bit vectors library for Haskell.

The current `vector` package represents unboxed arrays of `Bool`
allocating one byte per boolean, which might be considered wasteful.
This library provides a newtype wrapper `Bit` and a custom instance
of unboxed `Vector`, which packs booleans densely.
It is a time-memory tradeoff: 8x less memory footprint
at the price of moderate performance penalty
(mostly, for random writes).

## Thread safety

* `Data.Bit` is faster, but thread-unsafe. This is because
  naive updates are not atomic operations: read the whole word from memory,
  modify a bit, write the whole word back.
* `Data.Bit.ThreadSafe` is slower (up to 2x), but thread-safe.
  It implements updates via `fetch{And,Or,Xor}IntArray#` primitives
  from `GHC.Exts`.

## Similar packages

* `bv` and `bv-little`
  offer only immutable size-polymorphic bit vectors.
  `bitvec` provides an interface to mutable vectors as well.

* `array` is memory-efficient for `Bool`, but lacks
  a handy `Vector` interface and is not thread-safe.
