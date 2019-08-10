# bitvec [![Build Status](https://travis-ci.org/Bodigrim/bitvec.svg)](https://travis-ci.org/Bodigrim/bitvec) [![Hackage](http://img.shields.io/hackage/v/bitvec.svg)](https://hackage.haskell.org/package/bitvec) [![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/bitvec/badge)](https://matrix.hackage.haskell.org/package/bitvec) [![Stackage LTS](http://stackage.org/package/bitvec/badge/lts)](http://stackage.org/lts/package/bitvec) [![Stackage Nightly](http://stackage.org/package/bitvec/badge/nightly)](http://stackage.org/nightly/package/bitvec)

A newtype over `Bool` with a better `Vector` instance.

The [`vector`](https://hackage.haskell.org/package/vector)
package represents unboxed arrays of `Bool`
spending 1 byte (8 bits) per boolean.
This library provides a newtype wrapper `Bit` and a custom instance
of unboxed `Vector`, which packs bits densely,
achieving __8x less memory footprint.__
The performance stays mostly the same;
the most significant degradation happens for random writes
(up to 10% slower).
On the other hand, for certain bulk bit operations
`Vector Bit` is up to 64x faster than `Vector Bool`.

## Thread safety

* `Data.Bit` is faster, but writes and flips are thread-unsafe.
  This is because naive updates are not atomic:
  read the whole word from memory,
  then modify a bit, then write the whole word back.
* `Data.Bit.ThreadSafe` is slower (up to 20%),
  but writes and flips are thread-safe.

## Similar packages

* [`bv`](https://hackage.haskell.org/package/bv) and
  [`bv-little`](https://hackage.haskell.org/package/bv-little)
  do not offer mutable vectors.

* [`array`](https://hackage.haskell.org/package/array)
  is memory-efficient for `Bool`, but lacks
  a handy `Vector` interface and is not thread-safe.
