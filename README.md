# bitvec [![Build Status](https://travis-ci.org/Bodigrim/bitvec.svg)](https://travis-ci.org/Bodigrim/bitvec) [![Hackage](http://img.shields.io/hackage/v/bitvec.svg)](https://hackage.haskell.org/package/bitvec)

Bit vectors library for Haskell.

The current `vector` package represents unboxed arrays of `Bool`
allocating one byte per boolean, which might be considered wasteful.
This library provides a newtype wrapper `Bit` and a custom instance
of unboxed `Vector`, which packs booleans densely.
It is a time-memory tradeoff: 8x less memory footprint
at the price of moderate performance penalty
(mostly, for random writes).

## Similar packages

* `bv` and `bv-little`
  offer only immutable size-polymorphic bit vectors.
  `bitvec` provides an interface to mutable vectors as well.

* `array` is memory-efficient for `Bool`, but lacks
  a handy 'Vector' interface.
