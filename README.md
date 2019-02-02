bitvec [![Build Status](https://travis-ci.org/Bodigrim/bitvec.svg)](https://travis-ci.org/Bodigrim/bitvec)
======

Bit vectors library for Haskell.

The current `vector` package represents unboxed arrays of `Bool`
allocating one byte per boolean, which might be considered wasteful.
This library provides a newtype wrapper `Bit` and a custom instance
of unboxed `Vector`, which packs booleans densely.
It is a time-memory tradeoff: 8x less memory footprint
at the price of moderate performance penalty
(mostly, for random writes).
