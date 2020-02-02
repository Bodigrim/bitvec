# bitvec [![Build Status](https://travis-ci.org/Bodigrim/bitvec.svg)](https://travis-ci.org/Bodigrim/bitvec) [![Hackage](http://img.shields.io/hackage/v/bitvec.svg)](https://hackage.haskell.org/package/bitvec) [![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/bitvec/badge)](https://matrix.hackage.haskell.org/package/bitvec) [![Stackage LTS](http://stackage.org/package/bitvec/badge/lts)](http://stackage.org/lts/package/bitvec) [![Stackage Nightly](http://stackage.org/package/bitvec/badge/nightly)](http://stackage.org/nightly/package/bitvec)

A newtype over `Bool` with a better `Vector` instance: 8x less memory, up to 1000x faster.

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
`Vector Bit` is up to 1000x faster than `Vector Bool`.

## Thread safety

* `Data.Bit` is faster, but writes and flips are thread-unsafe.
  This is because naive updates are not atomic:
  read the whole word from memory,
  then modify a bit, then write the whole word back.
* `Data.Bit.ThreadSafe` is slower (up to 20%),
  but writes and flips are thread-safe.

## Quick start

Consider the following (very naive) implementation of
[the sieve of Eratosthenes](https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes). It returns a vector with `True`
at prime indices and `False` at composite indices.

```haskell
import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

eratosthenes :: U.Vector Bool
eratosthenes = runST $ do
  let len = 100
  sieve <- MU.replicate len True
  MU.write sieve 0 False
  MU.write sieve 1 False
  forM_ [2 .. floor (sqrt (fromIntegral len))] $ \p -> do
    isPrime <- MU.read sieve p
    when isPrime $
      forM_ [2 * p, 3 * p .. len - 1] $ \i ->
        MU.write sieve i False
  U.unsafeFreeze sieve
```

We can switch from `Bool` to `Bit` just by adding newtype constructors:

```haskell
import Data.Bit

import Control.Monad
import Control.Monad.ST
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

eratosthenes :: U.Vector Bit
eratosthenes = runST $ do
  let len = 100
  sieve <- MU.replicate len (Bit True)
  MU.write sieve 0 (Bit False)
  MU.write sieve 1 (Bit False)
  forM_ [2 .. floor (sqrt (fromIntegral len))] $ \p -> do
    Bit isPrime <- MU.read sieve p
    when isPrime $
      forM_ [2 * p, 3 * p .. len - 1] $ \i ->
        MU.write sieve i (Bit False)
  U.unsafeFreeze sieve
```

`Bit`-based implementation requires 8x less memory to store
the vector. For large sizes it allows to crunch more data in RAM
without swapping. For smaller arrays it helps to fit into
CPU caches.

```haskell
> listBits eratosthenes
[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
```

There are several high-level helpers, digesting bits in bulk,
which makes them up to 64x faster than respective counterparts
for `Vector Bool`. One can query population count (popcount)
of a vector (giving us [the prime-counting function](https://en.wikipedia.org/wiki/Prime-counting_function)):

```haskell
> countBits eratosthenes
25
```

And vice versa, query an address of the _n_-th set bit
(which corresponds to the _n_-th prime number here):

```haskell
> nthBitIndex (Bit True) 10 eratosthenes
Just 29
```

One may notice that the order of the inner traversal by `i`
does not matter and get tempted to run it in several parallel threads.
In this case it is vital to switch from `Data.Bit` to `Data.Bit.ThreadSafe`,
because the former is thread-unsafe with regards to writes.
There is a moderate performance penalty (up to 20%)
for using the thread-safe interface.

## Sets

Bit vectors can be used as a blazingly fast representation of sets
as long as their elements are `Enum`eratable and sufficiently dense,
leaving `IntSet` far behind.

For example, consider three possible representations of a set of `Word16`:

* As an `IntSet` with a readily available `union` function.
* As a 64k-long unboxed `Vector Bool`, implementing union as `zipWith (||)`.
* As a 64k-long unboxed `Vector Bit`, implementing union as `zipBits (.|.)`.

In our benchmarks (see `bench` folder) for not-too-sparse sets
the union of `Vector Bit` evaluates 24x-36x faster than the union of `IntSet`
and stunningly outperforms `Vector Bool` 500x-1000x.

## Binary polynomials

Binary polynomials are polynomials with coefficients modulo 2.
Their applications include coding theory and cryptography.
While one can successfully implement them with `poly` package,
operating on `UPoly Bit`,
this package provides even faster arithmetic routines
exposed via `F2Poly` data type and its instances.

```haskell
> :set -XBinaryLiterals
> -- (1 + x) (1 + x + x^2) = 1 + x^3 (mod 2)
> 0b11 * 0b111 :: F2Poly
F2Poly {unF2Poly = [1,0,0,1]}
```

Use `fromInteger` / `toInteger` to convert binary polynomials
from `Integer` to `F2Poly` and back.

## Package flags

This package supports the following flags to facilitate dependency management.
Disabling them does not diminish `bitvec`'s capabilities, but makes certain operations slower.

* Flag `integer-gmp`, enabled by default.

  Depend on `integer-gmp` package and use it to speed up operations on binary polynomials.
  Normally `integer-gmp` is shipped with core libraries anyways, so there is little to gain
  from disabling it, unless you use a custom build of GHC.

* Flag `libgmp`, enabled by default.

  Link against [GMP](https://gmplib.org/) library and use it to for ultimate performance of
  `zipBits`, `invertBits` and `countBits`. GMP is readily available on most machines
  (`brew install gmp` on macOS), but you may find useful to disable this flag working
  with exotic setup.

## Similar packages

* [`bv`](https://hackage.haskell.org/package/bv) and
  [`bv-little`](https://hackage.haskell.org/package/bv-little)
  do not offer mutable vectors.

* [`array`](https://hackage.haskell.org/package/array)
  is memory-efficient for `Bool`, but lacks
  a handy `Vector` interface and is not thread-safe.
