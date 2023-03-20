# bitvec [![Hackage](https://img.shields.io/hackage/v/bitvec.svg)](https://hackage.haskell.org/package/bitvec) [![Stackage LTS](https://www.stackage.org/package/bitvec/badge/lts)](https://www.stackage.org/lts/package/bitvec) [![Stackage Nightly](https://www.stackage.org/package/bitvec/badge/nightly)](https://www.stackage.org/nightly/package/bitvec)

A newtype over `Bool` with a better `Vector` instance: 8x less memory, up to 1000x faster.

The [`vector`](https://hackage.haskell.org/package/vector)
package represents unboxed arrays of `Bool`s
spending 1 byte (8 bits) per boolean.
This library provides a newtype wrapper `Bit` and a custom instance
of an unboxed `Vector`, which packs bits densely,
achieving an __8x smaller memory footprint.__
The performance stays mostly the same;
the most significant degradation happens for random writes
(up to 10% slower).
On the other hand, for certain bulk bit operations
`Vector Bit` is up to 1000x faster than `Vector Bool`.

## Thread safety

* `Data.Bit` is faster, but writes and flips are thread-unsafe.
  This is because naive updates are not atomic:
  they read the whole word from memory,
  then modify a bit, then write the whole word back.
* `Data.Bit.ThreadSafe` is slower (usually 10-20%),
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

The `Bit`-based implementation requires 8x less memory to store
the vector. For large sizes it allows to crunch more data in RAM
without swapping. For smaller arrays it helps to fit into
CPU caches.

```haskell
> listBits eratosthenes
[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
```

There are several high-level helpers, digesting bits in bulk,
which makes them up to 64x faster than the respective counterparts
for `Vector Bool`. One can query the population count (popcount)
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
There is a moderate performance penalty (usually 10-20%)
for using the thread-safe interface.

## Sets

Bit vectors can be used as a blazingly fast representation of sets,
as long as their elements are `Enum`eratable and sufficiently dense,
leaving `IntSet` far behind.

For example, consider three possible representations of a set of `Word16`:

* As an `IntSet` with a readily available `union` function.
* As a 64k-long unboxed `Vector Bool`, implementing union as `zipWith (||)`.
* As a 64k-long unboxed `Vector Bit`, implementing union as `zipBits (.|.)`.

When the `libgmp` flag is enabled,
according to our benchmarks (see `bench` folder),
the union of `Vector Bit` evaluates 24x-36x faster
than the union of not-too-sparse `IntSet`s
and stunningly outperforms `Vector Bool` by 500x-1000x.

## Binary polynomials

Binary polynomials are polynomials with coefficients modulo 2.
Their applications include coding theory and cryptography.
While one can successfully implement them with the [`poly`](https://hackage.haskell.org/package/poly) package,
operating on `UPoly Bit`,
this package provides even faster arithmetic routines
exposed via the `F2Poly` data type and its instances.

```haskell
> :set -XBinaryLiterals
> -- (1 + x) * (1 + x + x^2) = 1 + x^3 (mod 2)
> 0b11 * 0b111 :: F2Poly
F2Poly {unF2Poly = [1,0,0,1]}
```

Use `fromInteger` / `toInteger` to convert binary polynomials
from `Integer` to `F2Poly` and back.

## Package flags

* Flag `libgmp`, disabled by default.

  Link against the [GMP](https://gmplib.org/) library for the ultimate performance of
  `zipBits`, `invertBits` and `countBits`. GMP is readily available on most machines
  ([`apt-get install libgmp-dev`](https://packages.ubuntu.com/focal/libgmp-dev) on Ubuntu,
  [`brew install gmp`](https://formulae.brew.sh/formula/gmp)
  or [`port install gmp`](https://ports.macports.org/port/gmp/summary) on macOS,
  [`pacman -S mingw-w64-x86_64-gmp`](https://packages.msys2.org/package/mingw-w64-x86_64-gmp) on MinGW),
  so users are strongly encouraged to enable it whenever possible.

## Similar packages

* [`bv`](https://hackage.haskell.org/package/bv) and
  [`bv-little`](https://hackage.haskell.org/package/bv-little)
  do not offer mutable vectors.

* [`array`](https://hackage.haskell.org/package/array)
  is memory-efficient for `Bool`, but lacks
  a handy `Vector` interface and is not thread-safe.

## Additional resources

* __Bit vectors without compromises__, Haskell Love, 31.07.2020:
  [slides](https://github.com/Bodigrim/my-talks/raw/master/haskelllove2020/slides.pdf), [video](https://youtu.be/HhpH8DKFBls).
