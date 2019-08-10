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

And vice-versa, query an address of the _n_-th set bit
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
