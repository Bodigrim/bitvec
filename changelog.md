# 0.3.0.0

* Change arguments of 'unions' and 'intersections'.
* 'fromWords' and 'cloneFromWords' no longer take a number of bits.
* Remove 'pad' and 'padWith'.
* Remove 'any', 'anyBits', 'all', 'allBits', 'findIndex'.
* Remove 'mapMInPlaceWithIndex', 'mapInPlaceWithIndex', 'mapMInPlace', 'mapInPlace'.
* Remove 'wordSize' and 'wordLength'.
* Remove 'cloneFromWords', introduce 'castFromWords' and 'castToWords' instead.
* Remove 'unionInPlace', 'intersectionInPlace', 'differenceInPlace', 'symDiffInPlace'.
* Remove 'countBits', 'listBits', 'and', 'or' for mutable vectors.
* Remove 'readWord' and 'writeWord'.

# 0.2.0.1

* Fix 'Read' instance.

# 0.2.0.0

* Remove hand-written 'Num', 'Real', 'Integral', 'Bits' instances.
* Derive 'Bits' and 'FiniteBits' instances.
* Expose 'Bit' constructor directly and remove 'fromBool' function.
* Rename 'toBool' to 'unBit'.

# 0.1.1.0

* Fix bugs in `MVector` and `Vector` instances of `Bit`.
* Speed up `MVector` and `Vector` instances of `Bit`.
