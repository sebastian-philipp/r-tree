## 1.2.0

Proper rewrite of the library.
Additions are not listed, only the most important changes are.

* Cabal file changes:

  - Bumped lower boundary of `base` to `4.12`;

  - No longer depends on `binary`;

* `MBB`:

  - Now called `MBR`;

  - No longer has `Binary`, `Ord` and `Generic` instances;

  - All relevant functions have been moved to `.Unsafe` modules;

* `RTree`:

  - `Data.RTree` is now called `Data.RTree.D2.Double`;

  - Internals are now exposed in `Data.RTree.D2.Double.Unsafe`;

  - `Binary`, `Generic`, `Monoid` and `Semigroup` instances were removed;

  - `insertWith`, `union`, `unionWith` and `mapMaybe`,
    `fromList`, `toList`, `keys` and `values` were removed;

  - `length` is now named `size`;

  - Conversions between lookup functions:

    - `lookup` is `\ba -> foldrRangeWithKey (equals ba) (\_ x _ -> Just x) Nothing`;

    - `intersect` is `\ba -> foldrRangeWithKey (intersects ba) (\_ -> (:)) []`;

    - `intersectWithKey` is
      `\ba -> foldrRangeWithKey (intersects ba) (\bx x -> (:) (bx, x)) []`;

    - `lookupRange` is `\ba -> foldrRangeWithKey (containedBy ba) (\_ -> (:)) []`;

    - `lookupRangeWithKey` is
      `\ba -> foldrRangeWithKey (containedBy ba) (\bx x -> (:) (bx, x)) []`;

    - `lookupContainsRange` is `\ba -> foldrRangeWithKey (contains ba) (\_ -> (:)) []`;

    - `lookupContainsRangeWithKey` is
      `\ba -> foldrRangeWithKey (contains ba) (\bx x -> (:) (bx, x)) []`;

## 0.6.0

* Add `lookupContainsRange` and `lookupContainsRangeWithKey`.
* Add `intersectWithKey` and `intersect`.
* Now supports GHC 8.4, 8.5 and 8.6.
* Removed `test-strict` flag.
* Minimal Bounding Box is now also an instance of `Ord`

## 0.0.5.0

* changed the Functor instance of Data.RTree.Strict to be strict

* Data.RTree.Strict.RTree is now a newtype of Data.RTree.RTree

## 0.0.4.0

* Added Data.RTree.Strict

* Added Data.Binary interface for GHC 7.6
