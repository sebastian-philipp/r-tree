## 1.1.0

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

  - `Data.RTree` is now called `Data.RTree.*.Strict`;

  - Internals are now exposed in `Data.RTree.*.Strict.Unsafe`;

  - `Binary`, `Generic`, `Monoid` and `Semigroup` instances were removed;

  - `insertWith`, `union`, `unionWith` and `mapMaybe`,
    `fromList`, `toList`, `keys` and `values` were removed;

  - `length` and `null` are now only accessible from the `Foldable` typeclass;

  - Conversions between lookup functions:

    - `lookup` is `\ba -> getFirst . foldMap (equals ba) (First . Just)`;

    - `intersect` is `\ba -> foldMap (intersects ba) pure`;

    - `intersectWithKey` is `\ba -> foldMapWithKey (intersects ba) pure`;

    - `lookupRange` is `\ba -> foldMap (contains ba) pure`;

    - `lookupRangeWithKey` is `\ba -> foldMapWithKey (contains ba) pure`;

    - `lookupContainsRange` is `\ba -> foldMap (containedBy ba) pure`;

    - `lookupContainsRangeWithKey` is `\ba -> foldMapWithKey (containedBy ba) pure`;

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
