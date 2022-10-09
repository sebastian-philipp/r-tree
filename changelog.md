## 1.0.0

Proper rewrite of the library.
Additions are not listed, only the most important changes are.

* Cabal file changes:

  - Bumped lower boundary of `base` to `4.12`;

  - No longer depends on `binary`;

  - Now depends on `primitive`.

* `MBB`:

  - Now called `MBR` and resides within `Data.RTree.MBR`;

  - Now has a single type argument, the coordinate type, further referred to as `r`;

  - No longer has `Binary`, `Ord` and `Generic` instances;

  - `mbb` and `isPointMBB` were removed;

  - Functions within `Data.RTree.MBR` no longer have the `MBB` prefix;

* Both `RTree` interfaces:

  - `Data.RTree` is now called `Data.RTree.Lazy`;

  - `Data.RTree.Base` is now called `Data.RTree.Internal`;

  - `RTree` now has two type arguments, in the following order: `RTree r a`;

  - `Binary`, `Generic`, `Monoid` and `Semigroup` instances were removed;

  - `insertWith`, `union`, `unionWith` and `mapMaybe` were removed;

  - `keys` is now called `boxes`.

  - Conversions between lookup functions:

    - `lookup` is `\ba -> getFirst . foldMap (equals ba) (First . Just)`;

    - `intersect` is `\ba -> foldMap (intersects ba) pure`;

    - `intersectWithKey` is `\ba -> foldMapWithKey (intersects ba) pure`;

    - `lookupRange` is `\ba -> foldMap (contains ba) pure`;

    - `lookupRangeWithKey` is `\ba -> foldMapWithKey (contains ba) pure`;

    - `lookupContainsRange` is `\ba -> foldMap (within ba) pure`;

    - `lookupContainsRangeWithKey` is `\ba -> foldMapWithKey (within ba) pure`;

* Strict `RTree` interface:

  - Now shares the underlying `RTree` datatype with the lazy interface;

  - The strictness guarantees have been weakened to WHNF value evaluation on full
    tree evaluation.

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
