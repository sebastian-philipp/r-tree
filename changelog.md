## 0.0.4.0

* Added Data.RTree.Strict

* Added Data.Binary interface for GHC 7.6

## 0.0.5.0

* changed the Functor instance of Data.RTree.Strict to be strict

* Data.RTree.Strict.RTree is now a newtype of Data.RTree.RTree

## 0.6.0

* Add `lookupContainsRange` and `lookupContainsRangeWithKey`.
* Add `intersectWithKey` and `intersect`.
* Now supports GHC 8.4, 8.5 and 8.6.
* Removed `test-strict` flag.
* Minimal Bounding Box is now also an instance of `Ord`


## 0.6.1

* Add `lookupTouchesRangeWithKey` and `lookupTouchesRange`.