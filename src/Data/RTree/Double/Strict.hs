{- |
     Module     : Data.RTree.Double.Strict
     Copyright  : Copyright (c) 2015, Birte Wagner, Sebastian Philipp
                  Copyright (c) 2022, Oleksii Divak
     License    : MIT

     Maintainer : Oleksii Divak
     Stability  : experimental
     Portability: not portable

     @'RTree' a@ represents a two-dimensional spatial tree
     from bounding rectangles of type 'Double' to values of type @a@.

     R-trees have no notion of element order, as such:

     - Duplicate t'MBR's are permitted. Inserting a duplicate may put it anywhere on the
       tree, there is no guarantee a successive 'delete' will pick the newer entry
       over the older one.

     - Updating an t'MBR' of an entry requires a reinsertion of said entry.

     - Merge operations are not supported.

     == Laziness

     Evaluating the root of the tree (i.e. @(_ :: 'RTree' a)@) to WHNF evaluates the entire
     spine of the tree to normal form. This does not apply to values however, as such
     care must be taken to evaluate values properly before inserting them.

     == Performance

     Unless noted otherwise, operation complexity specified is worst-case.
     \(n\) refers to the total number of entries in the tree.

     == Implementation

     The implementation is heavily specialized for constants
     \(m = 2, M = 4, p = 1, k = 1\).

     Descriptions of the R-/R*-tree and of the algorithms implemented can be found within
     the following papers:

       * Antonin Guttman (1984),
         \"/R-Trees: A Dynamic Index Structure for Spatial Searching/\",
         <http://www-db.deis.unibo.it/courses/SI-LS/papers/Gut84.pdf>

       * N. Beckmann, H.P. Kriegel, R. Schneider, B. Seeger (1990),
         \"/The R*-tree: an efficient and robust access method for points and rectangles/\",
         <https://infolab.usc.edu/csci599/Fall2001/paper/rstar-tree.pdf>

       * S.T. Leutenegger, J.M. Edgington, M.A. Lopez (1997),
         \"/STR: A Simple and Efficient Algorithm for R-Tree Packing/\",
         <https://ia800900.us.archive.org/27/items/nasa_techdoc_19970016975/19970016975.pdf>
-}

module Data.RTree.Double.Strict
  ( MBR (MBR)
  , RTree

    -- * Construction
  , empty
  , singleton

    -- * Queries
    -- ** Value-only
    -- *** Map
  , Data.RTree.Double.Strict.Internal.map
  , map'

    -- *** Fold
  , Data.RTree.Double.Strict.Internal.foldl
  , Data.RTree.Double.Strict.Internal.foldr
  , Data.RTree.Double.Strict.Internal.foldMap
  , Data.RTree.Double.Strict.Internal.foldl'
  , Data.RTree.Double.Strict.Internal.foldr'

    -- *** Traversal
  , Data.RTree.Double.Strict.Internal.traverse

    -- ** MBR/value
    -- *** Map
  , mapWithKey
  , mapWithKey'

    -- *** Fold
  , foldlWithKey
  , foldrWithKey
  , foldMapWithKey
  , foldlWithKey'
  , foldrWithKey'

    -- *** Traversal
  , traverseWithKey

    -- ** Range
  , Predicate
  , equals
  , intersects
  , intersects'
  , contains
  , contains'
  , containedBy
  , containedBy'

    -- *** Map
  , mapRangeWithKey
  , mapRangeWithKey'

    -- *** Fold
  , foldlRangeWithKey
  , foldrRangeWithKey
  , foldMapRangeWithKey
  , foldlRangeWithKey'
  , foldrRangeWithKey'

    -- *** Traversal
  , traverseRangeWithKey

    -- * Insertion
  , insert
  , insertGut

    -- * Deletion
  , delete

    -- * Bulk-loading
  , bulkSTR
  ) where

import           Data.RTree.Double.Strict.Internal



-- | \(\mathcal{O}(1)\). Empty R-tree.
empty :: RTree a
empty = Empty

-- | \(\mathcal{O}(1)\). R-tree with a single entry.
singleton :: MBR -> a -> RTree a
singleton = Leaf1
