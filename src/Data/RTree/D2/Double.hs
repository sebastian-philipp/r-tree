{-# LANGUAGE PatternSynonyms #-}

{- |
     Module     : Data.RTree.D2.Double
     Copyright  : Copyright (c) 2015, Birte Wagner, Sebastian Philipp
                  Copyright (c) 2022, Oleksii Divak
     License    : MIT

     Maintainer : Oleksii Divak
     Stability  : experimental
     Portability: not portable

     @'RTree' a@ is a spine-strict two-dimensional spatial tree
     from bounding rectangles of type 'Double' to values of type @a@.

     R-trees have no notion of element order, as such:

     - Duplicate t'MBR's are permitted. Inserting a duplicate may put it anywhere on the
       tree, there is no guarantee a successive 'delete' will pick the newer entry
       over the older one.

     - Updating an t'MBR' of an entry requires a reinsertion of said entry.

     - Merge operations are not supported.

     == Laziness

     Evaluating the root of the tree (i.e. @(_ :: 'RTree' a)@) to WHNF
     evaluates the entire spine of the tree to normal form.

     Functions do not perform any additional evaluations unless
     their documentation directly specifies so.

     == Performance

     Each function's time complexity is provided in the documentation.
     
     \(n\) refers to the total number of entries in the tree.

     \(r\) refers to the time complexity of the chosen 'Predicate' lookup,
     ranging from \(\mathcal{O}(\log n)\) (well-balanced)
     to \(\mathcal{O}(n)\) (worst-case) depending on tree quality.

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

module Data.RTree.D2.Double
  ( MBR (MBR)
  , RTree

    -- * Construct
  , empty
  , singleton

    -- ** Bulk-loading
  , bulkSTR

    -- * Single-key
    -- ** Insert
  , insert
  , insertGut

    -- ** Delete
  , delete

    -- * Range
    -- | NOTE: both 'Predicate's and functions using them inline heavily.
  , Predicate
  , equals
  , intersects
  , intersects'
  , contains
  , contains'
  , containedBy
  , containedBy'

    -- ** Map
  , adjustRangeWithKey
  , adjustRangeWithKey'

    -- ** Fold
  , foldlRangeWithKey
  , foldrRangeWithKey
  , foldMapRangeWithKey
  , foldlRangeWithKey'
  , foldrRangeWithKey'

    -- ** Traverse
  , traverseRangeWithKey

    -- * Full tree
    -- ** Size
  , Data.RTree.D2.Double.Internal.null
  , size

    -- ** Map
  , Data.RTree.D2.Double.Internal.map
  , map'
  , mapWithKey
  , mapWithKey'

    -- ** Fold
    -- | === Left-to-right
  , Data.RTree.D2.Double.Internal.foldl
  , Data.RTree.D2.Double.Internal.foldl'
  , foldlWithKey
  , foldlWithKey'

    -- | === Right-to-left
  , Data.RTree.D2.Double.Internal.foldr
  , Data.RTree.D2.Double.Internal.foldr'
  , foldrWithKey
  , foldrWithKey'

    -- | === Monoid
  , Data.RTree.D2.Double.Internal.foldMap
  , foldMapWithKey

    -- ** Traverse
  , Data.RTree.D2.Double.Internal.traverse
  , traverseWithKey
  ) where

import           Data.RTree.D2.Double.Internal



-- | \(\mathcal{O}(1)\).
--   Empty tree.
empty :: RTree a
empty = Empty

-- | \(\mathcal{O}(1)\).
--   Tree with a single entry.
singleton :: MBR -> a -> RTree a
singleton = Leaf1
