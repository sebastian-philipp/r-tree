{-# LANGUAGE PatternSynonyms #-}

{- |
     Module     : Data.R2Tree.Double
     Copyright  : Copyright (c) 2015, Birte Wagner, Sebastian Philipp
                  Copyright (c) 2022, Oleksii Divak
     License    : MIT

     Maintainer : Oleksii Divak
     Stability  : experimental
     Portability: not portable

     @'R2Tree' a@ is a spine-strict two-dimensional spatial tree using 'Double's as keys.

     R-trees have no notion of element order, as such:

     - Duplicate t'MBR's are permitted. Inserting a duplicate may put it anywhere on the
       tree, there is no guarantee a successive 'delete' will pick the newer entry
       over the older one.

     - Updating an t'MBR' of an entry requires a reinsertion of said entry.

     - Merge operations are not supported.

     == Laziness

     Evaluating the root of the tree (i.e. @(_ :: 'R2Tree' a)@) to WHNF
     evaluates the entire spine of the tree to normal form.

     Functions do not perform any additional evaluations unless
     their documentation directly specifies so.

     == Performance

     Each function's time complexity is provided in the documentation.

     \(n\) refers to the total number of entries in the tree.
     Parts of the tree are denoted using subscripts: \(n_L\) refers to the left side,
     \(n_R\) to the right side, \(n_I\) to a range (interval), and
     \(n_M\) to entries collected with the use of a 'Monoid'.

     == Inlining

     Functions that produce and consume 'Predicate's inline heavily.
     To avoid unnecessary code duplication during compilation consider creating
     helper functions that apply these functions one to another, e.g.

@
listIntersections :: 'MBR' -> 'R2Tree' a -> [('MBR', a)]
listIntersections mbr = foldrRangeWithKey (intersects mbr) (\a b -> (:) (a, b)) []
@

     N.B. To inline properly functions that consume 'Predicate's
     must mention all of the arguments except for the tree.

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

module Data.R2Tree.Double
  ( MBR (MBR)
  , R2Tree

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
  , Data.R2Tree.Double.Internal.null
  , size

    -- ** Map
  , Data.R2Tree.Double.Internal.map
  , map'
  , mapWithKey
  , mapWithKey'

    -- ** Fold
    -- | === Left-to-right
  , Data.R2Tree.Double.Internal.foldl
  , Data.R2Tree.Double.Internal.foldl'
  , foldlWithKey
  , foldlWithKey'

    -- | === Right-to-left
  , Data.R2Tree.Double.Internal.foldr
  , Data.R2Tree.Double.Internal.foldr'
  , foldrWithKey
  , foldrWithKey'

    -- | === Monoid
  , Data.R2Tree.Double.Internal.foldMap
  , foldMapWithKey

    -- ** Traverse
  , Data.R2Tree.Double.Internal.traverse
  , traverseWithKey
  ) where

import           Data.R2Tree.Double.Internal



-- | \(\mathcal{O}(1)\).
--   Empty tree.
empty :: R2Tree a
empty = Empty

-- | \(\mathcal{O}(1)\).
--   Tree with a single entry.
singleton :: MBR -> a -> R2Tree a
singleton = Leaf1
