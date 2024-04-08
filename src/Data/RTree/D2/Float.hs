{-# LANGUAGE PatternSynonyms #-}

{- |
     Module     : Data.RTree.D2.Float
     Copyright  : Copyright (c) 2015, Birte Wagner, Sebastian Philipp
                  Copyright (c) 2022, Oleksii Divak
     License    : MIT

     Maintainer : Oleksii Divak
     Stability  : experimental
     Portability: not portable

     This module (and every module below it) is a duplicate of "Data.RTree.Double",
     defined for 'Float's instead of 'Double's.
-}

module Data.RTree.D2.Float
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
  , Data.RTree.D2.Float.Internal.null
  , size

    -- ** Map
  , Data.RTree.D2.Float.Internal.map
  , map'
  , mapWithKey
  , mapWithKey'

    -- ** Fold
    -- | === Left-to-right
  , Data.RTree.D2.Float.Internal.foldl
  , Data.RTree.D2.Float.Internal.foldl'
  , foldlWithKey
  , foldlWithKey'

    -- | === Right-to-left
  , Data.RTree.D2.Float.Internal.foldr
  , Data.RTree.D2.Float.Internal.foldr'
  , foldrWithKey
  , foldrWithKey'

    -- | === Monoid
  , Data.RTree.D2.Float.Internal.foldMap
  , foldMapWithKey

    -- ** Traverse
  , Data.RTree.D2.Float.Internal.traverse
  , traverseWithKey
  ) where

import           Data.RTree.D2.Float.Internal



-- | \(\mathcal{O}(1)\).
--   Empty tree.
empty :: RTree a
empty = Empty

-- | \(\mathcal{O}(1)\).
--   Tree with a single entry.
singleton :: MBR -> a -> RTree a
singleton = Leaf1
