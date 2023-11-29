{-# LANGUAGE MagicHash
           , UnboxedTuples #-}

{- |
     Module     : Data.RTree.Float.Strict
     Copyright  : Copyright (c) 2015, Birte Wagner, Sebastian Philipp
                  Copyright (c) 2022, Oleksii Divak
     License    : MIT

     Maintainer : Oleksii Divak
     Stability  : experimental
     Portability: not portable

     This module (and every module below it) is a duplicate of "Data.RTree.Double.Strict",
     defined for 'Float's instead of 'Double's.
-}

module Data.RTree.Float.Strict
  ( MBR (MBR)
  , RTree

    -- * Construction
  , empty
  , singleton

    -- * Queries
    -- ** Value-only
    -- *** Map
  , Data.RTree.Float.Strict.Internal.map
  , map'

    -- *** Fold
  , Data.RTree.Float.Strict.Internal.foldl
  , Data.RTree.Float.Strict.Internal.foldr
  , Data.RTree.Float.Strict.Internal.foldMap
  , Data.RTree.Float.Strict.Internal.foldl'
  , Data.RTree.Float.Strict.Internal.foldr'

    -- *** Traversal
  , Data.RTree.Float.Strict.Internal.traverse

    -- ** Key/value
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
  , insertGut
  , insert

    -- * Deletion
  , delete

    -- * Bulk-loading
  , bulkSTR
  ) where

import           Data.RTree.Float.Strict.Internal



-- | \(\mathcal{O}(1)\). Empty R-tree.
empty :: RTree a
empty = Empty

-- | \(\mathcal{O}(1)\). R-tree with a single entry.
singleton :: MBR -> a -> RTree a
singleton = Leaf1
