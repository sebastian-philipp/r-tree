{-# LANGUAGE PatternSynonyms #-}

{- |
     Module     : Data.R2Tree.Float
     Copyright  : Copyright (c) 2015, Birte Wagner, Sebastian Philipp
                  Copyright (c) 2022, Oleksii Divak
     License    : MIT

     Maintainer : Oleksii Divak
     Stability  : experimental
     Portability: not portable

     This module (and every module below it) is a duplicate of "Data.R2Tree.Double",
     defined for 'Float's instead of 'Double's.
-}

module Data.R2Tree.Float
  ( MBR (MBR)
  , R2Tree

    -- * Construct
  , empty
  , singleton
  , doubleton
  , tripleton
  , quadrupleton

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
  , Data.R2Tree.Float.Internal.null
  , size

    -- ** Map
  , Data.R2Tree.Float.Internal.map
  , map'
  , mapWithKey
  , mapWithKey'

    -- ** Fold
    -- | === Left-to-right
  , Data.R2Tree.Float.Internal.foldl
  , Data.R2Tree.Float.Internal.foldl'
  , foldlWithKey
  , foldlWithKey'

    -- | === Right-to-left
  , Data.R2Tree.Float.Internal.foldr
  , Data.R2Tree.Float.Internal.foldr'
  , foldrWithKey
  , foldrWithKey'

    -- | === Monoid
  , Data.R2Tree.Float.Internal.foldMap
  , foldMapWithKey

    -- ** Traverse
  , Data.R2Tree.Float.Internal.traverse
  , traverseWithKey
  ) where

import           Data.R2Tree.Float.Internal



-- | \(\mathcal{O}(1)\).
--   Empty tree.
empty :: R2Tree a
empty = Empty

-- | \(\mathcal{O}(1)\).
--   Tree with a single entry.
singleton :: MBR -> a -> R2Tree a
singleton = Leaf1

-- | \(\mathcal{O}(1)\).
--   Tree with two entries.
doubleton :: MBR -> a -> MBR -> a -> R2Tree a
doubleton = Leaf2

-- | \(\mathcal{O}(1)\).
--   Tree with three entries.
tripleton :: MBR -> a -> MBR -> a -> MBR -> a -> R2Tree a
tripleton = Leaf3

-- | \(\mathcal{O}(1)\).
--   Tree with four entries.
quadrupleton :: MBR -> a -> MBR -> a -> MBR -> a -> MBR -> a -> R2Tree a
quadrupleton = Leaf4
