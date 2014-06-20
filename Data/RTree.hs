{- |
    Module     : Data.RTree
    Copyright  : Copyright (c) 2014, Birte Wagner, Sebastian Philipp
    License    : MIT

    Maintainer : Birte Wagner, Sebastian Philipp (sebastian@spawnhost.de)
    Stability  : experimental
    Portability: not portable

    R-Tree is a spatial data structure similar to Quadtrees or B-Trees.

    An R-Tree is a balanced tree and optimized for lookups. This implemetation useses an R-Tree to privide
    a map to arbitrary values.

    Some function names clash with "Prelude" names, therefore this module is usually
    imported @qualified@, e.g.

    > import           Data.RTree (RTree)
    > import qualified Data.RTree as RT

    this implemetation is incomplete at the moment. Feel free to send comments, patches or merge requests.

-}


module Data.RTree
(
    -- * 'MBB'
    MBB.MBB
    , MBB.mbb
    -- * Data Type
    , RTree
    -- * Constructors
    , empty
    , singleton
    -- * Modification
    , insert
    , insertWith
    , delete
    , mapMaybe
    -- ** Merging
    , union
    , unionWith
    -- * Searching and Properties
    , lookup
    , lookupRange
    , lookupRangeWithKey
    , length
    , null
    , keys
    , values
    -- * Lists
    , fromList
    , toList
) where

import Prelude ()
import Data.RTree.Base
import Data.Functor

import qualified Data.RTree.MBB as MBB

instance Functor RTree where
  fmap = Data.RTree.Base.map
