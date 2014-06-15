{- |
    Module     : Data.RTree.Strict
    Copyright  : Copyright (c) 2014, Birte Wagner, Sebastian Philipp
    License    : MIT

    Maintainer : Birte Wagner, Sebastian Philipp (sebastian@spawnhost.de)
    Stability  : experimental
    Portability: not portable

    This is the Strict version of 'Data.RTree'

    the following property should be true (by using isNF ) :

    >>> propNF :: RTree a -> > IO Bool
    >>> propNF e = isNF $! e

-}


module Data.RTree.Strict
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

import qualified Data.RTree.MBB as MBB