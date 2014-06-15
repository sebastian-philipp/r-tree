{-# LANGUAGE BangPatterns               #-}
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
    MBB
    , mbb
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

import           Prelude hiding (lookup, length, null)
import Data.RTree.Base hiding (singleton, fromList, insertWith)

import           Control.Applicative ((<$>))
import Data.RTree.MBB

-- ---------------
-- smart constuctors

-- | creates a single element tree
singleton :: MBB -> a -> RTree a
singleton mbb !x = Leaf mbb x

-- ----------------------------------
-- Lists

-- | creates a tree out of pairs
fromList :: [(MBB, a)] -> RTree a
fromList l = fromList' $ (uncurry singleton) <$> l
-- ----------------------------------
-- insert 

-- | Inserts an element whith the given 'MBB' and a value in a tree. The combining function will be used if the value already exists.
insertWith :: (a -> a -> a) -> MBB -> a -> RTree a -> RTree a
insertWith f mbb e oldRoot = unionDistinctWith f (singleton mbb e) oldRoot

