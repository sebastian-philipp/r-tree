{- |
  Module     : Data.RTree
  Copyright  : Copyright (c) 2014, Birte Wagner, Sebastian Philipp
  License    : MIT

  Maintainer : Birte Wagner, Sebastian Philipp (sebastian@spawnhost.de)
  Stability  : experimental
  Portability: not portable

  R-Tree is a spartial data structure similar to Quadtrees or B-Trees.
  
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
    MBB.MBB,
    MBB.mbb,
    RTree,
    empty,
    singleton,
    insert,
    union,
    lookup,
    lookupRange,
    lookupRangeWithKey,
    fromList,
    toList,
    delete,
    length,
    null,
    keys,
    values,

) where

import Prelude ()
import Data.RTree.Base

import qualified Data.RTree.MBB as MBB