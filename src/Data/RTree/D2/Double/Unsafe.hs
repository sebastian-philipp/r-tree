{-# OPTIONS_HADDOCK not-home #-}

{- |
     Module     : Data.RTree.D2.Double.Unsafe
     Copyright  : Copyright (c) 2015, Birte Wagner, Sebastian Philipp
                  Copyright (c) 2022, Oleksii Divak
     License    : MIT

     Maintainer : Oleksii Divak
     Stability  : experimental
     Portability: not portable

     Underlying implementation of the 'RTree'.
-}

module Data.RTree.D2.Double.Unsafe
  ( MBR (MBR, UnsafeMBR)

    -- | === R-tree
    --   
    --   Each t'MBR' is tied to the value directly after it.
    --
    --   Invariant: the t'MBR' of each non-leaf node encloses
    --              all the t'MBR's inside the node.
  , RTree (..)

    -- * Common operations
  , validMBR
  , eqMBR
  , unionMBR
  , areaMBR
  , marginMBR
  , distanceMBR
  , containsMBR
  , containsMBR'
  , intersectionMBR
  , intersectionMBR'

    -- * Range
  , Predicate (..)
  ) where

import           Data.RTree.D2.Double.Internal
