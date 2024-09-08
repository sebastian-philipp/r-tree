{-# OPTIONS_HADDOCK not-home #-}

{- |
     Module     : Data.R2Tree.Double.Unsafe
     Copyright  : Copyright (c) 2015, Birte Wagner, Sebastian Philipp
                  Copyright (c) 2022, Oleksii Divak
     License    : MIT

     Maintainer : Oleksii Divak
     Stability  : experimental
     Portability: not portable

     Underlying implementation of the 'R2Tree'.
-}

module Data.R2Tree.Double.Unsafe
  ( MBR (MBR, UnsafeMBR)

    -- | === R-tree
    --   
    --   Each t'MBR' is tied to the value directly after it.
    --
    --   Invariant: the t'MBR' of each non-leaf node encloses
    --              all the t'MBR's inside the node.
  , R2Tree (..)

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

import           Data.R2Tree.Double.Internal
