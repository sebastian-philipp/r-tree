{-# LANGUAGE MagicHash
           , PatternSynonyms
           , UnboxedTuples
           , ViewPatterns #-}

{-# OPTIONS_HADDOCK not-home #-}

{- |
     Module     : Data.RTree.Double.Strict.Unsafe
     Copyright  : Copyright (c) 2015, Birte Wagner, Sebastian Philipp
                  Copyright (c) 2022, Oleksii Divak
     License    : MIT

     Maintainer : Oleksii Divak
     Stability  : experimental
     Portability: not portable

     Underlying implementation of the 'RTree'.
-}

module Data.RTree.Double.Strict.Unsafe
  ( -- * MBR
    MBR (MBR, UnsafeMBR)
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

    -- * Predicate
  , Predicate (..)

    -- * R-Tree
  , RTree (..)
  , Node (..)
  ) where

import           Data.RTree.Double.Strict.Internal
