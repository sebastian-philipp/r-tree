{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications  #-}

module Gen
  ( mbb
  , rtreeInt
  ) where

import Control.Applicative ((<$>))
import Data.RTree.Base
import Data.RTree.MBB      hiding (mbb)
import Hedgehog
import Prelude             hiding (length, lookup, map, null)
import Text.Show.Functions ()

import qualified Hedgehog.Gen   as G
import qualified Hedgehog.Range as R

mbb :: MonadGen m => m MBB
mbb = G.shrink more $ do
  cx  <- G.double (R.linearFrac (-100000.0) 100000.0)
  cy  <- G.double (R.linearFrac (-100000.0) 100000.0)
  h   <- G.double (R.linearFrac        0.0  100000.0)
  w   <- G.double (R.linearFrac        0.0  100000.0)
  return $ MBB (cx - w) (cy - h) (cx + w) (cy + h)
  where more :: MBB -> [MBB]
        more mbb'@(MBB ulx uly brx bry)
            | isPointMBB mbb' = []
            | otherwise       = [MBB (mid ulx brx) (mid uly bry) (mid ulx brx) (mid uly bry)]
        mid x y = (y - x) / 2

rtreeInt :: MonadGen m => m (RTree Int)
rtreeInt = G.shrink more $ do
  ks <- G.list (R.linear 0 100) mbb
  return $ fromList (ks `zip` [1..])
  where more :: RTree Int -> [RTree Int]
        more Empty = []
        more Leaf{} = [Empty]
        more t =
          [Empty] ++
          -- shrink to subterms
          getChildren t ++
          -- recursively shrink subterms
          [createNodeWithChildren newChildred | newChildred <- more <$> getChildren t]
