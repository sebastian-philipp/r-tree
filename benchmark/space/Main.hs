{-# LANGUAGE TypeApplications #-}

module Main where

import           Data.RTree.MBR as MBR
import qualified Data.RTree.Lazy as R

import           Control.Monad
import           Data.Foldable
import           Data.List hiding (lookup, map)
import           Prelude hiding (lookup, map)
import           System.Random.Stateful
import           Weigh



randMBR :: StatefulGen g m => g -> m (MBR Double)
randMBR g = do
  a <- uniformRM (0, 2 ^ (20 :: Int)) g
  b <- uniformRM (0, 2 ^ (20 :: Int)) g
  return $ MBR a b (a + 1) (b + 1)



main :: IO ()
main = do
  g <- newIOGenM $ mkStdGen 0
  raw <- flip zip [0 :: Int ..] <$> replicateM 16384 (randMBR g)

  mainWith $ do
    setColumns [Case, Allocated, Max, Live, GCs]
    wgroup "insert" $ do
      io "BKSS" (pure . foldr (uncurry R.insert) R.empty) raw
      io "Gut" (pure . foldr (uncurry R.insertGut) R.empty) raw
