{-# LANGUAGE TypeApplications #-}

module Main where

import           Data.NoTree (NoTree)
import qualified Data.NoTree as No
import           Data.RTree.MBR as MBR
import qualified Data.RTree.Lazy as R

import           Control.Monad
import           Data.Foldable
import           Data.List hiding (lookup, map)
import           Data.Monoid
import           Gauge
import           Prelude hiding (lookup, map)
import           System.Random.Stateful



randPoint :: StatefulGen g m => g -> m (MBR Double)
randPoint g = do
  a <- uniformRM (0, 2 ^ (20 :: Int)) g
  b <- uniformRM (0, 2 ^ (20 :: Int)) g
  return $ MBR a b (a + 1) (b + 1)

randArea :: StatefulGen g m => g -> m (MBR Double)
randArea g = do
  a <- uniformRM (0, 2 ^ (20 :: Int)) g
  b <- uniformRM (0, 2 ^ (20 :: Int)) g
  c <- uniformRM (0, 2 ^ (20 :: Int)) g
  d <- uniformRM (0, 2 ^ (20 :: Int)) g
  return $ MBR (a `min` c) (b `min` d) (a `max` c) (b `max` d)



newStdGenM :: IO (IOGenM StdGen)
newStdGenM = newIOGenM $ mkStdGen 0

genPoints :: StatefulGen g m => Int -> g -> m (NoTree Double Int)
genPoints n g = No.fromList . flip zip [0..] <$> replicateM n (randPoint g)

genAreas :: StatefulGen g m => Int -> g -> m [MBR Double]
genAreas n = replicateM n . randPoint



lookup :: String -> (MBR Double -> R.Predicate Double) -> Benchmark
lookup name pre =
  env ( do g <- newIOGenM $ mkStdGen 0
           no <- genPoints 4096 g
           return (R.fromList $ No.toList no, take 1024 $ fst <$> No.toList no)
      ) $ \ ~(r, brs) ->
    bgroup ("lookup/" <> name) $
      [ bench "First" $
          flip nf brs $
                 foldMap $ \x -> [R.foldMap (pre x) (First . Just) r]

      , bench "Last" $
          flip nf brs $
                 foldMap $ \x -> [R.foldMap (pre x) (Last . Just) r]
      ]


map :: String -> (MBR Double -> R.Predicate Double) -> Benchmark
map name pre =
  env ( do g <- newIOGenM $ mkStdGen 0
           no <- genPoints 4096 g
           as <- genAreas 1024 g
           return (R.fromList $ No.toList no, as)
      ) $ \ ~(r, brs) ->
    bench ("map/" <> name) $
      flip nf brs $
             fmap $ \x -> [R.map (pre x) (+1) r]

traversal :: String -> (MBR Double -> R.Predicate Double) -> Benchmark
traversal name pre =
  env ( do g <- newIOGenM $ mkStdGen 0
           no <- genPoints 4096 g
           as <- genAreas 1024 g
           return (R.fromList $ No.toList no, as)
      ) $ \ ~(r, brs) ->
    bench ("traverse/" <> name) $
      flip nfAppIO brs $
             traverse $ \x -> fmap (:[]) $ R.traverse (pre x) (pure @IO . (+) 1) r



main :: IO ()
main = do
  defaultMain
    [ {- env ( do g <- newIOGenM $ mkStdGen 0
               no <- genPoints 4096 g
               return $ No.toList no
          ) $ \ ~raw ->
        bgroup "insert"
          [ bench "BKSS" $
              nf (foldr (uncurry R.insert) R.empty) raw

          , bench "Gut" $
              nf (foldr (uncurry R.insertGut) R.empty) raw

          , bench "STR" $
              nf R.bulkSTR raw
          ]

    , env ( do g <- newIOGenM $ mkStdGen 0
               no <- genPoints 4096 g
               return (R.fromList $ No.toList no, fst <$> No.toList no)
          ) $ \ ~(r, brs) ->
        bench "delete" $
          nf (foldr R.delete r) brs

    , lookup "equals"     R.equals
    , lookup "intersects" R.intersects
    , lookup "contains"   R.contains
    , lookup "within"     R.within

    , map "equals"     R.equals
    , map "intersects" R.intersects
    , map "contains"   R.contains
    , map "within"     R.within
-}
      traversal "equals"     R.equals
    , traversal "intersects" R.intersects
    , traversal "contains"   R.contains
    , traversal "within"     R.within
    ]
