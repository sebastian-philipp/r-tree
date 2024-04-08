{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import           Data.RTree.D2.Double (RTree, MBR, Predicate)
import qualified Data.RTree.D2.Double as R

import           Control.DeepSeq
import           Control.Monad
import           Data.Foldable
import           Data.List hiding (lookup, map)
import           Data.Monoid
import           Gauge
import           Prelude hiding (lookup, map)
import           System.Random.Stateful



instance NFData MBR where
  rnf ba = ba `seq` ()



randPoint :: StatefulGen g m => g -> m MBR
randPoint g = do
  a <- uniformRM (0, 2 ^ (20 :: Int)) g
  b <- uniformRM (0, 2 ^ (20 :: Int)) g
  return $ R.MBR a b (a + 1) (b + 1)

randArea :: StatefulGen g m => g -> m MBR
randArea g = do
  a <- uniformRM (0, 2 ^ (20 :: Int)) g
  b <- uniformRM (0, 2 ^ (20 :: Int)) g
  c <- uniformRM (0, 2 ^ (20 :: Int)) g
  d <- uniformRM (0, 2 ^ (20 :: Int)) g
  return $ R.MBR a b c d



newStdGenM :: IO (IOGenM StdGen)
newStdGenM = newIOGenM $ mkStdGen 0

genPoints :: StatefulGen g m => Int -> g -> m [(MBR, Int)]
genPoints n g = flip zip [0..] <$> replicateM n (randPoint g)

genAreas :: StatefulGen g m => Int -> g -> m [MBR]
genAreas n = replicateM n . randPoint



lookup
  :: String -> ([(MBR, Int)] -> RTree Int)
  -> String -> (MBR -> Predicate) -> Benchmark
lookup cat from name pre =
  env ( do g <- newIOGenM $ mkStdGen 0
           no <- genPoints 4096 g
           return (from no, take 1024 $ fst <$> no)
      ) $ \ ~(r, brs) ->
    bgroup (cat <> "/lookup/" <> name) $
      [ bench "First" $
          flip nf brs $
                 foldMap $ \x -> [R.foldMapRangeWithKey (pre x) (\_ -> First . Just) r]

      , bench "List" $
          flip nf brs $
                 foldMap $ \x -> [R.foldMapRangeWithKey (pre x) (\_ -> (:[])) r]
      ]


map
  :: String -> ([(MBR, Int)] -> RTree Int)
  -> String -> (MBR -> Predicate) -> Benchmark
map cat from name pre =
  env ( do g <- newIOGenM $ mkStdGen 0
           no <- genPoints 4096 g
           as <- genAreas 1024 g
           return (from no, as)
      ) $ \ ~(r, brs) ->
    bench (cat <> "/map/" <> name) $
      flip nf brs $
             fmap $ \x -> [R.adjustRangeWithKey (pre x) (\_ -> (+) 1) r]

traversal
  :: String -> ([(MBR, Int)] -> RTree Int)
  -> String -> (MBR -> Predicate) -> Benchmark
traversal cat from name pre =
  env ( do g <- newIOGenM $ mkStdGen 0
           no <- genPoints 4096 g
           as <- genAreas 1024 g
           return (from no, as)
      ) $ \ ~(r, brs) ->
    bench (cat <> "/traverse/" <> name) $
      flip nfAppIO brs $
             traverse $ \x -> fmap (:[]) $ R.traverseRangeWithKey (pre x) (\_ -> pure @IO . (+) 1) r


fromList :: Foldable t => t (MBR, b) -> RTree b
fromList = foldr (uncurry R.insert) R.empty

fromListGut :: Foldable t => t (MBR, b) -> RTree b
fromListGut = foldr (uncurry R.insertGut) R.empty


main :: IO ()
main = do
  defaultMain
    [ env ( do g <- newIOGenM $ mkStdGen 0
               no <- genPoints 4096 g
               return no
          ) $ \ ~raw ->
        bgroup "insert"
          [ bench "BKSS" $
              nf fromList raw

          , bench "Gut" $
              nf fromListGut raw

          , bench "STR" $
              nf R.bulkSTR raw
          ]

    , env ( do g <- newIOGenM $ mkStdGen 0
               no <- genPoints 4096 g
               return (fromList no, fst <$> no)
          ) $ \ ~(r, brs) ->
        bench "delete" $
          nf (foldr R.delete r) brs

    , lookup "BKSS" fromList "equals"      R.equals
    , lookup "BKSS" fromList "intersects"  R.intersects
    , lookup "BKSS" fromList "contains"    R.contains
    , lookup "BKSS" fromList "containedBy" R.containedBy

    , map "BKSS" fromList "equals"      R.equals
    , map "BKSS" fromList "intersects"  R.intersects
    , map "BKSS" fromList "contains"    R.contains
    , map "BKSS" fromList "containedBy" R.containedBy

    , traversal "BKSS" fromList "equals"      R.equals
    , traversal "BKSS" fromList "intersects"  R.intersects
    , traversal "BKSS" fromList "contains"    R.contains
    , traversal "BKSS" fromList "containedBy" R.containedBy

    , lookup "Gut" fromListGut "equals"      R.equals
    , lookup "Gut" fromListGut "intersects"  R.intersects
    , lookup "Gut" fromListGut "contains"    R.contains
    , lookup "Gut" fromListGut "containedBy" R.containedBy

    , map "Gut" fromListGut "equals"      R.equals
    , map "Gut" fromListGut "intersects"  R.intersects
    , map "Gut" fromListGut "contains"    R.contains
    , map "Gut" fromListGut "containedBy" R.containedBy

    , traversal "Gut" fromListGut "equals"      R.equals
    , traversal "Gut" fromListGut "intersects"  R.intersects
    , traversal "Gut" fromListGut "contains"    R.contains
    , traversal "Gut" fromListGut "containedBy" R.containedBy

    , lookup "STR" R.bulkSTR "equals"      R.equals
    , lookup "STR" R.bulkSTR "intersects"  R.intersects
    , lookup "STR" R.bulkSTR "contains"    R.contains
    , lookup "STR" R.bulkSTR "containedBy" R.containedBy

    , map "STR" R.bulkSTR "equals"      R.equals
    , map "STR" R.bulkSTR "intersects"  R.intersects
    , map "STR" R.bulkSTR "contains"    R.contains
    , map "STR" R.bulkSTR "containedBy" R.containedBy

    , traversal "STR" R.bulkSTR "equals"      R.equals
    , traversal "STR" R.bulkSTR "intersects"  R.intersects
    , traversal "STR" R.bulkSTR "contains"    R.contains
    , traversal "STR" R.bulkSTR "containedBy" R.containedBy
    ]
