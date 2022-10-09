{-# LANGUAGE TypeApplications #-}

module Main where

import           Data.NoTree (NoTree)
import qualified Data.NoTree as No
import           Data.RTree.MBR as MBR
import           Data.RTree.Lazy (RTree)
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



lookup
  :: String -> ([(MBR Double, Int)] -> RTree Double Int)
  -> String -> (MBR Double -> R.Predicate Double) -> Benchmark
lookup cat from name pre =
  env ( do g <- newIOGenM $ mkStdGen 0
           no <- genPoints 4096 g
           return (from $ No.toList no, take 1024 $ fst <$> No.toList no)
      ) $ \ ~(r, brs) ->
    bgroup (cat <> "/lookup/" <> name) $
      [ bench "First" $
          flip nf brs $
                 foldMap $ \x -> [R.foldMap (pre x) (First . Just) r]

      , bench "List" $
          flip nf brs $
                 foldMap $ \x -> [R.foldMap (pre x) (:[]) r]
      ]


map
  :: String -> ([(MBR Double, Int)] -> RTree Double Int)
  -> String -> (MBR Double -> R.Predicate Double) -> Benchmark
map cat from name pre =
  env ( do g <- newIOGenM $ mkStdGen 0
           no <- genPoints 4096 g
           as <- genAreas 1024 g
           return (from $ No.toList no, as)
      ) $ \ ~(r, brs) ->
    bench (cat <> "/map/" <> name) $
      flip nf brs $
             fmap $ \x -> [R.map (pre x) (+1) r]

traversal
  :: String -> ([(MBR Double, Int)] -> RTree Double Int)
  -> String -> (MBR Double -> R.Predicate Double) -> Benchmark
traversal cat from name pre =
  env ( do g <- newIOGenM $ mkStdGen 0
           no <- genPoints 4096 g
           as <- genAreas 1024 g
           return (from $ No.toList no, as)
      ) $ \ ~(r, brs) ->
    bench (cat <> "/traverse/" <> name) $
      flip nfAppIO brs $
             traverse $ \x -> fmap (:[]) $ R.traverse (pre x) (pure @IO . (+) 1) r


fromListGut :: (Foldable t, Num r, Ord r) => t (MBR r, b) -> RTree r b
fromListGut = foldl' (flip $ uncurry R.insertGut) R.empty


main :: IO ()
main = do
  defaultMain
    [ env ( do g <- newIOGenM $ mkStdGen 0
               no <- genPoints 4096 g
               return $ No.toList no
          ) $ \ ~raw ->
        bgroup "insert"
          [ bench "BKSS" $
              nf R.fromList raw

          , bench "Gut" $
              nf fromListGut raw

          , bench "STR" $
              nf R.bulkSTR raw
          ]

    , env ( do g <- newIOGenM $ mkStdGen 0
               no <- genPoints 4096 g
               return (R.fromList $ No.toList no, fst <$> No.toList no)
          ) $ \ ~(r, brs) ->
        bench "delete" $
          nf (foldr R.delete r) brs

    , lookup "BKSS" R.fromList "equals"     R.equals
    , lookup "BKSS" R.fromList "intersects" R.intersects
    , lookup "BKSS" R.fromList "contains"   R.contains
    , lookup "BKSS" R.fromList "within"     R.within

    , map "BKSS" R.fromList "equals"     R.equals
    , map "BKSS" R.fromList "intersects" R.intersects
    , map "BKSS" R.fromList "contains"   R.contains
    , map "BKSS" R.fromList "within"     R.within

    , traversal "BKSS" R.fromList "equals"     R.equals
    , traversal "BKSS" R.fromList "intersects" R.intersects
    , traversal "BKSS" R.fromList "contains"   R.contains
    , traversal "BKSS" R.fromList "within"     R.within

    , lookup "Gut" fromListGut "equals"     R.equals
    , lookup "Gut" fromListGut "intersects" R.intersects
    , lookup "Gut" fromListGut "contains"   R.contains
    , lookup "Gut" fromListGut "within"     R.within

    , map "Gut" fromListGut "equals"     R.equals
    , map "Gut" fromListGut "intersects" R.intersects
    , map "Gut" fromListGut "contains"   R.contains
    , map "Gut" fromListGut "within"     R.within

    , traversal "Gut" fromListGut "equals"     R.equals
    , traversal "Gut" fromListGut "intersects" R.intersects
    , traversal "Gut" fromListGut "contains"   R.contains
    , traversal "Gut" fromListGut "within"     R.within

    , lookup "STR" R.bulkSTR "equals"     R.equals
    , lookup "STR" R.bulkSTR "intersects" R.intersects
    , lookup "STR" R.bulkSTR "contains"   R.contains
    , lookup "STR" R.bulkSTR "within"     R.within

    , map "STR" R.bulkSTR "equals"     R.equals
    , map "STR" R.bulkSTR "intersects" R.intersects
    , map "STR" R.bulkSTR "contains"   R.contains
    , map "STR" R.bulkSTR "within"     R.within

    , traversal "STR" R.bulkSTR "equals"     R.equals
    , traversal "STR" R.bulkSTR "intersects" R.intersects
    , traversal "STR" R.bulkSTR "contains"   R.contains
    , traversal "STR" R.bulkSTR "within"     R.within
    ]
