{-# LANGUAGE RankNTypes #-}

module Test.RTree.D2.Double.Sample
  ( Sample
  , zero
  , one
  , four
  , five
  , tiny
  , small
  , medium
  , large

  , mkUnary0
  , mkUnary1
  ) where

import           Data.RTree.D2.Double
import           No.Tree.D2 (NoTree)
import qualified No.Tree.D2 as No
import           Test.Kit

import           System.Random



data Sample =
       Sample
         [(MBR, Int)] -- ^ Keys in the tree
         [(MBR, Int)] -- ^ Keys not in the tree
       deriving Show

zero, one, four, five :: Sample
zero =
  Sample
    []
    [(MBR 6 3 9 6, 6), (MBR 2 7 7 8, 7), (MBR 1 2 3 4, 8), (MBR 5 1 9 4, 9)]

one =
  Sample
    [(MBR 4 5 6 7, 1)]
    [(MBR 6 3 9 6, 6), (MBR 2 7 7 8, 7), (MBR 1 2 3 4, 8), (MBR 5 1 9 4, 9)]

four =
  Sample
    [(MBR 3 4 5 6, 1), (MBR 1 2 6 2, 2), (MBR 4 1 8 7, 3), (MBR 3 2 9 3, 4)]
    [(MBR 6 3 9 6, 6), (MBR 2 7 7 8, 7), (MBR 1 2 3 4, 8), (MBR 5 1 9 4, 9)]

five =
  Sample
    [(MBR 3 4 5 6, 1), (MBR 1 2 6 2, 2), (MBR 4 1 8 7, 3), (MBR 3 2 9 3, 4), (MBR 2 1 7 7, 5)]
    [(MBR 6 3 9 6, 6), (MBR 2 7 7 8, 7), (MBR 1 2 3 4, 8), (MBR 5 1 9 4, 9)]



randMBR :: RandomGen g => (Int, Int) -> g -> (MBR, g)
randMBR r g0 =
  let ~(x0, g1) = uniformR r g0
      ~(y0, g2) = uniformR r g1
      ~(x1, g3) = uniformR r g2
      ~(y1, g4) = uniformR r g3

  in (MBR (fromIntegral x0) (fromIntegral y0) (fromIntegral x1) (fromIntegral y1), g4)

list :: (g -> (a, g)) -> Int -> g -> ([a], g)
list gen = go
  where
    go n g
      | n <= 0    = ([], g)
      | otherwise = let ~(a, g')   = gen g
                        ~(as, g'') = go (n - 1) g'
                    in (a:as, g'')



halve :: [a] -> ([a], [a])
halve (a:b:cs) = let ~(as, bs) = halve cs
                 in (a:as, b:bs)
halve as = (as, [])

sample :: (Int, Int) -> Int -> StdGen -> Sample
sample r n g0 =
  let ~(xs, _)  = list (randMBR r) n g0

      ~(as, bs) = halve $ zip xs [1..]

  in Sample as bs



tiny, small, medium, large :: Sample
tiny   = sample (0x1000, 0x80000) 16   (mkStdGen 0)
small  = sample (0x1000, 0x80000) 64   (mkStdGen 1)
medium = sample (0x1000, 0x80000) 512  (mkStdGen 2)
large  = sample (0x1000, 0x80000) 4096 (mkStdGen 3)



type FromList tree = forall a. [(MBR, a)] -> tree a

mkUnary0 :: FromList tree -> Sample -> [Case () (tree Int) (NoTree Int)]
mkUnary0 fromList (Sample xs _) =
  [Case () (fromList xs) (No.fromList xs)]

mkUnary1 :: FromList tree -> Sample -> [Case (MBR, Int) (tree Int) (NoTree Int)]
mkUnary1 fromList (Sample xs ys) =
  let tree = fromList xs
      no = No.fromList xs

  in fmap (\(bx, x) -> Case (bx, x) tree no) $ xs <> ys
