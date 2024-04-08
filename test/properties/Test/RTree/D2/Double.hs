{-# LANGUAGE RankNTypes #-}

module Test.RTree.D2.Double
  ( test
  ) where

import qualified Data.RTree.D2.Double as R
import           Data.RTree.D2.Double.Debug
import           Data.RTree.D2.Double.Unsafe
import           No.Tree.D2 (NoTree)
import qualified No.Tree.D2 as No
import           Test.Kit
import           Test.RTree.D2.Double.Sample

import           Data.Functor.Identity
import           Data.List
import           Test.Hspec



mbrT :: Spec
mbrT = do
  describe "valid" $ do
    it "0 0 1 1" $
      validMBR (UnsafeMBR 0 0 1 1) `shouldBe` True

    it "1 0 0 1" $
      validMBR (UnsafeMBR 1 0 0 1) `shouldBe` False

    it "1 1 0 0" $
      validMBR (UnsafeMBR 1 1 0 0) `shouldBe` False

  describe "union" $ do
    it "2 1 3 4 / 6 5 8 9" $
      unionMBR (UnsafeMBR 2 1 3 4) (UnsafeMBR 6 5 8 9) `shouldBe` UnsafeMBR 2 1 8 9

    it "2 4 5 8 / 1 3 6 9" $
      unionMBR (UnsafeMBR 2 4 5 8) (UnsafeMBR 1 3 6 9) `shouldBe` UnsafeMBR 1 3 6 9

    it "1 3 6 9 / 2 4 7 8" $
      unionMBR (UnsafeMBR 1 3 6 9) (UnsafeMBR 2 4 7 8) `shouldBe` UnsafeMBR 1 3 7 9

  describe "area" $ do
    it "2 1 8 9" $
      areaMBR (UnsafeMBR 2 1 8 9) `shouldBe` 48

    it "3 4 6 5" $
      areaMBR (UnsafeMBR 3 4 6 5) `shouldBe` 3

  describe "margin" $ do
    it "2 1 8 9" $
      marginMBR (UnsafeMBR 2 1 8 9) `shouldBe` 14

    it "3 4 6 5" $
      marginMBR (UnsafeMBR 3 4 6 5) `shouldBe` 4

  describe "distance" $ do
    it "2 1 3 4 / 6 5 8 9" $
      distanceMBR (UnsafeMBR 2 1 3 4) (UnsafeMBR 6 5 8 9) `shouldBe` 162

    it "2 4 5 8 / 1 3 6 9" $
      distanceMBR (UnsafeMBR 2 4 5 8) (UnsafeMBR 1 3 6 9) `shouldBe` 0

    it "1 3 6 9 / 2 4 7 8" $
      distanceMBR (UnsafeMBR 1 3 6 9) (UnsafeMBR 2 4 7 8) `shouldBe` 4

  describe "contains" $ do
    it "2 1 3 4 / 6 5 8 9" $
      containsMBR (UnsafeMBR 2 1 3 4) (UnsafeMBR 6 5 8 9) `shouldBe` False

    it "2 1 8 9 / 3 4 5 6" $
      containsMBR (UnsafeMBR 2 1 8 9) (UnsafeMBR 3 4 5 6) `shouldBe` True

    it "2 1 8 9 / 2 1 2 8" $
      containsMBR (UnsafeMBR 2 1 8 9) (UnsafeMBR 2 1 2 8) `shouldBe` True

    it "2 1 8 9 / 8 9 8 9" $
      containsMBR (UnsafeMBR 2 1 8 9) (UnsafeMBR 8 9 8 9) `shouldBe` True

  describe "contains'" $ do
    it "2 1 3 4 / 6 5 8 9" $
      containsMBR' (UnsafeMBR 2 1 3 4) (UnsafeMBR 6 5 8 9) `shouldBe` False

    it "2 1 8 9 / 3 4 5 6" $
      containsMBR' (UnsafeMBR 2 1 8 9) (UnsafeMBR 3 4 5 6) `shouldBe` True

    it "2 1 8 9 / 2 1 2 8" $
      containsMBR' (UnsafeMBR 2 1 8 9) (UnsafeMBR 2 1 2 8) `shouldBe` False

    it "2 1 8 9 / 8 9 8 9" $
      containsMBR' (UnsafeMBR 2 1 8 9) (UnsafeMBR 8 9 8 9) `shouldBe` False

  describe "intersection" $ do
    it "2 1 3 4 / 6 5 8 9" $
      intersectionMBR (UnsafeMBR 2 1 3 4) (UnsafeMBR 6 5 8 9) `shouldBe` Nothing

    it "1 3 6 9 / 2 4 5 8" $
      intersectionMBR (UnsafeMBR 1 3 6 9) (UnsafeMBR 2 4 5 8) `shouldBe` Just (UnsafeMBR 2 4 5 8)

    it "2 4 7 8 / 1 3 6 9" $
      intersectionMBR (UnsafeMBR 2 4 7 8) (UnsafeMBR 1 3 6 9) `shouldBe` Just (UnsafeMBR 2 4 6 8)

    it "1 2 5 4 / 3 4 6 5" $
      intersectionMBR (UnsafeMBR 1 2 5 4) (UnsafeMBR 3 4 6 5) `shouldBe` Just (UnsafeMBR 3 4 5 4)

    it "3 4 5 6 / 5 6 7 8" $
      intersectionMBR (UnsafeMBR 3 4 5 6) (UnsafeMBR 5 6 7 8) `shouldBe` Just (UnsafeMBR 5 6 5 6)

  describe "intersection'" $ do
    it "2 1 3 4 / 6 5 8 9" $
      intersectionMBR' (UnsafeMBR 2 1 3 4) (UnsafeMBR 6 5 8 9) `shouldBe` Nothing

    it "1 3 6 9 / 2 4 5 8" $
      intersectionMBR' (UnsafeMBR 1 3 6 9) (UnsafeMBR 2 4 5 8) `shouldBe` Just (UnsafeMBR 2 4 5 8)

    it "2 4 7 8 / 1 3 6 9" $
      intersectionMBR' (UnsafeMBR 2 4 7 8) (UnsafeMBR 1 3 6 9) `shouldBe` Just (UnsafeMBR 2 4 6 8)

    it "1 2 5 4 / 3 4 6 5" $
      intersectionMBR' (UnsafeMBR 1 2 5 4) (UnsafeMBR 3 4 6 5) `shouldBe` Nothing

    it "3 4 5 6 / 5 6 7 8" $
      intersectionMBR' (UnsafeMBR 3 4 5 6) (UnsafeMBR 5 6 7 8) `shouldBe` Nothing



predicateT :: Spec
predicateT = do
  describe "equals 2 3 7 6" $ do
    let Predicate nodePred leafPred = R.equals (UnsafeMBR 2 3 7 6)
    it "node 1 2 9 8" $
      nodePred (UnsafeMBR 1 2 9 8) `shouldBe` True

    it "leaf 1 2 9 8" $
      leafPred (UnsafeMBR 1 2 9 8) `shouldBe` False

    it "node 2 3 7 6" $
      nodePred (UnsafeMBR 2 3 7 6) `shouldBe` True

    it "leaf 2 3 7 6" $
      leafPred (UnsafeMBR 2 3 7 6) `shouldBe` True

    it "node 3 4 6 5" $
      nodePred (UnsafeMBR 3 4 6 5) `shouldBe` False

    it "leaf 3 4 6 5" $
      leafPred (UnsafeMBR 3 4 6 5) `shouldBe` False

    it "node 3 4 9 8" $
      nodePred (UnsafeMBR 3 4 9 8) `shouldBe` False

    it "leaf 3 4 9 8" $
      leafPred (UnsafeMBR 3 4 9 8) `shouldBe` False

  describe "intersects 2 3 7 6" $ do
    let Predicate nodePred leafPred = R.intersects (UnsafeMBR 2 3 7 6)
    it "node 1 2 9 8" $
      nodePred (UnsafeMBR 1 2 9 8) `shouldBe` True

    it "leaf 1 2 9 8" $
      leafPred (UnsafeMBR 1 2 9 8) `shouldBe` True

    it "node 2 3 7 6" $
      nodePred (UnsafeMBR 2 3 7 6) `shouldBe` True

    it "leaf 2 3 7 6" $
      leafPred (UnsafeMBR 2 3 7 6) `shouldBe` True

    it "node 3 4 6 5" $
      nodePred (UnsafeMBR 3 4 6 5) `shouldBe` True

    it "leaf 3 4 6 5" $
      leafPred (UnsafeMBR 3 4 6 5) `shouldBe` True

    it "node 3 4 9 8" $
      nodePred (UnsafeMBR 3 4 9 8) `shouldBe` True

    it "leaf 3 4 9 8" $
      leafPred (UnsafeMBR 3 4 9 8) `shouldBe` True

    it "node 2 3 7 6" $
      nodePred (UnsafeMBR 7 3 8 6) `shouldBe` True

    it "leaf 2 3 7 6" $
      leafPred (UnsafeMBR 7 3 8 6) `shouldBe` True

  describe "intersects' 2 3 7 6" $ do
    let Predicate nodePred leafPred = R.intersects' (UnsafeMBR 2 3 7 6)
    it "node 2 3 7 6" $
      nodePred (UnsafeMBR 7 3 8 6) `shouldBe` False

    it "leaf 2 3 7 6" $
      leafPred (UnsafeMBR 7 3 8 6) `shouldBe` False

  describe "contains 2 3 7 6" $ do
    let Predicate nodePred leafPred = R.contains (UnsafeMBR 2 3 7 6)
    it "node 1 2 9 8" $
      nodePred (UnsafeMBR 1 2 9 8) `shouldBe` True

    it "leaf 1 2 9 8" $
      leafPred (UnsafeMBR 1 2 9 8) `shouldBe` True

    it "node 2 3 7 6" $
      nodePred (UnsafeMBR 2 3 7 6) `shouldBe` True

    it "leaf 2 3 7 6" $
      leafPred (UnsafeMBR 2 3 7 6) `shouldBe` True

    it "node 3 4 6 5" $
      nodePred (UnsafeMBR 3 4 6 5) `shouldBe` False

    it "leaf 3 4 6 5" $
      leafPred (UnsafeMBR 3 4 6 5) `shouldBe` False

    it "node 3 4 9 8" $
      nodePred (UnsafeMBR 3 4 9 8) `shouldBe` False

    it "leaf 3 4 9 8" $
      leafPred (UnsafeMBR 3 4 9 8) `shouldBe` False

  describe "contains' 2 3 7 6" $ do
    let Predicate nodePred leafPred = R.contains' (UnsafeMBR 2 3 7 6)
    it "node 2 3 7 6" $
      nodePred (UnsafeMBR 2 3 7 6) `shouldBe` True

    it "leaf 2 3 7 6" $
      leafPred (UnsafeMBR 2 3 7 6) `shouldBe` False

  describe "containedBy 2 3 7 6" $ do
    let Predicate nodePred leafPred = R.containedBy (UnsafeMBR 2 3 7 6)
    it "node 1 2 9 8" $
      nodePred (UnsafeMBR 1 2 9 8) `shouldBe` True

    it "leaf 1 2 9 8" $
      leafPred (UnsafeMBR 1 2 9 8) `shouldBe` False

    it "node 2 3 7 6" $
      nodePred (UnsafeMBR 2 3 7 6) `shouldBe` True

    it "leaf 2 3 7 6" $
      leafPred (UnsafeMBR 2 3 7 6) `shouldBe` True

    it "node 3 4 6 5" $
      nodePred (UnsafeMBR 3 4 6 5) `shouldBe` True

    it "leaf 3 4 6 5" $
      leafPred (UnsafeMBR 3 4 6 5) `shouldBe` True

    it "node 3 4 9 8" $
      nodePred (UnsafeMBR 3 4 9 8) `shouldBe` True

    it "leaf 3 4 9 8" $
      leafPred (UnsafeMBR 3 4 9 8) `shouldBe` False

  describe "containedBy' 2 3 7 6" $ do
    let Predicate nodePred leafPred = R.containedBy' (UnsafeMBR 2 3 7 6)
    it "node 2 3 7 6" $
      nodePred (UnsafeMBR 2 3 7 6) `shouldBe` True

    it "leaf 2 3 7 6" $
      leafPred (UnsafeMBR 2 3 7 6) `shouldBe` False



rFromList :: [(MBR, a)] -> RTree a
rFromList = foldr (uncurry R.insert) R.empty

rToList :: RTree a -> [(MBR, a)]
rToList = R.foldrWithKey (\ba a -> (:) (ba, a)) []



unary0 :: [Case () (RTree Int) (NoTree Int)]
unary0 = foldMap (mkUnary0 rFromList) [zero, one, four, five, tiny, small, medium]

unary1 :: [Case (MBR, Int) (RTree Int) (NoTree Int)]
unary1 = foldMap (mkUnary1 rFromList) [zero, one, four, five, tiny, small, medium]

unary1_ :: [Case MBR (RTree Int) (NoTree Int)]
unary1_ = augment fst unary1



compareMBR :: Ord a => (MBR, a) -> (MBR, a) -> Ordering
compareMBR (MBR x0 y0 x1 y1, a) (MBR x2 y2 x3 y3, b) =
  case compare a b of
    EQ  -> case compare x0 x2 of
             EQ -> case compare y0 y2 of
                     EQ -> case compare x1 x3 of
                             EQ -> compare y1 y3
                             cmp -> cmp
                     cmp -> cmp
             cmp -> cmp
    cmp -> cmp

type TreeT s a = Test s (RTree a) (NoTree a) (RTree a) (NoTree a)

treeEq :: Ord a => RTree a -> NoTree a -> Bool
treeEq tree no =
  case validate tree of
    Valid -> sortBy compareMBR (No.toList no) == sortBy compareMBR (rToList tree)
    _     -> False

type TreeIdT s a = Test s (RTree a) (NoTree a) (Identity (RTree a)) (Identity (NoTree a))

treeIdEq :: Ord a => Identity (RTree a) -> Identity (NoTree a) -> Bool
treeIdEq (Identity tree) (Identity no) = treeEq tree no



type ListT s a = Test s (RTree a) (NoTree a) [a] [a]

listEq :: Ord a => [a] -> [a] -> Bool
listEq as bs = sort as == sort bs

type ListWithKeyT s a = Test s (RTree a) (NoTree a) [(MBR, a)] [(MBR, a)]

listWithKeyEq :: Ord a => [(MBR, a)] -> [(MBR, a)] -> Bool
listWithKeyEq as bs = sortBy compareMBR as == sortBy compareMBR bs



insertT :: (Num a, Ord a) => TreeT (MBR, a) a
insertT = Test treeEq (\(bx, x) r -> R.insert bx (negate x) r)
                      (\(bx, x) no -> No.insert bx (negate x) no)

insertGutT :: (Num a, Ord a) => TreeT (MBR, a) a
insertGutT = Test treeEq (\(bx, x) r -> R.insertGut bx (negate x) r)
                         (\(bx, x) no -> No.insert bx (negate x) no)

deleteT :: Ord a => TreeT MBR a
deleteT = Test treeEq R.delete No.delete



mapT, mapT' :: TreeT () Int
mapT  = mapT_ R.map
mapT' = mapT_ R.map'

mapT_ :: (forall a. (a -> a) -> RTree a -> RTree a) -> TreeT () Int
mapT_ f = Test treeEq (\_ -> f negate) (\_ -> No.mapWithKey (\_ -> negate))



mapWithKeyT, mapWithKeyT' :: TreeT () Int
mapWithKeyT  = mapWithKeyT_ R.mapWithKey
mapWithKeyT' = mapWithKeyT_ R.mapWithKey'

compressMBR :: MBR -> Int
compressMBR (UnsafeMBR xmin ymin xmax ymax) =
  truncate xmin + truncate ymin + truncate xmax + truncate ymax

mapWithKeyT_ :: (forall a. (MBR -> a -> a) -> RTree a -> RTree a) -> TreeT () Int
mapWithKeyT_ f =
  let g k i = compressMBR k + i
  in Test treeEq (\_ -> f g) (\_ -> No.mapWithKey g)



adjustRangeWithKeyT, adjustRangeWithKeyT' :: (MBR -> Predicate) -> TreeT MBR Int
adjustRangeWithKeyT  = adjustRangeWithKeyT_ R.adjustRangeWithKey
adjustRangeWithKeyT' = adjustRangeWithKeyT_ R.adjustRangeWithKey'

adjustRangeWithKeyT_
  :: (forall a. Predicate -> (MBR -> a -> a) -> RTree a -> RTree a)
  -> (MBR -> Predicate)
  -> TreeT MBR Int
adjustRangeWithKeyT_ f p =
  let g k i = compressMBR k + i
  in Test treeEq (\bx -> f (p bx) g) (\bx -> No.adjustRangeWithKey (p bx) g)



foldlT, foldrT, foldMapT, foldlT', foldrT' :: ListT () Int
foldlT   = foldT $ R.foldl (flip (:)) []
foldrT   = foldT $ R.foldr (:) []
foldMapT = foldT $ R.foldMap (:[])
foldlT'  = foldT $ R.foldl' (flip (:)) []
foldrT'  = foldT $ R.foldr' (:) []

foldT :: (forall a. RTree a -> [a]) -> ListT () Int
foldT f = Test listEq (\_ -> f) (\_ -> fmap snd . No.toList)



foldlWithKeyT, foldrWithKeyT, foldMapWithKeyT, foldlWithKeyT', foldrWithKeyT'
  :: ListWithKeyT () Int
foldlWithKeyT   = foldWithKeyT $ R.foldlWithKey (\z bx x -> (bx, x) : z) []
foldrWithKeyT   = foldWithKeyT $ R.foldrWithKey (\bx x -> (:) (bx, x)) []
foldMapWithKeyT = foldWithKeyT $ R.foldMapWithKey (\bx x -> [(bx, x)])
foldlWithKeyT'  = foldWithKeyT $ R.foldlWithKey' (\z bx x -> (bx, x) : z) []
foldrWithKeyT'  = foldWithKeyT $ R.foldrWithKey' (\bx x -> (:) (bx, x)) []

foldWithKeyT :: (forall a. RTree a -> [(MBR, a)]) -> ListWithKeyT () Int
foldWithKeyT f = Test listWithKeyEq (\_ -> f) (\_ -> No.toList)



foldlRangeWithKeyT
  , foldrRangeWithKeyT
  , foldMapRangeWithKeyT
  , foldlRangeWithKeyT'
  , foldrRangeWithKeyT'
 :: (MBR -> Predicate) -> ListWithKeyT MBR Int
foldlRangeWithKeyT   = foldRangeWithKeyT $ \p -> R.foldlRangeWithKey p (\z bx x -> (bx, x) : z) []
foldrRangeWithKeyT   = foldRangeWithKeyT $ \p -> R.foldrRangeWithKey p (\bx x -> (:) (bx, x)) []
foldMapRangeWithKeyT = foldRangeWithKeyT $ \p -> R.foldMapRangeWithKey p (\bx x -> [(bx, x)])
foldlRangeWithKeyT'  = foldRangeWithKeyT $ \p -> R.foldlRangeWithKey' p (\z bx x -> (bx, x) : z) []
foldrRangeWithKeyT'  = foldRangeWithKeyT $ \p -> R.foldrRangeWithKey' p (\bx x -> (:) (bx, x)) []

foldRangeWithKeyT
  :: (forall a. Predicate -> RTree a -> [(MBR, a)])
  -> (MBR -> Predicate) -> ListWithKeyT MBR Int
foldRangeWithKeyT f p =
  Test listWithKeyEq (\bx -> f (p bx))
                     (\bx -> No.foldrRangeWithKey (p bx) (\ba a -> (:) (ba, a)) [])



traverseT :: TreeIdT () Int
traverseT =
  let f = Identity . negate
  in Test treeIdEq (\_ -> R.traverse f) (\_ -> No.traverseWithKey (\_ -> f))

traverseWithKeyT :: TreeIdT () Int
traverseWithKeyT =
  let f k i = Identity $ compressMBR k + i
  in Test treeIdEq (\_ -> R.traverseWithKey f) (\_ -> No.traverseWithKey f)

traverseRangeWithKeyT :: (MBR -> Predicate) -> TreeIdT MBR Int
traverseRangeWithKeyT p =
  let f k i = Identity $ compressMBR k + i
  in Test treeIdEq (\bx -> R.traverseRangeWithKey (p bx) f) (\bx -> No.traverseRangeWithKey (p bx) f)



test :: Spec
test = do
  describe "MBR"
    mbrT

  describe "Predicate"
    predicateT

  describe "RTree" $ do
    describe "Single-key" $ do
      it "insert"    $ run unary1 insertT
      it "insertGut" $ run unary1 insertGutT
      it "delete"    $ run unary1_ deleteT

    describe "Range" $ do
      it "adjustRangeWithKey/equals"       $ run unary1_ (adjustRangeWithKeyT  R.equals)
      it "adjustRangeWithKey/intersects"   $ run unary1_ (adjustRangeWithKeyT  R.intersects)
      it "adjustRangeWithKey'/equals"      $ run unary1_ (adjustRangeWithKeyT' R.equals)
      it "adjustRangeWithKey'/intersects"  $ run unary1_ (adjustRangeWithKeyT' R.intersects)

      it "foldlRangeWithKey/equals"        $ run unary1_ (foldlRangeWithKeyT  R.equals)
      it "foldlRangeWithKey/intersects"    $ run unary1_ (foldlRangeWithKeyT  R.intersects)
      it "foldlRangeWithKey'/equals"       $ run unary1_ (foldlRangeWithKeyT' R.equals)
      it "foldlRangeWithKey'/intersects"   $ run unary1_ (foldlRangeWithKeyT' R.intersects)

      it "foldrRangeWithKey/equals"        $ run unary1_ (foldrRangeWithKeyT  R.equals)
      it "foldrRangeWithKey/intersects"    $ run unary1_ (foldrRangeWithKeyT  R.intersects)
      it "foldrRangeWithKey'/equals"       $ run unary1_ (foldrRangeWithKeyT' R.equals)
      it "foldrRangeWithKey'/intersects"   $ run unary1_ (foldrRangeWithKeyT' R.intersects)

      it "foldMapRangeWithKey/equals"      $ run unary1_ (foldMapRangeWithKeyT  R.equals)
      it "foldMapRangeWithKey/intersects"  $ run unary1_ (foldMapRangeWithKeyT  R.intersects)

      it "traverseRangeWithKey/equals"     $ run unary1_ (traverseRangeWithKeyT  R.equals)
      it "traverseRangeWithKey/intersects" $ run unary1_ (traverseRangeWithKeyT  R.intersects)

    describe "Full tree" $ do
      it "map"             $ run unary0 mapT
      it "map'"            $ run unary0 mapT'
      it "mapWithKey"      $ run unary0 mapWithKeyT
      it "mapWithKey'"     $ run unary0 mapWithKeyT'

      it "foldl"           $ run unary0 foldlT
      it "foldl'"          $ run unary0 foldlT'
      it "foldlWithKey"    $ run unary0 foldlWithKeyT
      it "foldlWithKey'"   $ run unary0 foldlWithKeyT'

      it "foldr"           $ run unary0 foldrT
      it "foldr'"          $ run unary0 foldrT'
      it "foldrWithKey"    $ run unary0 foldrWithKeyT
      it "foldrWithKey'"   $ run unary0 foldrWithKeyT'

      it "foldMap"         $ run unary0 foldMapT
      it "foldMapWithKey"  $ run unary0 foldMapWithKeyT

      it "traverse"        $ run unary0 traverseT
      it "traverseWithKey" $ run unary0 traverseWithKeyT
