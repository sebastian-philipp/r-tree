{-# LANGUAGE RankNTypes #-}

module Test.RTree.Double.Strict
  ( rtreeT
  ) where

import           Data.NoTree.Strict (NoTree)
import qualified Data.NoTree.Strict as No
import qualified Data.RTree.Double.Strict as R
import           Data.RTree.Double.Strict.Debug
import           Data.RTree.Double.Strict.Unsafe

import           Control.Exception
import           Data.List
import           System.Random
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



randMBR :: RandomGen g => Int -> g -> (MBR, g)
randMBR s g0 =
  let (x0, g1) = uniformR (-s, s) g0
      (y0, g2) = uniformR (-s, s) g1
      (x1, g3) = uniformR (-s, s) g2
      (y1, g4) = uniformR (-s, s) g3

  in (MBR (fromIntegral x0) (fromIntegral y0) (fromIntegral x1) (fromIntegral y1), g4)



randMBRs :: RandomGen g => Int -> Int -> g -> ([MBR], g)
randMBRs s = go
  where
    go n g
      | n <= 0    = ([], g)
      | otherwise = let ~(ba, g') = randMBR s g
                        ~(as, g'') = go (n - 1) g'
                    in (ba:as, g'')


fromList :: [(MBR, a)] -> RTree a
fromList = foldr (uncurry R.insert) R.empty

toList :: RTree a -> [(MBR, a)]
toList = R.foldrWithKey (\ba a -> (:) (ba, a)) []



compareMBR :: (MBR, Int) -> (MBR, Int) -> Ordering
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



insertT :: (forall a. MBR -> a -> RTree a -> RTree a) -> StdGen -> Maybe (Int, String)
insertT f g =
  let (xs, _) = randMBRs 1024 1024 g
  in go 0 No.empty R.empty xs
  where
    go n no r (x:xs) =
      let no' = No.insert x n no
          r' = f x n r

      in if not $ isProper r'
           then Just (n, "not proper")
           else
             case depth r' of
               Nothing -> Just (n, "unbalanced")
               Just _  ->
                 if sortBy compareMBR (No.toList no') /= sortBy compareMBR (toList r')
                   then Just (n, "element mismatch")
                   else go (n + 1) no' r' xs

    go _ _  _    []  = Nothing



deleteT :: StdGen -> Maybe (Int, String)
deleteT g =
  let (xs, _) = randMBRs 1024 1024 g
      raw     = zip xs [0 :: Int ..]
  in go 0 (No.fromList raw) (fromList raw) xs
  where
    go n no r (x:xs) =
      let no' = No.delete x no
          r' = R.delete x r

      in if not $ isProper r'
           then Just (n, "not proper")
           else
             case depth r' of
               Nothing -> Just (n, "unbalanced")
               Just _  ->
                 let noes = sortBy compareMBR (No.toList no')
                     rs   = sortBy compareMBR (toList r')
                 in if noes /= rs
                      then Just (n, "element mismatch: " <> show rs <> " /= " <> show noes)
                      else go (n + 1) no' r' xs

    go _ _  r    []
      | null r    = Nothing
      | otherwise = Just (0, "not empty")



foldT
  :: (forall a. NoTree a -> [a])
  -> (forall a. RTree a -> [a])
  -> StdGen
  -> Maybe String
foldT noF rF g =
  let (xs, _) = randMBRs 1024 4096 g
      raw     = zip xs [0 :: Int ..]

      noes = noF $ No.fromList raw
      rs   = rF $ fromList raw

  in if sort noes == sort rs
       then Nothing
       else Just $ "element mismatch: " <> show rs <> " /= " <> show noes

foldWithKeyT
  :: (forall a. NoTree a -> [(MBR, a)])
  -> (forall a. RTree a -> [(MBR, a)])
  -> StdGen
  -> Maybe String
foldWithKeyT noF rF g =
  let (xs, _) = randMBRs 1024 4096 g
      raw     = zip xs [0 :: Int ..]

      noes = noF $ No.fromList raw
      rs   = rF $ fromList raw

  in if sortBy compareMBR noes == sortBy compareMBR rs
       then Nothing
       else Just $ "element mismatch: " <> show rs <> " /= " <> show noes



compress :: MBR -> Int
compress (UnsafeMBR xmin ymin xmax ymax) =
  truncate xmin + truncate ymin + truncate xmax + truncate ymax

mapT
  :: (NoTree Int -> NoTree Int)
  -> (RTree Int -> RTree Int)
  -> StdGen
  -> Maybe String
mapT noF rF g =
  let (xs, _) = randMBRs 1024 4096 g
      raw     = zip xs [0 :: Int ..]

      noes = sortBy compareMBR . No.toList . noF $ No.fromList raw
      rs   = sortBy compareMBR . toList . rF $ fromList raw

  in if noes == rs
       then Nothing
       else Just $ "element mismatch: " <> show rs <> " /= " <> show noes

traverseT
  :: (forall f. Applicative f => NoTree Int -> f (NoTree Int))
  -> (forall f. Applicative f => RTree Int -> f (RTree Int))
  -> StdGen
  -> IO (Maybe String)
traverseT noF rF g = do
  let (xs, _) = randMBRs 1024 4096 g
      raw     = zip xs [0 :: Int ..]

  no <- noF $ No.fromList raw
  r  <- rF $ fromList raw

  let noes = sortBy compareMBR $ No.toList no
      rs   = sortBy compareMBR $ toList r

  pure $ if noes == rs
           then Nothing
           else Just $ "element mismatch: " <> show rs <> " /= " <> show noes



wildcard :: Predicate
wildcard = Predicate (\_ -> True) (\_ -> True)



data Anvil = Anvil
             deriving Show

instance Exception Anvil

lazyMapT :: (forall a. RTree a -> RTree a) -> StdGen -> IO Bool
lazyMapT f g =
  let (xs, _) = randMBRs 1024 4096 g
      r   = fromList $ zip xs [0 :: Int ..]

  in (f r `seq` pure True) `catch` (\Anvil -> pure False)

strictMapT :: (forall a. RTree a -> RTree a) -> StdGen -> IO Bool
strictMapT f g =
  let (xs, _) = randMBRs 1024 4096 g
      r   = fromList $ zip xs [0 :: Int ..]

  in (f r `seq` pure False) `catch` (\Anvil -> pure True)



rtreeT :: Spec
rtreeT = do
  describe "MBR"
    mbrT

  describe "Predicate"
    predicateT

  describe "RTree" $ do
    it "insert" $
      insertT R.insert (mkStdGen 0) `shouldBe` Nothing

    it "insertGut" $
      insertT R.insertGut (mkStdGen 1) `shouldBe` Nothing

    it "delete" $
      deleteT (mkStdGen 2) `shouldBe` Nothing

    describe "fold" $ do
      it "l" $
        foldT
          (No.foldlRangeWithKey wildcard (\z _ a -> a:z) [])
          (R.foldl (flip (:)) [])
          (mkStdGen 10)
          `shouldBe` Nothing

      it "l'" $
        foldT
          (No.foldlRangeWithKey wildcard (\z _ a -> a:z) [])
          (R.foldl' (flip (:)) [])
          (mkStdGen 11)
          `shouldBe` Nothing

      it "lWithKey" $
        foldWithKeyT
          (No.foldlRangeWithKey wildcard (\z ba a -> (ba, a):z) [])
          (R.foldlWithKey (\z ba a -> (ba, a):z) [])
          (mkStdGen 12)
          `shouldBe` Nothing

      it "lWithKey'" $
        foldWithKeyT
          (No.foldlRangeWithKey wildcard (\z ba a -> (ba, a):z) [])
          (R.foldlWithKey' (\z ba a -> (ba, a):z) [])
          (mkStdGen 13)
          `shouldBe` Nothing

      describe "lRangeWithKey" $ do
        it "equals" $
          let ~(bx, g') = randMBR 1024 (mkStdGen 14)
          in foldWithKeyT
               (No.foldlRangeWithKey (R.equals bx) (\z ba a -> (ba, a):z) [])
               (R.foldlRangeWithKey (R.equals bx) (\z ba a -> (ba, a):z) [])
               g'
               `shouldBe` Nothing

        it "intersects" $
          let ~(bx, g') = randMBR 1024 (mkStdGen 15)
          in foldWithKeyT
               (No.foldlRangeWithKey (R.intersects bx) (\z ba a -> (ba, a):z) [])
               (R.foldlRangeWithKey (R.intersects bx) (\z ba a -> (ba, a):z) [])
               g'
               `shouldBe` Nothing

        it "contains" $
          let ~(bx, g') = randMBR 1024 (mkStdGen 16)
          in foldWithKeyT
               (No.foldlRangeWithKey (R.contains bx) (\z ba a -> (ba, a):z) [])
               (R.foldlRangeWithKey (R.contains bx) (\z ba a -> (ba, a):z) [])
               g'
               `shouldBe` Nothing

        it "containedBy" $
          let ~(bx, g') = randMBR 1024 (mkStdGen 17)
          in foldWithKeyT
               (No.foldlRangeWithKey (R.containedBy bx) (\z ba a -> (ba, a):z) [])
               (R.foldlRangeWithKey (R.containedBy bx) (\z ba a -> (ba, a):z) [])
               g'
               `shouldBe` Nothing

      describe "lRangeWithKey'" $ do
        it "equals" $
          let ~(bx, g') = randMBR 1024 (mkStdGen 18)
          in foldWithKeyT
               (No.foldlRangeWithKey (R.equals bx) (\z ba a -> (ba, a):z) [])
               (R.foldlRangeWithKey' (R.equals bx) (\z ba a -> (ba, a):z) [])
               g'
               `shouldBe` Nothing

        it "intersects" $
          let ~(bx, g') = randMBR 1024 (mkStdGen 19)
          in foldWithKeyT
               (No.foldlRangeWithKey (R.intersects bx) (\z ba a -> (ba, a):z) [])
               (R.foldlRangeWithKey' (R.intersects bx) (\z ba a -> (ba, a):z) [])
               g'
               `shouldBe` Nothing

        it "contains" $
          let ~(bx, g') = randMBR 1024 (mkStdGen 20)
          in foldWithKeyT
               (No.foldlRangeWithKey (R.contains bx) (\z ba a -> (ba, a):z) [])
               (R.foldlRangeWithKey' (R.contains bx) (\z ba a -> (ba, a):z) [])
               g'
               `shouldBe` Nothing

        it "containedBy" $
          let ~(bx, g') = randMBR 1024 (mkStdGen 21)
          in foldWithKeyT
               (No.foldlRangeWithKey (R.containedBy bx) (\z ba a -> (ba, a):z) [])
               (R.foldlRangeWithKey' (R.containedBy bx) (\z ba a -> (ba, a):z) [])
               g'
               `shouldBe` Nothing

      it "r" $
        foldT
          (No.foldrRangeWithKey wildcard (\_ -> (:)) [])
          (R.foldr (:) [])
          (mkStdGen 30)
          `shouldBe` Nothing

      it "r'" $
        foldT
          (No.foldrRangeWithKey wildcard (\_ -> (:)) [])
          (R.foldr' (:) [])
          (mkStdGen 31)
          `shouldBe` Nothing

      it "rWithKey" $
        foldWithKeyT
          (No.foldrRangeWithKey wildcard (\ba a -> (:) (ba, a)) [])
          (R.foldrWithKey (\ba a -> (:) (ba, a)) [])
          (mkStdGen 32)
          `shouldBe` Nothing

      it "rWithKey'" $
        foldWithKeyT
          (No.foldrRangeWithKey wildcard (\ba a -> (:) (ba, a)) [])
          (R.foldrWithKey' (\ba a -> (:) (ba, a)) [])
          (mkStdGen 33)
          `shouldBe` Nothing

      describe "rRangeWithKey" $ do
        it "equals" $
          let ~(bx, g') = randMBR 1024 (mkStdGen 34)
          in foldWithKeyT
               (No.foldrRangeWithKey (R.equals bx) (\ba a -> (:) (ba, a)) [])
               (R.foldrRangeWithKey (R.equals bx) (\ba a -> (:) (ba, a)) [])
               g'
               `shouldBe` Nothing

        it "intersects" $
          let ~(bx, g') = randMBR 1024 (mkStdGen 35)
          in foldWithKeyT
               (No.foldrRangeWithKey (R.intersects bx) (\ba a -> (:) (ba, a)) [])
               (R.foldrRangeWithKey (R.intersects bx) (\ba a -> (:) (ba, a)) [])
               g'
               `shouldBe` Nothing

        it "contains" $
          let ~(bx, g') = randMBR 1024 (mkStdGen 36)
          in foldWithKeyT
               (No.foldrRangeWithKey (R.contains bx) (\ba a -> (:) (ba, a)) [])
               (R.foldrRangeWithKey (R.contains bx) (\ba a -> (:) (ba, a)) [])
               g'
               `shouldBe` Nothing

        it "containedBy" $
          let ~(bx, g') = randMBR 1024 (mkStdGen 37)
          in foldWithKeyT
               (No.foldrRangeWithKey (R.containedBy bx) (\ba a -> (:) (ba, a)) [])
               (R.foldrRangeWithKey (R.containedBy bx) (\ba a -> (:) (ba, a)) [])
               g'
               `shouldBe` Nothing

      describe "rRangeWithKey'" $ do
        it "equals" $
          let ~(bx, g') = randMBR 1024 (mkStdGen 38)
          in foldWithKeyT
               (No.foldrRangeWithKey (R.equals bx) (\ba a -> (:) (ba, a)) [])
               (R.foldrRangeWithKey' (R.equals bx) (\ba a -> (:) (ba, a)) [])
               g'
               `shouldBe` Nothing

        it "intersects" $
          let ~(bx, g') = randMBR 1024 (mkStdGen 39)
          in foldWithKeyT
               (No.foldrRangeWithKey (R.intersects bx) (\ba a -> (:) (ba, a)) [])
               (R.foldrRangeWithKey' (R.intersects bx) (\ba a -> (:) (ba, a)) [])
               g'
               `shouldBe` Nothing

        it "contains" $
          let ~(bx, g') = randMBR 1024 (mkStdGen 40)
          in foldWithKeyT
               (No.foldrRangeWithKey (R.contains bx) (\ba a -> (:) (ba, a)) [])
               (R.foldrRangeWithKey' (R.contains bx) (\ba a -> (:) (ba, a)) [])
               g'
               `shouldBe` Nothing

        it "containedBy" $
          let ~(bx, g') = randMBR 1024 (mkStdGen 41)
          in foldWithKeyT
               (No.foldrRangeWithKey (R.containedBy bx) (\ba a -> (:) (ba, a)) [])
               (R.foldrRangeWithKey' (R.containedBy bx) (\ba a -> (:) (ba, a)) [])
               g'
               `shouldBe` Nothing

      it "Map" $
        foldT
          (No.foldMapRangeWithKey wildcard (\_ -> pure))
          (R.foldMap (:[]))
          (mkStdGen 50)
          `shouldBe` Nothing

      it "MapWithKey" $
        foldWithKeyT
          (No.foldMapRangeWithKey wildcard (\ba a -> [(ba, a)]))
          (R.foldMapWithKey (\ba a -> [(ba, a)]))
          (mkStdGen 51)
          `shouldBe` Nothing

      describe "MapRangeWithKey" $ do
        it "equals" $
          let ~(bx, g') = randMBR 1024 (mkStdGen 52)
          in foldWithKeyT
               (No.foldMapRangeWithKey (R.equals bx) (\ba a -> [(ba, a)]))
               (R.foldMapRangeWithKey (R.equals bx) (\ba a -> [(ba, a)]))
               g'
               `shouldBe` Nothing

        it "intersects" $
          let ~(bx, g') = randMBR 1024 (mkStdGen 53)
          in foldWithKeyT
               (No.foldMapRangeWithKey (R.intersects bx) (\ba a -> [(ba, a)]))
               (R.foldMapRangeWithKey (R.intersects bx) (\ba a -> [(ba, a)]))
               g'
               `shouldBe` Nothing

        it "contains" $
          let ~(bx, g') = randMBR 1024 (mkStdGen 54)
          in foldWithKeyT
               (No.foldMapRangeWithKey (R.contains bx) (\ba a -> [(ba, a)]))
               (R.foldMapRangeWithKey (R.contains bx) (\ba a -> [(ba, a)]))
               g'
               `shouldBe` Nothing

        it "containedBy" $
          let ~(bx, g') = randMBR 1024 (mkStdGen 55)
          in foldWithKeyT
               (No.foldMapRangeWithKey (R.containedBy bx) (\ba a -> [(ba, a)]))
               (R.foldMapRangeWithKey (R.containedBy bx) (\ba a -> [(ba, a)]))
               g'
               `shouldBe` Nothing

    describe "map" $ do
      it "^" $
        mapT
          (No.mapRangeWithKey wildcard (\_ a -> a + 1))
          (R.map (+1))
          (mkStdGen 60)
          `shouldBe` Nothing

      it "'" $
        mapT
          (No.mapRangeWithKey wildcard (\_ a -> a + 1))
          (R.map (+1))
          (mkStdGen 61)
          `shouldBe` Nothing

      it "WithKey" $
        mapT
          (No.mapRangeWithKey wildcard (\ba a -> compress ba + a + 1))
          (R.mapWithKey (\ba a -> compress ba + a + 1))
          (mkStdGen 62)
          `shouldBe` Nothing

      it "WithKey'" $
        mapT
          (No.mapRangeWithKey wildcard (\ba a -> compress ba + a + 1))
          (R.mapWithKey' (\ba a -> compress ba + a + 1))
          (mkStdGen 63)
          `shouldBe` Nothing

      describe "RangeWithKey" $ do
        it "equals" $
          let ~(bx, g') = randMBR 1024 (mkStdGen 64)
          in mapT
               (No.mapRangeWithKey (R.equals bx) (\ba a -> compress ba + a + 1))
               (R.mapRangeWithKey (R.equals bx) (\ba a -> compress ba + a + 1))
               g'
               `shouldBe` Nothing

        it "intersects" $
          let ~(bx, g') = randMBR 1024 (mkStdGen 65)
          in mapT
               (No.mapRangeWithKey (R.intersects bx) (\ba a -> compress ba + a + 1))
               (R.mapRangeWithKey (R.intersects bx) (\ba a -> compress ba + a + 1))
               g'
               `shouldBe` Nothing

        it "contains" $
          let ~(bx, g') = randMBR 1024 (mkStdGen 66)
          in mapT
               (No.mapRangeWithKey (R.contains bx) (\ba a -> compress ba + a + 1))
               (R.mapRangeWithKey (R.contains bx) (\ba a -> compress ba + a + 1))
               g'
               `shouldBe` Nothing

        it "containedBy" $
          let ~(bx, g') = randMBR 1024 (mkStdGen 67)
          in mapT
               (No.mapRangeWithKey (R.containedBy bx) (\ba a -> compress ba + a + 1))
               (R.mapRangeWithKey (R.containedBy bx) (\ba a -> compress ba + a + 1))
               g'
               `shouldBe` Nothing

      describe "RangeWithKey'" $ do
        it "equals" $
          let ~(bx, g') = randMBR 1024 (mkStdGen 68)
          in mapT
               (No.mapRangeWithKey (R.equals bx) (\ba a -> compress ba + a + 1))
               (R.mapRangeWithKey' (R.equals bx) (\ba a -> compress ba + a + 1))
               g'
               `shouldBe` Nothing

        it "intersects" $
          let ~(bx, g') = randMBR 1024 (mkStdGen 69)
          in mapT
               (No.mapRangeWithKey (R.intersects bx) (\ba a -> compress ba + a + 1))
               (R.mapRangeWithKey' (R.intersects bx) (\ba a -> compress ba + a + 1))
               g'
               `shouldBe` Nothing

        it "contains" $
          let ~(bx, g') = randMBR 1024 (mkStdGen 70)
          in mapT
               (No.mapRangeWithKey (R.contains bx) (\ba a -> compress ba + a + 1))
               (R.mapRangeWithKey' (R.contains bx) (\ba a -> compress ba + a + 1))
               g'
               `shouldBe` Nothing

        it "containedBy" $
          let ~(bx, g') = randMBR 1024 (mkStdGen 71)
          in mapT
               (No.mapRangeWithKey (R.containedBy bx) (\ba a -> compress ba + a + 1))
               (R.mapRangeWithKey' (R.containedBy bx) (\ba a -> compress ba + a + 1))
               g'
               `shouldBe` Nothing

    describe "traverse" $ do
      it "^" $
        traverseT
          (No.traverseRangeWithKey wildcard (\_ a -> pure $ a + 1))
          (R.traverse (pure . (+1)))
          (mkStdGen 80)
          `shouldReturn` Nothing

      it "WithKey" $
        traverseT
          (No.traverseRangeWithKey wildcard (\ba a -> pure $ compress ba + a + 1))
          (R.traverseWithKey (\ba a -> pure $ compress ba + a + 1))
          (mkStdGen 81)
          `shouldReturn` Nothing

      describe "RangeWithKey" $ do
        it "equals" $
          let ~(bx, g') = randMBR 1024 (mkStdGen 64)
          in traverseT
               (No.traverseRangeWithKey (R.equals bx) (\ba a -> pure $ compress ba + a + 1))
               (R.traverseRangeWithKey (R.equals bx) (\ba a -> pure $ compress ba + a + 1))
               g'
               `shouldReturn` Nothing

        it "intersects" $
          let ~(bx, g') = randMBR 1024 (mkStdGen 65)
          in traverseT
               (No.traverseRangeWithKey (R.intersects bx) (\ba a -> pure $ compress ba + a + 1))
               (R.traverseRangeWithKey (R.intersects bx) (\ba a -> pure $ compress ba + a + 1))
               g'
               `shouldReturn` Nothing

        it "contains" $
          let ~(bx, g') = randMBR 1024 (mkStdGen 66)
          in traverseT
               (No.traverseRangeWithKey (R.contains bx) (\ba a -> pure $ compress ba + a + 1))
               (R.traverseRangeWithKey (R.contains bx) (\ba a -> pure $ compress ba + a + 1))
               g'
               `shouldReturn` Nothing

        it "containedBy" $
          let ~(bx, g') = randMBR 1024 (mkStdGen 67)
          in traverseT
               (No.traverseRangeWithKey (R.containedBy bx) (\ba a -> pure $ compress ba + a + 1))
               (R.traverseRangeWithKey (R.containedBy bx) (\ba a -> pure $ compress ba + a + 1))
               g'
               `shouldReturn` Nothing

  describe "strictness" $ do
    it "map" $
      lazyMapT (R.map (\_ -> throw Anvil)) (mkStdGen 90) `shouldReturn` True

    it "map'" $
      strictMapT (R.map' (\_ -> throw Anvil)) (mkStdGen 91) `shouldReturn` True

    it "mapWithKey" $
      lazyMapT (R.mapWithKey (\_ _ -> throw Anvil)) (mkStdGen 92) `shouldReturn` True

    it "mapWithKey'" $
      strictMapT (R.mapWithKey' (\_ _ -> throw Anvil)) (mkStdGen 93) `shouldReturn` True

    it "mapRangeWithKey" $
      lazyMapT (R.mapRangeWithKey wildcard (\_ _ -> throw Anvil)) (mkStdGen 94)
        `shouldReturn` True

    it "mapRangeWithKey'" $
      strictMapT (R.mapRangeWithKey' wildcard (\_ _ -> throw Anvil)) (mkStdGen 95)
        `shouldReturn` True
