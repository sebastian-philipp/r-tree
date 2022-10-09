{-# LANGUAGE CPP #-}

module Main where

import           Data.NoTree (NoTree)
import qualified Data.NoTree as No
import           Data.RTree.Internal (equideep)
import           Data.RTree.Lazy (MBR (..), Predicate, RTree)
import qualified Data.RTree.Lazy as R
import qualified Data.RTree.Strict as RS

import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Data.Foldable
import           Data.Maybe
import           Data.Monoid
import           Prelude hiding (length, lookup)
import           System.Random.Stateful
import           Test.Hspec



randMBR :: StatefulGen g m => Int -> g -> m (MBR Int)
randMBR n g = do
  x1 <- uniformRM (0, n) g
  y1 <- uniformRM (0, n) g
  x2 <- uniformRM (0, n) g
  y2 <- uniformRM (0, n) g
  return $ MBR (x1 `min` x2) (y1 `min` y2) (x1 `max` x2) (y1 `max` y2)



excluding :: Monad m => (MBR r -> Predicate r) -> NoTree r a -> m (MBR r) -> m (MBR r)
excluding crit no f = do
  ba <- f
  case getFirst $ No.foldMap (crit ba) (First . Just) no of
    Nothing -> return ba
    Just _  -> excluding crit no f

uniqueIndex :: (Monad m, Ord r) => NoTree r a -> m Int -> m Int
uniqueIndex no f = do
  i <- f
  if No.foldMap (R.equals . fst $ No.toList no !! i) (const $ Sum 1) no < Sum (2 :: Int)
    then return i
    else uniqueIndex no f



anyPredicate :: (Ord r, StatefulGen g m) => g -> m (MBR r -> Predicate r)
anyPredicate g = do
  let lst = [ R.equals, R.intersects, R.intersects'
            , R.contains, R.contains', R.within, R.within' ]
  i <- uniformRM (0, length lst - 1) g
  return $ lst !! i



coordSum :: MBR Int -> Int -> Int
coordSum (MBR xmin ymin xmax ymax) a = xmin + ymin + xmax + ymax + a + 10

coordSumA :: Applicative f => MBR Int -> Int -> f Int
coordSumA ba = pure . coordSum ba



soup :: Bool -> Int -> SpecWith (RTree Int Int, NoTree Int Int, IOGenM StdGen)
soup zero n = do
  describe "insert" $ do
    describe "lazy" $ do
      it "nonexistent" $ \(r, no, g) ->
        replicateM_ 10 $ do
          ba <- excluding R.equals no $ randMBR (n - 1) g
          a <- uniformRM (0, n - 1) g
          R.toList (R.insert ba a r) `shouldMatchList` No.toList (No.insert ba a no)
  
      it "existing" $ \(r, no, g) -> do
        when zero $ pendingWith "Nonapplicable"
        replicateM_ 10 $ do
          i <- uniformRM (0, n - 1) g
          let (ba, a) = No.toList no !! i
          R.toList (R.insert ba a r) `shouldMatchList` No.toList (No.insert ba a no)
  
      it "10 copies" $ \(r, no, g) ->
        replicateM_ 10 $ do
          ba <- randMBR (n - 1) g
          as <- replicateM 10 $ uniformRM (0, 1023) g
          R.toList (foldr (R.insert ba) r as)
            `shouldMatchList` No.toList (foldr (No.insert ba) no as)

    describe "strict" $ do
      it "nonexistent" $ \(r, no, g) ->
        replicateM_ 10 $ do
          ba <- excluding R.equals no $ randMBR (n - 1) g
          a <- uniformRM (0, n - 1) g
          R.toList (RS.insert ba a r) `shouldMatchList` No.toList (No.insert ba a no)
  
      it "existing" $ \(r, no, g) -> do
        when zero $ pendingWith "Nonapplicable"
        replicateM_ 10 $ do
          i <- uniformRM (0, n - 1) g
          let (ba, a) = No.toList no !! i
          R.toList (RS.insert ba a r) `shouldMatchList` No.toList (No.insert ba a no)
  
      it "10 copies" $ \(r, no, g) ->
        replicateM_ 10 $ do
          ba <- randMBR (n - 1) g
          as <- replicateM 10 $ uniformRM (0, 1023) g
          R.toList (foldr (RS.insert ba) r as)
            `shouldMatchList` No.toList (foldr (No.insert ba) no as)

  describe "insertGut" $ do
    describe "lazy" $ do
      it "nonexistent" $ \(r, no, g) ->
        replicateM_ 10 $ do
          ba <- excluding R.equals no $ randMBR (n - 1) g
          a <- uniformRM (0, n - 1) g
          R.toList (R.insertGut ba a r) `shouldMatchList` No.toList (No.insert ba a no)
  
      it "existing" $ \(r, no, g) -> do
        when zero $ pendingWith "Nonapplicable"
        replicateM_ 10 $ do
          i <- uniformRM (0, n - 1) g
          let (ba, a) = No.toList no !! i
          R.toList (R.insertGut ba a r) `shouldMatchList` No.toList (No.insert ba a no)
  
      it "10 copies" $ \(r, no, g) ->
        replicateM_ 10 $ do
          ba <- randMBR (n - 1) g
          as <- replicateM 10 $ uniformRM (0, n - 1) g
          R.toList (foldr (R.insertGut ba) r as)
            `shouldMatchList` No.toList (foldr (No.insert ba) no as)

    describe "strict" $ do
      it "nonexistent" $ \(r, no, g) ->
        replicateM_ 10 $ do
          ba <- excluding R.equals no $ randMBR (n - 1) g
          a <- uniformRM (0, n - 1) g
          R.toList (RS.insertGut ba a r) `shouldMatchList` No.toList (No.insert ba a no)
  
      it "existing" $ \(r, no, g) -> do
        when zero $ pendingWith "Nonapplicable"
        replicateM_ 10 $ do
          i <- uniformRM (0, n - 1) g
          let (ba, a) = No.toList no !! i
          R.toList (RS.insertGut ba a r) `shouldMatchList` No.toList (No.insert ba a no)
  
      it "10 copies" $ \(r, no, g) ->
        replicateM_ 10 $ do
          ba <- randMBR (n - 1) g
          as <- replicateM 10 $ uniformRM (0, n - 1) g
          R.toList (foldr (RS.insertGut ba) r as)
            `shouldMatchList` No.toList (foldr (No.insert ba) no as)

  describe "delete" $ do
    it "nonexistent" $ \(r, no, g) ->
      replicateM_ 10 $ do
        ba <- excluding R.equals no $ randMBR (n - 1) g
        R.toList (R.delete ba r) `shouldMatchList` No.toList (No.delete ba no)
  
    it "existing" $ \(r, no, g) -> do
      when zero $ pendingWith "Nonapplicable"
      replicateM_ 10 $ do
        i <- uniqueIndex no $ uniformRM (0, n - 1) g
        let ba = fst $ No.toList no !! i
        R.toList (R.delete ba r) `shouldMatchList` No.toList (No.delete ba no)
  
    it "10 copies" $ \(r, no, g) ->
      replicateM_ 10 $ do
        ba <- excluding R.equals no $ randMBR (n - 1) g
        as <- replicate 10 <$> uniformRM (0, n - 1) g
        R.toList (R.delete ba $ foldr (R.insert ba) r as)
          `shouldMatchList` No.toList (No.delete ba $ foldr (No.insert ba) no as)

  describe "Functor" $ do
    it "fmap" $ \(r, no, _g) ->
      R.toList (fmap (+1024) r) `shouldMatchList` No.toList (fmap (+1024) no)
  
  describe "mapWithKey" $ do
    it "nonexistent" $ \(r, no, g) -> do
      replicateM_ 10 $ do
        pre <- anyPredicate g
        ba <- excluding pre no $ randMBR (n - 1) g
        R.toList (R.mapWithKey (pre ba) coordSum r) `shouldBe` R.toList r

    it "existing" $ \(r, no, g) -> do
      when zero $ pendingWith "Nonapplicable"
      replicateM_ 10 $ do
        i <- uniformRM (0, n - 1) g
        let ba = fst $ No.toList no !! i
        pre <- anyPredicate g
        R.toList (R.mapWithKey (pre ba) coordSum r)
         `shouldMatchList` No.toList (No.mapWithKey (pre ba) coordSum no)

    it "10 copies" $ \(r, no, g) -> do
      replicateM_ 10 $ do
        ba <- randMBR (n - 1) g
        as <- replicateM 10 $ uniformRM (0, n - 1) g
        pre <- anyPredicate g
        let r'  = foldr (R.insert ba) r as
            no' = foldr (No.insert ba) no as
        R.toList (R.mapWithKey (pre ba) coordSum r')
          `shouldMatchList` No.toList (No.mapWithKey (pre ba) coordSum no')

  describe "Foldable" $ do
    it "foldMap" $ \(r, no, _g) ->
      foldMap pure r `shouldMatchList` foldMap pure no
#if __GLASGOW_HASKELL__ >= 808
    it "foldMap'" $ \(r, no, _g) ->
      foldMap' pure r `shouldMatchList` foldMap' pure no
#endif
    it "foldr" $ \(r, no, _g) ->
      foldr (:) [] r `shouldMatchList` foldr (:) [] no

    it "foldr'" $ \(r, no, _g) ->
      foldr' (:) [] r `shouldMatchList` foldr' (:) [] no

    it "foldl" $ \(r, no, _g) ->
      foldl (flip (:)) [] r `shouldMatchList` foldl (flip (:)) [] no

    it "foldl'" $ \(r, no, _g) ->
      foldl' (flip (:)) [] r `shouldMatchList` foldl' (flip (:)) [] no

  describe "foldMapWithKey" $ do
    it "nonexistent" $ \(r, no, g) -> do
      replicateM_ 10 $ do
        pre <- anyPredicate g
        ba <- excluding pre no $ randMBR (n - 1) g
        R.foldMapWithKey (pre ba) coordSumA r `shouldBe` []

    it "existing" $ \(r, no, g) -> do
      when zero $ pendingWith "Nonapplicable"
      replicateM_ 10 $ do
        i <- uniformRM (0, n - 1) g
        let ba = fst $ No.toList no !! i
        pre <- anyPredicate g
        R.foldMapWithKey (pre ba) coordSumA r
          `shouldMatchList` No.foldMapWithKey (pre ba) coordSumA no

    it "10 copies" $ \(r, no, g) -> do
      replicateM_ 10 $ do
        ba <- randMBR (n - 1) g
        as <- replicateM 10 $ uniformRM (0, n - 1) g
        pre <- anyPredicate g
        let r'  = foldr (R.insert ba) r as
            no' = foldr (No.insert ba) no as
        R.foldMapWithKey (pre ba) coordSumA r'
          `shouldMatchList` No.foldMapWithKey (pre ba) coordSumA no'
#if __GLASGOW_HASKELL__ >= 808
  describe "foldMapWithKey'" $ do
    it "nonexistent" $ \(r, no, g) -> do
      replicateM_ 10 $ do
        pre <- anyPredicate g
        ba <- excluding pre no $ randMBR (n - 1) g
        R.foldMapWithKey' (pre ba) coordSumA r `shouldBe` []

    it "existing" $ \(r, no, g) -> do
      when zero $ pendingWith "Nonapplicable"
      replicateM_ 10 $ do
        i <- uniformRM (0, n - 1) g
        let ba = fst $ No.toList no !! i
        pre <- anyPredicate g
        R.foldMapWithKey' (pre ba) coordSumA r
          `shouldMatchList` No.foldMapWithKey' (pre ba) coordSumA no

    it "10 copies" $ \(r, no, g) -> do
      replicateM_ 10 $ do
        ba <- randMBR (n - 1) g
        as <- replicateM 10 $ uniformRM (0, n - 1) g
        pre <- anyPredicate g
        let r'  = foldr (R.insert ba) r as
            no' = foldr (No.insert ba) no as
        R.foldMapWithKey' (pre ba) coordSumA r'
          `shouldMatchList` No.foldMapWithKey' (pre ba) coordSumA no'
#endif
  describe "foldrWithKey" $ do
    it "nonexistent" $ \(r, no, g) -> do
      replicateM_ 10 $ do
        pre <- anyPredicate g
        ba <- excluding pre no $ randMBR (n - 1) g
        R.foldrWithKey (pre ba) ((.) mappend . coordSumA) [] r `shouldBe` []

    it "existing" $ \(r, no, g) -> do
      when zero $ pendingWith "Nonapplicable"
      replicateM_ 10 $ do
        i <- uniformRM (0, n - 1) g
        let ba = fst $ No.toList no !! i
        pre <- anyPredicate g
        R.foldrWithKey (pre ba) ((.) mappend . coordSumA) [] r
          `shouldMatchList` No.foldrWithKey (pre ba) ((.) mappend . coordSumA) [] no

    it "10 copies" $ \(r, no, g) -> do
      replicateM_ 10 $ do
        ba <- randMBR (n - 1) g
        as <- replicateM 10 $ uniformRM (0, n - 1) g
        pre <- anyPredicate g
        let r'  = foldr (R.insert ba) r as
            no' = foldr (No.insert ba) no as
        R.foldrWithKey (pre ba) ((.) mappend . coordSumA) [] r'
          `shouldMatchList` No.foldrWithKey (pre ba) ((.) mappend . coordSumA) [] no'

  describe "foldrWithKey'" $ do
    it "nonexistent" $ \(r, no, g) -> do
      replicateM_ 10 $ do
        pre <- anyPredicate g
        ba <- excluding pre no $ randMBR (n - 1) g
        R.foldrWithKey' (pre ba) ((.) mappend . coordSumA) [] r `shouldBe` []

    it "existing" $ \(r, no, g) -> do
      when zero $ pendingWith "Nonapplicable"
      replicateM_ 10 $ do
        i <- uniformRM (0, n - 1) g
        let ba = fst $ No.toList no !! i
        pre <- anyPredicate g
        R.foldrWithKey' (pre ba) ((.) mappend . coordSumA) [] r
          `shouldMatchList` No.foldrWithKey' (pre ba) ((.) mappend . coordSumA) [] no

    it "10 copies" $ \(r, no, g) -> do
      replicateM_ 10 $ do
        ba <- randMBR (n - 1) g
        as <- replicateM 10 $ uniformRM (0, n - 1) g
        pre <- anyPredicate g
        let r'  = foldr (R.insert ba) r as
            no' = foldr (No.insert ba) no as
        R.foldrWithKey' (pre ba) ((.) mappend . coordSumA) [] r'
          `shouldMatchList` No.foldrWithKey' (pre ba) ((.) mappend . coordSumA) [] no'

  describe "foldlWithKey" $ do
    let f acc ba a = coordSum ba a : acc
    it "nonexistent" $ \(r, no, g) -> do
      replicateM_ 10 $ do
        pre <- anyPredicate g
        ba <- excluding pre no $ randMBR (n - 1) g
        R.foldlWithKey (pre ba) f [] r `shouldBe` []

    it "existing" $ \(r, no, g) -> do
      when zero $ pendingWith "Nonapplicable"
      replicateM_ 10 $ do
        i <- uniformRM (0, n - 1) g
        let ba = fst $ No.toList no !! i
        pre <- anyPredicate g
        R.foldlWithKey (pre ba) f [] r
          `shouldMatchList` No.foldlWithKey (pre ba) f [] no

    it "10 copies" $ \(r, no, g) -> do
      replicateM_ 10 $ do
        ba <- randMBR (n - 1) g
        as <- replicateM 10 $ uniformRM (0, n - 1) g
        pre <- anyPredicate g
        let r'  = foldr (R.insert ba) r as
            no' = foldr (No.insert ba) no as
        R.foldlWithKey (pre ba) f [] r'
          `shouldMatchList` No.foldlWithKey (pre ba) f [] no'

  describe "foldlWithKey'" $ do
    let f acc ba a = coordSum ba a : acc
    it "nonexistent" $ \(r, no, g) -> do
      replicateM_ 10 $ do
        pre <- anyPredicate g
        ba <- excluding pre no $ randMBR (n - 1) g
        R.foldlWithKey' (pre ba) f [] r `shouldBe` []

    it "existing" $ \(r, no, g) -> do
      when zero $ pendingWith "Nonapplicable"
      replicateM_ 10 $ do
        i <- uniformRM (0, n - 1) g
        let ba = fst $ No.toList no !! i
        pre <- anyPredicate g
        R.foldlWithKey' (pre ba) f [] r
          `shouldMatchList` No.foldlWithKey' (pre ba) f [] no

    it "10 copies" $ \(r, no, g) -> do
      replicateM_ 10 $ do
        ba <- randMBR (n - 1) g
        as <- replicateM 10 $ uniformRM (0, n - 1) g
        pre <- anyPredicate g
        let r'  = foldr (R.insert ba) r as
            no' = foldr (No.insert ba) no as
        R.foldlWithKey' (pre ba) f [] r'
          `shouldMatchList` No.foldlWithKey' (pre ba) f [] no'

  describe "Traversable" $
    it "traverse" $ \(r, no, _g) -> do
      r' <- traverse (pure . (+) 1024) r
      no' <- traverse (pure . (+) 1024) no
      R.toList r' `shouldMatchList` No.toList no'

  describe "traverseWithKey" $ do
    it "nonexistent" $ \(r, no, g) -> do
      replicateM_ 10 $ do
        pre <- anyPredicate g
        ba <- excluding pre no $ randMBR (n - 1) g
        r' <- R.traverseWithKey (pre ba) coordSumA r
        R.toList r' `shouldMatchList` R.toList r'

    it "existing" $ \(r, no, g) -> do
      when zero $ pendingWith "Nonapplicable"
      replicateM_ 10 $ do
        i <- uniformRM (0, n - 1) g
        let ba = fst $ No.toList no !! i
        pre <- anyPredicate g
        r' <- R.traverseWithKey (pre ba) coordSumA r
        no' <- No.traverseWithKey (pre ba) coordSumA no
        R.toList r' `shouldMatchList` No.toList no'

    it "10 copies" $ \(r, no, g) -> do
      replicateM_ 10 $ do
        ba <- randMBR (n - 1) g
        as <- replicateM 10 $ uniformRM (0, n - 1) g
        pre <- anyPredicate g
        let r'  = foldr (R.insert ba) r as
            no' = foldr (No.insert ba) no as
        r''  <- R.traverseWithKey (pre ba) coordSumA r'
        no'' <- No.traverseWithKey (pre ba) coordSumA no'
        R.toList r'' `shouldMatchList` No.toList no''



main :: IO ()
main =
  hspec $ do
    describe "unique/fromList/0" $
      beforeAll ((,,) R.empty No.empty <$> newIOGenM (mkStdGen 0)) $ soup True 0

    describe "unique/fromList/1" $
      let prep = do
            g <- newIOGenM $ mkStdGen 0
            br <- randMBR 1023 g
            let r  = R.singleton br 0
                no = No.singleton br 0
            _ <- evaluate $ force (r, no)
            return $ (r, no, g)

      in beforeAll prep $ soup False 1

    describe "unique/fromList/4" $
      let n = 4

          prep = do
            g <- newIOGenM $ mkStdGen 0
            brs <- replicateM n $ randMBR 1023 g
            let as = zip brs [0 :: Int ..]
                r  = R.fromList as
                no = No.fromList as
            _ <- evaluate $ force (r, no)
            return $ (r, no, g)

      in beforeAll prep $ soup False n

    describe "unique/fromList/32" $
      let n = 32

          prep = do
            g <- newIOGenM $ mkStdGen 0
            brs <- replicateM n $ randMBR 1023 g
            let as = zip brs [0 :: Int ..]
                r  = R.fromList as
                no = No.fromList as
            _ <- evaluate $ force (r, no)
            return $ (r, no, g)

      in beforeAll prep $ soup False n

    describe "unique/bulkSTR/128" $
      let n = 128

          prep = do
            g <- newIOGenM $ mkStdGen 0
            brs <- replicateM n $ randMBR 1023 g
            let as = zip brs [0 :: Int ..]
                r  = R.bulkSTR as
                no = No.fromList as
            _ <- evaluate $ force (r, no)
            return $ (r, no, g)

      in beforeAll prep $ soup False n

    describe "overlaps/fromList/128" $
      let n = 128

          prep = do
            g <- newIOGenM $ mkStdGen 0
            brs <- replicateM n $ randMBR 7 g
            let as = zip brs [0 :: Int ..]
                r  = R.fromList as
                no = No.fromList as
            _ <- evaluate $ force (r, no)
            return $ (r, no, g)

      in beforeAll prep $ soup False n

    describe "unique/fromList/512" $
      let n = 512

          prep = do
            g <- newIOGenM $ mkStdGen 0
            brs <- replicateM n $ randMBR 1023 g
            let as = zip brs [0 :: Int ..]
                r  = R.fromList as
                no = No.fromList as
            _ <- evaluate $ force (r, no)
            return (r, no, g)

      in beforeAll prep $ soup False n

    beforeAll ( do g <- newIOGenM (mkStdGen 0)
                   brs <- replicateM 2048 $ randMBR 1023 g
                   let as = zip brs [0 :: Int ..]
                   return $ force (R.fromList as, No.fromList as)
              ) $
      it "coherent deletion reinserts" $ \(r, no) ->
        let f acc (ba, _) = do
              let acc' = R.delete ba acc
              acc' `shouldSatisfy` isJust . equideep
              return acc'

        in do r' <- foldlM f r (No.toList no)
              r' `shouldSatisfy` R.null
