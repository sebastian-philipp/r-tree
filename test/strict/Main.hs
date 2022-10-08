{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import           Data.RTree.Internal (RTree (..), Node (..))
import           Data.RTree.MBR
import qualified Data.RTree.Strict as R

import           Control.Monad
import           Data.Foldable
import           Data.Maybe
import           Data.Monoid
import           Data.Primitive.Array
import           NoThunks.Class
import           System.Random.Stateful
import           Test.Hspec



randMBR :: StatefulGen g m => g -> m (MBR Int)
randMBR g = do
  a <- uniformRM (0, 1023) g
  b <- uniformRM (0, 1023) g
  return $ MBR a b (a + 1) (b + 1)



instance NoThunks a => NoThunks (RTree r a) where
  wNoThunks ctx r =
    let ctx' = "RTree" : ctx
    in case r of
         Root _ba a  -> wNoThunks ctx' $! a
         Leaf1 _ba a -> wNoThunks ctx' a
         Empty       -> return Nothing

  showTypeOf _ = "RTree"

instance NoThunks a => NoThunks (Node r a) where
  wNoThunks ctx x =
    case x of
      Node n _brs as ->
        let name i = "Leaf[" <> show i <> "]"

            f i = do a <- indexArrayM as i
                     wNoThunks (name i : ctx) $! a

        in allNoThunks $ f <$> [0 .. n - 1]

      Leaf n _brs as ->
        let name i = "Leaf[" <> show i <> "]"

            f i = do a <- indexArrayM as i
                     wNoThunks (name i : ctx) a

        in allNoThunks $ f <$> [0 .. n - 1]

  showTypeOf _ = "Node"



wThunksList :: Context -> [a] -> IO (Maybe ThunkInfo)
wThunksList ctx = noThunks ctx . fmap AllowThunk



main :: IO ()
main =
  hspec $ do
    it "insert" $ do
      g <- newIOGenM $ mkStdGen 0
      brs <- replicateM 1024 $ randMBR g
      let f r (ba, a) = do
            let r' = R.insert ba a r
            s <- wNoThunks [show $ R.length r'] r'
            s `shouldSatisfy` isNothing
            return r'

      void . foldlM f R.empty $ zip brs [0 :: Int ..]

    it "insertGut" $ do
      g <- newIOGenM $ mkStdGen 0
      brs <- replicateM 1024 $ randMBR g
      let f r (ba, a) = do
            let r' = R.insertGut ba a r
            s <- wNoThunks [show $ R.length r'] r'
            s `shouldSatisfy` isNothing
            return r'

      void . foldlM f R.empty $ zip brs [0 :: Int ..]

    it "fromList" $ do
      g <- newIOGenM $ mkStdGen 0
      brs <- replicateM 1024 $ randMBR g
      let r = R.fromList $ zip brs [0 :: Int ..]
      s <- wNoThunks [] r
      s `shouldSatisfy` isNothing

    it "bulkSTR" $ do
      g <- newIOGenM $ mkStdGen 0
      brs <- replicateM 1024 $ randMBR g
      let r = R.bulkSTR $ zip brs [0 :: Int ..]
      s <- wNoThunks [] r
      s `shouldSatisfy` isNothing
#if __GLASGOW_HASKELL__ >= 808
    it "foldMapWithKey'" $ do
      g <- newIOGenM $ mkStdGen 0
      brs <- replicateM 1024 $ randMBR g
      let r = R.fromList $ zip brs [0 :: Int ..]

      s <- wNoThunks [] $
             getFirst $ R.foldMap' (R.intersects $ MBR 256 256 768 768) (First . Just) r
      s `shouldSatisfy` isNothing
#endif
    it "foldrWithKey'" $ do
      g <- newIOGenM $ mkStdGen 0
      brs <- replicateM 1024 $ randMBR g
      let r = R.fromList $ zip brs [0 :: Int ..]

      s <- wNoThunks [] $
             R.foldr' (R.intersects $ MBR 256 256 768 768) (:) [] r
      s `shouldSatisfy` isNothing

    it "foldlWithKey'" $ do
      g <- newIOGenM $ mkStdGen 0
      brs <- replicateM 1024 $ randMBR g
      let r = R.fromList $ zip brs [0 :: Int ..]

      s <- wNoThunks [] $
             R.foldl' (R.intersects $ MBR 256 256 768 768) (flip (:)) [] r
      s `shouldSatisfy` isNothing
