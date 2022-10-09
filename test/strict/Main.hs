{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import           Data.RTree.Internal (RTree (..), Node (..))
import           Data.RTree.Strict (MBR (..))
import qualified Data.RTree.Strict as R

import           Control.Monad
import           Data.Foldable
import           Data.List.NonEmpty as NonEmpty
import           Data.Maybe
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
    case r of
      Root _ba a  -> wNoThunks ("RTree" : ctx) a
      Leaf1 _ba a -> wNoThunks ("Leaf1" : ctx) a
      Empty       -> return Nothing

  showTypeOf _ = "RTree"

instance NoThunks a => NoThunks (Node r a) where
  wNoThunks ctx x =
    case x of
      Node as ->
        let name i = ("Node[" <> show (i :: Int) <> "]") : ctx

            f (ba, a, i) = [wNoThunks (name i) ba, wNoThunks (name i) a]

        in allNoThunks . foldMap f $
                           NonEmpty.zipWith (\(ba, a) i -> (ba, a, i)) as (0 :| [1..])

      Leaf as ->
        let name i = ("Leaf[" <> show (i :: Int) <> "]") : ctx
            
            f (ba, a, i) = [wNoThunks (name i) ba, wNoThunks (name i) a]

        in allNoThunks . foldMap f $
                           NonEmpty.zipWith (\(ba, a) i -> (ba, a, i)) as (0 :| [1..])

  showTypeOf _ = "Node"

instance NoThunks (MBR r) where
  wNoThunks _ _ = return Nothing

  showTypeOf _ = "MBR"



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

      void . foldlM f R.empty $ Prelude.zip brs [0 :: Int ..]

    it "insertGut" $ do
      g <- newIOGenM $ mkStdGen 0
      brs <- replicateM 1024 $ randMBR g
      let f r (ba, a) = do
            let r' = R.insertGut ba a r
            s <- wNoThunks [show $ R.length r'] r'
            s `shouldSatisfy` isNothing
            return r'

      void . foldlM f R.empty $ Prelude.zip brs [0 :: Int ..]

    it "bulkSTR" $ do
      g <- newIOGenM $ mkStdGen 0
      brs <- replicateM 1024 $ randMBR g
      let r = R.bulkSTR $ Prelude.zip brs [0 :: Int ..]
      s <- wNoThunks [] r
      s `shouldSatisfy` isNothing
