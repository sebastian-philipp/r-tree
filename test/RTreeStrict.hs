{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Main where

-- import qualified Data.RTree                       as Lazy    -- just for dev.

import Control.Applicative         ((<$>))
import Control.DeepSeq             (($!!))
import Control.Monad.IO.Class
import Data.RTree.MBB
import Data.RTree.Strict
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Prelude                     hiding (lookup, map, mapM, null, succ)
import Test.Hspec
import Test.HUnit                  hiding (Test, Testable)

import qualified Data.RTree     as L
import qualified Gen            as G
import qualified GHC.AssertNF   as NF
import qualified Hedgehog.Gen   as G
import qualified Hedgehog.Range as R

newtype Attr = A [Int]
    deriving (Show, Semigroup)

instance Monoid Attr where
    mempty = mkA []
    mappend (A xs) (A ys) = mkA (xs ++ ys)

-- evaluation of x `mappend` y to WHNF leads to NF
-- because of the $!! in mkA
--
-- example
--
--    A [1,2] `mappend` A [3,4]
-- =  { subst of mappend }
--    mkA ([1,2] ++ [3,4])
-- =  { subst of mkA }
--    A $!! ([1,2] ++ [3,4])
-- =  { subst of $!! }
--    A [1,2,3,4]
--
-- in a call of Data.RTree.insert k (x `mappend` y) m
-- the attribute is forced to be in WHNF, and this leads to NF

type Map = RTree Attr

-- smart constructor for evaluation into NF
-- before calling the constructor A

mkA :: [Int] -> Attr
mkA xs = A $!! xs

mkA' :: Int -> Attr
mkA' x = mkA [0 .. x]

consA :: Int -> Attr -> Attr
consA n a = mkA [n] `mappend` a

default (Int)

main :: IO ()
main = hspec $ do
  it "isNF" test_isNF
  it "empty" (checkIsNF (empty :: RTree ()))
  it "t_1" (checkIsNF t_1)
  it "tu_1" (checkIsNF tu_1)
  it "tu_2" (checkIsNF tu_2)
  it "tu_2" (checkIsNF test_union)
  it "test_insertWith1" (checkIsNF test_insertWith1)
  it "test_insertWith" (checkIsNF test_insertWith)
  it "test_map" (checkIsNF test_map)
  it "test_toStrict" (checkIsNF test_toStrict)

  -- it "m1" (checkIsNF m1)
  -- it "m2" (checkIsNF m2)
  -- it "m3" (checkIsNF m3)
  -- it "m5" (checkIsNF m3)
  -- it "m6" (checkIsNF m3)
  -- it "m7 (map test)" (checkIsNF m7)
  -- it "fromList l4" (checkIsNF $ fromList l4)
  -- it "m8 (fromList''' ll)" (checkIsNF m8)

  -- it "adjust m6" (checkIsNF $ adjust (consA 42) "ab" m6)
  -- it "adjust m1" (checkIsNF $ adjust (consA 42) "xx" m1)
  -- it "delete m6" (checkIsNF $ delete "ab" m6)
  -- it "delete m1" (checkIsNF $ delete "xx" m1)

  -- it "m2 union m3" (checkIsNF $ m2 `union` m3)
  -- it "m2 unionWith m2" (checkIsNF $ unionWith mappend m2 m2)

  it "prop_fromList" $ requireProperty $ do
    l <- forAll $ G.list (R.linear 0 100) $ (,) <$> G.mbb <*> G.int R.constantBounded
    -- hPutStrLn stderr $ "\n" ++ show l
    -- hPutStrLn stderr $ "\n" ++ show (fromList''' l)
    passed <- liftIO $ NF.isNF $! fromList l
    Hedgehog.assert passed

  it "prop_union" $ requireProperty $ do
    l1 <- forAll $ G.list (R.linear 0 100) $ (,) <$> G.mbb <*> G.int R.constantBounded
    l2 <- forAll $ G.list (R.linear 0 100) $ (,) <$> G.mbb <*> G.int R.constantBounded
    passed <- liftIO $ do
      let sm = fromList l1 `union` fromList l2
      NF.isNF $! sm
    Hedgehog.assert passed

test_isNF :: Assertion
test_isNF = fmap not (NF.isNF [(1::Int)..10]) @? "isNF"

checkIsNF :: (Show a) => RTree a -> Assertion
checkIsNF !m = NF.isNF m @? ("isNF " ++ show m)

-- some simple test data
-- ------------------------
t_mbb1, t_mbb2 , t_mbb3, t_mbb4, t_mbb5, t_mbb6 :: MBB
t_mbb1 = (MBB 0.0 0.0 1.0 1.0)
t_mbb2 = (MBB 5.0 0.0 6.0 1.0)
t_mbb3 = (MBB 1.0 2.0 2.0 3.0)
t_mbb4 = (MBB 6.0 2.0 7.0 3.0)
t_mbb5 = (MBB 3.0 3.0 4.0 4.0)
t_mbb6 = (MBB 0.0 0.0 0.0 0.0)

u_1, u_2 :: [(MBB, Attr)]
u_1 = [(t_mbb1, mkA' 1), (t_mbb2, mkA' 2),(t_mbb3, mkA' 3),(t_mbb4, mkA' 4)]
u_2 = [(t_mbb5, mkA' 5), (t_mbb6, mkA' 6)] ++ u_1

t_1, t_2, t_3, t_4, t_5, t_6 :: RTree Attr
[t_5, t_6, t_1, t_2, t_3, t_4] = (uncurry singleton) <$> u_2

tu_1, tu_2 :: RTree Attr
tu_1 = fromList u_1
tu_2 = fromList u_2


test_union :: RTree Attr
test_union = unionWith mappend tu_1 t_6

test_map :: RTree Attr
test_map = fmap id tu_1

test_insertWith1 :: RTree Attr
test_insertWith1 = insertWith mappend t_mbb1 (mkA' 4) t_1

test_insertWith :: RTree Attr
test_insertWith = insertWith mappend t_mbb6 (mkA' 6) tu_2

test_toStrict :: RTree Attr
test_toStrict = toStrict $ L.fromList u_2
