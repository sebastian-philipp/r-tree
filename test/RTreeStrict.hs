{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Main
where

-- import qualified Data.RTree                       as Lazy    -- just for dev.
import            Prelude                              hiding (lookup, map, mapM,
                                                       null, succ)

--import           Control.Arrow                        (second)
import            Control.Applicative ((<$>))
import            Control.DeepSeq                      (($!!))

import            Data.Monoid
import            Data.RTree.Strict
import qualified  Data.RTree as L
import            Data.RTree.MBB

import            GHC.AssertNF

-- import           System.IO

import            Test.Framework
import            Test.Framework.Providers.HUnit
import            Test.HUnit                           hiding (Test, Testable)

newtype Attr = A [Int]
    deriving (Show)

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
main = defaultMain
       [
         testCase "isNF" test_isNF
       , testCase "empty" (checkIsNF (empty :: RTree ()))
       , testCase "t_1" (checkIsNF t_1)
       , testCase "tu_1" (checkIsNF tu_1)
       , testCase "tu_2" (checkIsNF tu_2)
       , testCase "tu_2" (checkIsNF test_union)
       , testCase "test_insertWith1" (checkIsNF test_insertWith1)
       , testCase "test_insertWith" (checkIsNF test_insertWith)
       , testCase "test_map" (checkIsNF test_map)
       , testCase "test_toStrict" (checkIsNF test_toStrict)


       --, testCase "m1" (checkIsNF m1)
       --, testCase "m2" (checkIsNF m2)
       --, testCase "m3" (checkIsNF m3)
       --, testCase "m5" (checkIsNF m3)
       --, testCase "m6" (checkIsNF m3)
       --, testCase "m7 (map test)" (checkIsNF m7)
       --, testCase "fromList l4" (checkIsNF $ fromList l4)
       --, testCase "m8 (fromList''' ll)" (checkIsNF m8)

       --, testCase "adjust m6" (checkIsNF $ adjust (consA 42) "ab" m6)
       --, testCase "adjust m1" (checkIsNF $ adjust (consA 42) "xx" m1)
       --, testCase "delete m6" (checkIsNF $ delete "ab" m6)
       --, testCase "delete m1" (checkIsNF $ delete "xx" m1)

       --, testCase "m2 union m3" (checkIsNF $ m2 `union` m3)
       --, testCase "m2 unionWith m2" (checkIsNF $ unionWith mappend m2 m2)

       --  -- these test do not run properly with ghc-7.7-pre and ghc-heap-view-0.5.2
       --  -- no idea, whether patched ghc-heap-view or QuickCheck is the reason
       --, testProperty "prop_simple" prop_simple
       --, testProperty "prop_union" prop_union
       --, testProperty "prop_diff" prop_diff
       ]

test_isNF :: Assertion
test_isNF = fmap not (isNF [(1::Int)..10]) @? "isNF"

checkIsNF :: (Show a) => RTree a -> Assertion
checkIsNF !m = isNF m @? ("isNF " ++ show m)

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



--prop_simple :: Q.Property
--prop_simple = Q.monadicIO $ do
--                            l <- Q.pick Q.arbitrary
--                            passed <- Q.run $ do -- hPutStrLn stderr $ "\n" ++ show l
--                                                 -- hPutStrLn stderr $ "\n" ++ show (fromList''' l)
--                                                 isNF $! fromList''' l
--                            Q.assert passed

--prop_union :: Q.Property
--prop_union = Q.monadicIO $ do
--                            l1 <- Q.pick Q.arbitrary
--                            l2 <- Q.pick Q.arbitrary
--                            let sm = fromList''' l1 `union` fromList''' l2
--                            checkIsNFProp sm


--prop_diff :: Q.Property
--prop_diff = Q.monadicIO $ do
--                            l1 <- Q.pick Q.arbitrary
--                            l2 <- Q.pick Q.arbitrary
--                            let sm = fromList''' l1 `difference` fromList''' l2
--                            checkIsNFProp sm

--checkIsNFProp :: a -> Q.PropertyM IO ()
--checkIsNFProp sm = do
--                            passed <- Q.run $ isNF $! sm
--                            Q.run $ assertNF $! sm
--                            Q.assert passed
