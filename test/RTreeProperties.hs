{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications  #-}

module Main
  ( main
  ) where

import Control.Applicative         ((<$>))
import Data.Binary                 (decode, encode)
import Data.Function               (on)
import Data.List                   ((\\))
import Data.RTree.Base
import Data.RTree.MBB              hiding (mbb)
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Prelude                     hiding (length, lookup, map, null)
import Test.Hspec
import Test.HUnit                  hiding (Test, Testable)
import Text.Show.Functions         ()

import qualified Data.List as L (length)
import qualified Gen       as G

--import Graphics.Gnuplot.Simple

-- todo: write tests

main :: IO ()
main = hspec $ do
  it "test_null" $ do
    null empty  @?= True
    null t_1    @?= False
  it "test_singleton" $ do
    t_1 `eqRt` t_1
    length t_1  @?= 1
    keys t_1    @?= [t_mbb1]
    values t_1  @?= ["a"]
  it "test_insert" $ do
    insert t_mbb2 "b" t_1             `eqRt` fromList [(t_mbb1, "a"), (t_mbb2, "b")]
    insert t_mbb1 "a" t_2             `eqRt` fromList [(t_mbb1, "a"), (t_mbb2, "b")]
    insert t_mbb1 "a+" t_1            `eqRt` fromList [(t_mbb1, "a+")]
    insert t_mbb1 "a" empty           `eqRt` t_1
    insert t_mbb5 "e" (fromList u_1)  `eqRt` fromList (u_1 ++ [(t_mbb5, "e")])
    insert t_mbb6 "f" (fromList u_1)  `eqRt` fromList (u_1 ++ [(t_mbb6, "f")])
  it "test_lookup" $ do
    lookup t_mbb3 t_3   @?= Just "c"
    lookup t_mbb1 tu_1  @?= Just "a"
    lookup t_mbb2 tu_2  @?= Just "b"
    lookup t_mbb3 tu_2  @?= Just "c"
    lookup t_mbb4 tu_2  @?= Just "d"
    lookup t_mbb5 tu_2  @?= Just "e"
    lookup t_mbb6 tu_2  @?= Just "f"

    lookup t_mbb2 tu_3  @?= Just "b"
    lookup t_mbb3 tu_3  @?= Just "c"
    lookup t_mbb4 tu_3  @?= Just "d"
    lookup t_mbb5 tu_3  @?= Just "e"
    lookup t_mbb6 tu_3  @?= Just "f"
    lookup t_mbb7 tu_3  @?= Just "g"
    lookup t_mbb8 tu_3  @?= Just "h"

    lookup t_mbb1 empty @?= (Nothing :: Maybe ())
    lookup t_mbb6 (fromList u_1) @?= Nothing

  it "test_lookupRange" $ do
    lookupRange t_mbb3 t_3  @?= ["c"]
    lookupRange t_mbb1 tu_1 @?= ["a"]
    lookupRange t_mbb2 tu_2 @?= ["b"]
    lookupRange t_mbb3 tu_2 @?= ["c"]
    lookupRange t_mbb4 tu_2 @?= ["d"]
    lookupRange t_mbb5 tu_2 @?= ["e"]
    lookupRange t_mbb6 tu_2 @?= ["f"]

    lookupRange (MBB 1.0 1.0 7.0 3.0) tu_2 @?= ["c", "d"]
    lookupRange (MBB 0.0 0.0 1.0 1.0) tu_2 @?= ["f", "a"]
    lookupRange (MBB 0.0 0.0 7.0 4.0) tu_2 @?= ["e","c","f","a","b","d"] -- todo order irrelevant

    lookupRange t_mbb2 tu_3 @?= ["b"]
    lookupRange t_mbb3 tu_3 @?= ["c"]
    lookupRange t_mbb4 tu_3 @?= ["d"]
    lookupRange t_mbb5 tu_3 @?= ["e"]
    lookupRange t_mbb6 tu_3 @?= ["f"]
    lookupRange t_mbb7 tu_3 `eqList` ["g","e","c"]
    lookupRange t_mbb8 tu_3 `eqList` ["h", "b"]

    lookupRange (MBB 3.0 2.0 7.0 4.0) tu_3 `eqList` ["e","d"]
    lookupRange (MBB 0.0 0.0 5.0 3.0) tu_3 `eqList` ["f","a","c"]

  it "test_lookupRangeWithKey" $ do
    lookupContainsRange t_mbb3 t_3  @?= ["c"]
    lookupContainsRange t_mbb1 tu_1 @?= ["a"]
    lookupContainsRange t_mbb2 tu_2 @?= ["b"]
    lookupContainsRange t_mbb3 tu_2 @?= ["c"]
    lookupContainsRange t_mbb4 tu_2 @?= ["d"]
    lookupContainsRange t_mbb5 tu_2 @?= ["e"]
    lookupContainsRange t_mbb6 tu_2 `eqList` ["f","a"]

    lookupContainsRange (MBB 1.0 1.0 7.0 3.0) tu_2 @?= []
    lookupContainsRange (MBB 0.0 0.0 1.0 1.0) tu_2 @?= ["a"]
    lookupContainsRange (MBB 0.0 0.0 7.0 4.0) tu_2 @?= []
    lookupContainsRange (MBB 0.5 0.5 0.5 0.5) tu_2 @?= ["a"]
    lookupContainsRange (MBB 0.0 1.0 0.0 1.0) tu_2 @?= ["a"]
    lookupContainsRange (MBB 1.0 0.0 1.0 0.0) tu_2 @?= ["a"]
    lookupContainsRange (MBB 1.0 1.0 1.0 1.0) tu_2 @?= ["a"]

    lookupContainsRange t_mbb2 tu_3 `eqList` ["b","h"]
    lookupContainsRange t_mbb3 tu_3 `eqList` ["c","g"]
    lookupContainsRange t_mbb4 tu_3 `eqList` ["d"]
    lookupContainsRange t_mbb5 tu_3 `eqList` ["e","g"]
    lookupContainsRange t_mbb6 tu_3 `eqList` ["f","a"]
    lookupContainsRange t_mbb7 tu_3 `eqList` ["g"]
    lookupContainsRange t_mbb8 tu_3 `eqList` ["h"]
  it "test_lookupContainsRange" $ do
    lookupContainsRange t_mbb3 t_3  @?= ["c"]
    lookupContainsRange t_mbb1 tu_1 @?= ["a"]
    lookupContainsRange t_mbb2 tu_2 @?= ["b"]
    lookupContainsRange t_mbb3 tu_2 @?= ["c"]
    lookupContainsRange t_mbb4 tu_2 @?= ["d"]
    lookupContainsRange t_mbb5 tu_2 @?= ["e"]
    lookupContainsRange t_mbb6 tu_2 `eqList` ["f","a"]

    lookupContainsRange (MBB 1.0 1.0 7.0 3.0) tu_2 @?= []
    lookupContainsRange (MBB 0.0 0.0 1.0 1.0) tu_2 @?= ["a"]
    lookupContainsRange (MBB 0.0 0.0 7.0 4.0) tu_2 @?= []
    lookupContainsRange (MBB 0.5 0.5 0.5 0.5) tu_2 @?= ["a"]
    lookupContainsRange (MBB 0.0 1.0 0.0 1.0) tu_2 @?= ["a"]
    lookupContainsRange (MBB 1.0 0.0 1.0 0.0) tu_2 @?= ["a"]
    lookupContainsRange (MBB 1.0 1.0 1.0 1.0) tu_2 @?= ["a"]

    lookupContainsRange t_mbb2 tu_3 `eqList` ["b","h"]
    lookupContainsRange t_mbb3 tu_3 `eqList` ["c","g"]
    lookupContainsRange t_mbb4 tu_3 `eqList` ["d"]
    lookupContainsRange t_mbb5 tu_3 `eqList` ["e","g"]
    lookupContainsRange t_mbb6 tu_3 `eqList` ["f","a"]
    lookupContainsRange t_mbb7 tu_3 `eqList` ["g"]
    lookupContainsRange t_mbb8 tu_3 `eqList` ["h"]

    lookupContainsRange (MBB 4.5 2.5 4.5 2.5) tu_3 `eqList` ["g","h"]

  it "test_lookupContainsRangeWithKey" $ do
    lookupContainsRangeWithKey t_mbb3 t_3   @?=     [(t_mbb3, "c")]
    lookupContainsRangeWithKey t_mbb1 tu_1  @?=     [(t_mbb1, "a")]
    lookupContainsRangeWithKey t_mbb2 tu_2  @?=     [(t_mbb2, "b")]
    lookupContainsRangeWithKey t_mbb3 tu_2  @?=     [(t_mbb3, "c")]
    lookupContainsRangeWithKey t_mbb4 tu_2  @?=     [(t_mbb4, "d")]
    lookupContainsRangeWithKey t_mbb5 tu_2  @?=     [(t_mbb5, "e")]
    lookupContainsRangeWithKey t_mbb6 tu_2 `eqList` [(t_mbb6, "f"), (t_mbb1, "a")]
  it "test_union" $ do
    union empty empty `eqRt` (empty :: RTree ())
    union tu_2 tu_1   `eqRt` tu_2
    union t_1 empty   `eqRt` t_1
    union empty t_1   `eqRt` t_1
  it "test_unionWith" $ do
    unionWith undefined empty empty `eqRt` (empty :: RTree ())
    unionWith (++) tu_2 tu_1 `eqRt` (fromList [(t_mbb1,"aa"),(t_mbb2,"bb"),(t_mbb3,"cc"),(t_mbb4,"dd"),(t_mbb5,"e"),(t_mbb6,"f")]) -- tu_2
  it "test_length" $ do
    length empty @?= 0
    length t_1   @?= 1
    length tu_2  @?= L.length u_2
  it "test_keys" $ do
    keys empty @?= []
    keys t_1   @?= [t_mbb1]
    keys tu_2  `eqList` (fst <$> u_2)
  it "test_values" $ do
    values empty @?= ([] :: [()])
    values t_1   @?= ["a"]
    values tu_2  `eqList` (snd <$> u_2)
  it "test_delete" $ do
    let d1 = delete (MBB 3.0 3.0 4.0 4.0) tu_2
    values d1 @?= ["c","f","a","b","d"]
    let d2 = delete (MBB 1.0 2.0 2.0 3.0) d1
    values d2 @?= ["f","a","b","d"]
    let d3 = delete (MBB 0.0 0.0 0.0 0.0) d2
    values d3 @?= ["a","b","d"]
    let d4 = delete (MBB 5.0 0.0 6.0 1.0) d3
    values d4 @?= ["a","d"]
    let d5 = delete (MBB 0.0 0.0 1.0 1.0) d4
    values d5 @?= ["d"]
    let d6 = delete (MBB 6.0 2.0 7.0 3.0) d5
    values d6 @?= []
  it "test_fromList" $ do
    fromList [] `eqRt` (empty :: RTree ())
  it "test_binary" $ do
    (decode $ encode $ tu_2) @?= tu_2
  it "prop_mbb" $ requireProperty $ do
    mbb <- forAll G.mbb
    Hedgehog.assert $ isValidMBB mbb
  it "prop_rtree" $ requireProperty $ do
    rt <- forAll G.rtreeInt
    Hedgehog.assert $ isValid "prop_rtree" rt

  -- prop "map a StringMap" prop_map

-- ------------------------
t_mbb1, t_mbb2 , t_mbb3, t_mbb4, t_mbb5, t_mbb6, t_mbb7, t_mbb8 :: MBB
t_mbb1 = (MBB 0.0 0.0 1.0 1.0)
t_mbb2 = (MBB 5.0 0.0 6.0 1.0)
t_mbb3 = (MBB 1.0 2.0 2.0 3.0)
t_mbb4 = (MBB 6.0 2.0 7.0 3.0)
t_mbb5 = (MBB 3.0 3.0 4.0 4.0)
t_mbb6 = (MBB 0.0 0.0 0.0 0.0)
t_mbb7 = (MBB 1.0 2.0 5.0 4.0)
t_mbb8 = (MBB 4.0 0.0 6.0 3.0)

t_1, t_2, t_3 :: RTree String
t_1 = singleton t_mbb1 "a"
t_2 = singleton t_mbb2 "b"
t_3 = singleton t_mbb3 "c"


u_1, u_2, u_3 :: [(MBB, String)]
u_1 = [(t_mbb1, "a"), (t_mbb2, "b"),(t_mbb3, "c"),(t_mbb4, "d")]
u_2 = [(t_mbb5, "e"), (t_mbb6, "f")] ++ u_1
u_3 = [(t_mbb7, "g"), (t_mbb8, "h")] ++ u_2

tu_1, tu_2, tu_3 :: RTree String
tu_1 = fromList u_1
tu_2 = fromList u_2
tu_3 = fromList u_3

-- ------------------------
eqRt :: (Show a, Eq a) => RTree a -> RTree a -> Assertion
eqRt = eqList `on` toList

eqList :: (Show a, Eq a) => [a] -> [a] -> Assertion
eqList l1 l2 = do
    [] @=? (l1 \\ l2)
    (L.length l1) @=? (L.length l2)

-- -------------------------

{- t_p = node (mbb 6469.0 9103.0 6656.0 9721.0) [
    Leaf {getmbb = (mbb 6469.0 9103.0 6469.0 9721.0), getElem = ()},
    Leaf {getmbb = (mbb 6786.0 9678.0 6656.0 9651.0), getElem = ()},
    Leaf {getmbb = (mbb 6593.0 9103.0 6593.0 9721.0), getElem = ()}]
t_pp = Leaf {getmbb = (mbb 6531.0 9103.0 6531.0 9721.0), getElem = ()}
t_ppp = union t_pp t_p
-}

{-

mbbToPath :: MBB -> [(Double, Double)]
mbbToPath (MBB ulx uly brx bry) = [(ulx, uly),(brx, uly),(brx, bry),(ulx, bry),(ulx, uly)]

rtreeToPaths :: RTree a -> [[(Double, Double)]]
rtreeToPaths = foldWithMBB handleLeaf handleNode []
    where
        handleLeaf mbb _ = [mbbToPath mbb]
        handleNode mbb xs = [mbbToPath mbb] ++ (concat xs)


plotRtree :: RTree a -> IO ()
plotRtree tree = do
    print [p20 ulx brx, p20 uly bry]
    print [ulx, brx, uly, bry]
    plotPaths [Key Nothing, XRange $ p20 ulx brx, YRange $ p20 uly bry] $ rtreeToPaths tree
    where
    (MBB ulx uly brx bry)  = getMBB tree
    p20 l r = (l - ((r-l) / 5), r + ((r-l) / 5))


testData :: FilePath -> IO (RTree ())
testData p = do
    d <- lines <$> readFile p
    let pairs = zip (listToMBB <$> (L.map read d)) (replicate 100000000 ())
    return $ fromList pairs
    where
        listToMBB :: [Double] -> MBB
        listToMBB [ulx, uly, brx, bry] = MBB ulx uly brx bry
        listToMBB xs = error $ "invalid data " ++ show xs
-}
