{-# LANGUAGE FlexibleInstances #-}


module Main
(
    main
)
where
import           Data.RTree.Base
import           Data.RTree.MBB hiding (mbb)


-- import qualified Data.Set as S
import           Prelude                              hiding (lookup, map, null, length)
import           Data.Binary (encode, decode)
import           Data.Function (on)
import           Data.List ((\\))
import qualified Data.List as L (map, length)
import           Debug.Trace                          (trace)
import           Control.Applicative ((<$>))

import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck.Arbitrary            (Arbitrary, arbitrary, shrink)
import           Test.QuickCheck.Gen                  (suchThat)
import           Test.HUnit                           hiding (Test, Testable)
import           Text.Show.Functions                  ()


--import Graphics.Gnuplot.Simple

-- todo: write tests

main :: IO ()
main = do
    defaultMain
       [
             testCase "test_null" test_null
           , testCase "test_singleton" test_singleton
           , testCase "test_insert" test_insert
           , testCase "test_lookup" test_lookup
           , testCase "test_lookupRange" test_lookupRange
           , testCase "test_lookupRangeWithKey" test_lookupRangeWithKey
           , testCase "test_union" test_union
           , testCase "test_unionWith" test_unionWith
           , testCase "test_length" test_length
           , testCase "test_keys" test_keys
           , testCase "test_values" test_values
           , testCase "test_delete" test_delete
           , testCase "test_fromList" test_fromList
           , testCase "test_binary" test_binary
           , testProperty "prop_mbb" prop_mbb
           , testProperty "prop_rtree" prop_rtree
           
--       , testProperty "map a StringMap" prop_map

       ]
-- ------------------------
t_mbb1, t_mbb2 , t_mbb3, t_mbb4, t_mbb5, t_mbb6 :: MBB
t_mbb1 = (MBB 0.0 0.0 1.0 1.0)
t_mbb2 = (MBB 5.0 0.0 6.0 1.0)
t_mbb3 = (MBB 1.0 2.0 2.0 3.0)
t_mbb4 = (MBB 6.0 2.0 7.0 3.0)
t_mbb5 = (MBB 3.0 3.0 4.0 4.0)
t_mbb6 = (MBB 0.0 0.0 0.0 0.0)

t_1, t_2, t_3, t_4, t_5, t_6 :: RTree String
t_1 = singleton t_mbb1 "a"
t_2 = singleton t_mbb2 "b"
t_3 = singleton t_mbb3 "c"
t_4 = singleton t_mbb4 "d"
t_5 = singleton t_mbb5 "e"
t_6 = singleton t_mbb6 "f"

u_1, u_2 :: [(MBB, String)]
u_1 = [(t_mbb1, "a"), (t_mbb2, "b"),(t_mbb3, "c"),(t_mbb4, "d")]
u_2 = [(t_mbb5, "e"), (t_mbb6, "f")] ++ u_1

tu_1, tu_2 :: RTree String
tu_1 = fromList u_1
tu_2 = fromList u_2

-- ------------------------
eqRt :: (Show a, Eq a) => RTree a -> RTree a -> Assertion
eqRt = eqList `on` toList

eqList :: (Show a, Eq a) => [a] -> [a] -> Assertion
eqList l1 l2 = [] @=? (l1 \\ l2)
-- ------------------------

test_null :: Assertion
test_null = do
    null empty @?= True
    null t_1 @?= False

test_singleton :: Assertion
test_singleton = do
    t_1 `eqRt` t_1
    length t_1 @?= 1
    keys t_1 @?= [t_mbb1]
    values t_1 @?= ["a"]

test_insert :: Assertion
test_insert = do
    insert t_mbb2 "b" t_1  `eqRt` fromList [(t_mbb1, "a"), (t_mbb2, "b")]
    insert t_mbb1 "a" t_2  `eqRt` fromList [(t_mbb1, "a"), (t_mbb2, "b")]
    insert t_mbb1 "a+" t_1 `eqRt` fromList [(t_mbb1, "a+")]
    insert t_mbb1 "a" empty `eqRt` t_1
    insert t_mbb5 "e" (fromList u_1) `eqRt` fromList (u_1 ++ [(t_mbb5, "e")])
    insert t_mbb6 "f" (fromList u_1) `eqRt` fromList (u_1 ++ [(t_mbb6, "f")])

test_lookup :: Assertion
test_lookup = do
    lookup t_mbb3 t_3 @?= Just "c"
    lookup t_mbb1 tu_1 @?= Just "a"
    lookup t_mbb2 tu_2 @?= Just "b"
    lookup t_mbb3 tu_2 @?= Just "c"
    lookup t_mbb4 tu_2 @?= Just "d"
    lookup t_mbb5 tu_2 @?= Just "e"
    lookup t_mbb6 tu_2 @?= Just "f"

    lookup t_mbb1 empty @?= (Nothing :: Maybe ())
    lookup t_mbb6 (fromList u_1) @?= Nothing


test_lookupRange :: Assertion
test_lookupRange = do
    lookupRange t_mbb3 t_3 @?= ["c"]
    lookupRange t_mbb1 tu_1 @?= ["a"]
    lookupRange t_mbb2 tu_2 @?= ["b"]
    lookupRange t_mbb3 tu_2 @?= ["c"]
    lookupRange t_mbb4 tu_2 @?= ["d"]
    lookupRange t_mbb5 tu_2 @?= ["e"]
    lookupRange t_mbb6 tu_2 @?= ["f"]

    lookupRange (MBB 1.0 1.0 7.0 3.0) tu_2 @?= ["c", "d"]
    lookupRange (MBB 0.0 0.0 1.0 1.0) tu_2 @?= ["f", "a"]
    lookupRange (MBB 0.0 0.0 7.0 4.0) tu_2 @?= ["e","c","f","a","b","d"] -- todo order irrelevant

test_lookupRangeWithKey :: Assertion
test_lookupRangeWithKey = do
    lookupRangeWithKey t_mbb3 t_3 @?= [(t_mbb3, "c")]
    lookupRangeWithKey t_mbb1 tu_1 @?= [(t_mbb1, "a")]
    lookupRangeWithKey t_mbb2 tu_2 @?= [(t_mbb2, "b")]
    lookupRangeWithKey t_mbb3 tu_2 @?= [(t_mbb3, "c")]
    lookupRangeWithKey t_mbb4 tu_2 @?= [(t_mbb4, "d")]

    lookupRangeWithKey (MBB 1.0 1.0 7.0 3.0) tu_2 @?= [(t_mbb3, "c"), (t_mbb4, "d")]
    lookupRangeWithKey (MBB 0.0 0.0 1.0 1.0) tu_2 @?= [(t_mbb6, "f"), (t_mbb1, "a")]
    lookupRangeWithKey (MBB 0.0 0.0 7.0 4.0) tu_2 `eqList` u_2 -- todo order irrelevant

test_union :: Assertion
test_union = do
    union empty empty `eqRt` (empty :: RTree ())
    union tu_2 tu_1   `eqRt` tu_2
    union t_1 empty   `eqRt` t_1
    union empty t_1   `eqRt` t_1

test_unionWith :: Assertion
test_unionWith = do
    unionWith undefined empty empty `eqRt` (empty :: RTree ())
    unionWith (++) tu_2 tu_1 `eqRt` (fromList [(t_mbb1,"aa"),(t_mbb2,"bb"),(t_mbb3,"cc"),(t_mbb4,"dd"),(t_mbb5,"e"),(t_mbb6,"f")]) -- tu_2


test_length :: Assertion
test_length = do
    length empty @?= 0
    length t_1   @?= 1
    length tu_2  @?= L.length u_2

test_keys :: Assertion
test_keys = do
    keys empty @?= []
    keys t_1   @?= [t_mbb1]
    keys tu_2  `eqList` (fst <$> u_2)

test_values :: Assertion
test_values = do
    values empty @?= ([] :: [()])
    values t_1   @?= ["a"]
    values tu_2  `eqList` (snd <$> u_2)

test_delete :: Assertion
test_delete = do
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

test_fromList :: Assertion
test_fromList = do
    fromList [] `eqRt` (empty :: RTree ())

test_binary :: Assertion
test_binary = do
    (decode $ encode $ tu_2) @?= tu_2



instance Arbitrary MBB where
    arbitrary = do
        cx <- arbitrary
        cy <- arbitrary
        h <- arbitrary `suchThat` (>=0)
        w <- arbitrary `suchThat` (>=0)
        return $ MBB (cx - w) (cy - h) (cx + w) (cy + h)

    shrink mbb@(MBB ulx uly brx bry)
        | isPointMBB mbb = []
        | otherwise      = [MBB (mid ulx brx) (mid uly bry) (mid ulx brx) (mid uly bry)] 
        where 
            mid x y = (y - x) / 2

instance Arbitrary (RTree Int) where
    arbitrary = do
        ks <- arbitrary
        return $ fromList (ks `zip` [1..])

    shrink Empty = []
    shrink Leaf{} = [Empty]
    shrink t =
        [Empty] ++
        -- shrink to subterms
        (getChildren t) ++
        -- recursively shrink subterms
        [createNodeWithChildren newChildred | newChildred <- shrink (getChildren t)]

prop_mbb :: MBB -> Bool
prop_mbb mbb = isValidMBB mbb


prop_rtree :: RTree Int -> Bool
prop_rtree t = isValid "prop_rtree" t

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