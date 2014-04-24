module Main
where
import           Data.RTree

import           Prelude                              hiding (lookup, map, null, length)
import qualified Data.List as L (map)
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit                           hiding (Test, Testable)
import           Text.Show.Functions                  ()

import           Control.Applicative ((<$>))

import Graphics.Gnuplot.Simple

-- todo: write tests

main :: IO ()
main = do
    defaultMain
       [
--        , testCase "lookuprange" test_range
--       , testProperty "map a StringMap" prop_map

       ]


t_mbb1, t_mbb2 , t_mbb3, t_mbb4:: MBB
t_mbb1 = (MBB 0.0 0.0 1.0 1.0)
t_mbb2 = (MBB 5.0 0.0 6.0 1.0)
t_mbb3 = (MBB 1.0 2.0 2.0 3.0)
t_mbb4 = (MBB 6.0 2.0 7.0 3.0)
t_1, t_2, t_3, t_4 :: RTree String
t_1 = singleton t_mbb1 "a"
t_2 = singleton t_mbb2 "b"
t_3 = singleton t_mbb3 "c"
t_4 = singleton t_mbb4 "d"
t_5 = fromList [(t_mbb1, "a"), (t_mbb2, "b"),(t_mbb3, "c"),(t_mbb4, "d")]
{- t_p = node (MBB 6469.0 9103.0 6656.0 9721.0) [
    Leaf {getMBB = (MBB 6469.0 9103.0 6469.0 9721.0), getElem = ()},
    Leaf {getMBB = (MBB 6786.0 9678.0 6656.0 9651.0), getElem = ()},
    Leaf {getMBB = (MBB 6593.0 9103.0 6593.0 9721.0), getElem = ()}]
t_pp = Leaf {getMBB = (MBB 6531.0 9103.0 6531.0 9721.0), getElem = ()}
t_ppp = union t_pp t_p
-}

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