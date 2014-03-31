{-# LANGUAGE NoMonomorphismRestriction #-}

-- Copyright (c) 2014, Birte Wagner, Sebastian Philipp
--

module Data.RTree where

import Data.Function
import Data.List (maximumBy, minimumBy)
import Control.Applicative ((<$>))

import Graphics.Gnuplot.Simple

type Point = (Double, Double)

type Rect = (Point, Point)
type MBB = Rect

data RTree a = 
      Node {getMBB :: MBB, getChilderen :: [RTree a] }
    | Leaf {getMBB :: MBB, getElem :: a}
    deriving (Show, Eq)

m, n :: Int
m = 1
n = 2

singleton :: MBB -> a -> RTree a
singleton mbb x = Leaf mbb x

unifyMBB :: RTree a -> RTree a -> MBB
unifyMBB x y = unifyMBB' [getMBB x, getMBB y]

unifyMBB' :: [MBB] -> MBB
unifyMBB' [] = error "no MBB"
unifyMBB' [x] = x
unifyMBB' ((ul,br):xs) = (minUl, maxBr)
    where
    (ul', br') = unifyMBB' xs
    minUl :: Point
    minUl = ((min `on` fst) ul ul', (min `on` snd) ul ul')
    maxBr :: Point
    maxBr = ((max `on` fst) br br', (max `on` snd) br br')

combineChildren :: [RTree a] -> RTree a
combineChildren c = Node (unifyMBB' $ getMBB <$> c) c

validRtree :: RTree a -> Bool
validRtree Leaf{} = True
validRtree x@(Node _ c) = length c >= 2 && (and $ validRtree <$> c) && (isBalanced x)

depth :: RTree a -> Int
depth (Leaf _ _ ) = 0
depth (Node _ c) = 1 + (depth $ head c)

isBalanced :: RTree a -> Bool 
isBalanced (Leaf _ _ ) = True
isBalanced (Node _ c) = (and $ isBalanced <$> c) && (and $ (== depth (head c)) <$> (depth <$> c))

lookup :: a -> RTree a -> a
lookup = undefined

length' :: RTree a -> Int 
length' (Leaf {}) = 1
length' (Node _ c) = sum $ length' <$> c

-- ----------------------------------

insert :: (Eq a) => a -> MBB -> RTree a -> RTree a
insert e mbb oldRoot = insert' (Leaf mbb e) oldRoot

insert' :: (Eq a) => RTree a-> RTree a -> RTree a
insert' Node{} _ = error "insert: node"
insert' newLeaf oldRoot = case addAndSplit newLeaf oldRoot of
        [root] -> root
        [r1, r2] -> combineChildren [r1, r2]

addAndSplit :: (Eq a) => RTree a -> RTree a -> [RTree a]
addAndSplit leaf e@Node{} = maybeSplitNode $ (addLeaf leaf) e
addAndSplit leaf e@Leaf{} = [leaf, e] -- root case

addLeaf :: (Eq a) => RTree a -> RTree a -> RTree a
addLeaf leaf@Leaf{} e 
    | depth e == 0 = Node (leaf `unifyMBB` e) (leaf : getChilderen e)
    | otherwise    = Node (leaf `unifyMBB` e) (insertLeaf leaf (getChilderen e))
addLeaf _ _ = error "addLeaf: node"

maybeSplitNode :: (Eq a) => RTree a -> [RTree a]
maybeSplitNode Leaf{} = error "splitNode: Leaf"
maybeSplitNode x
    | (length $ getChilderen x) > n = splitNode x
    | otherwise = [x]

insertLeaf :: (Eq a) => RTree a -> [RTree a] -> [RTree a]
insertLeaf newLeaf children = findNodeWithMinimalAreaIncrease (addAndSplit newLeaf) newLeaf children

findNodeWithMinimalAreaIncrease :: (RTree a -> [RTree a]) -> RTree a -> [RTree a] -> [RTree a]
findNodeWithMinimalAreaIncrease f e children = concat $ xsAndIncrease'
    where
--  xsAndIncrease :: [(RTree a, Double)]    
    xsAndIncrease = zip children ((areaIncreasesWith e) <$> children)
    minimalIncrease = minimum $ snd <$> xsAndIncrease
--  xsAndIncrease' :: [(RTree a, Double)]    
    xsAndIncrease' = map mapIf xsAndIncrease
    mapIf (x, increase) = if increase == minimalIncrease then
            f x
        else
            [x]

-- | /O(n²)/ solution
splitNode :: (Eq a) => RTree a -> [RTree a]
splitNode Leaf{} = error "splitNode: Leaf"
splitNode e = [combineChildren x1, combineChildren x2]
    where
    (l, r) = findGreatestArea $ getChilderen e
    (x1, x2) = quadSplit [l] [r] (filter (\x -> x /= l && x /= r) $ getChilderen e)

findGreatestArea :: (Eq a) => [RTree a] -> (RTree a, RTree a)
findGreatestArea list = (x', y')
    where
    listOfTripels = [(x, y, unifyMBB x y) | x <- list, y <- list , x /= y]
    (x', y', _) = maximumBy (compare `on` (\(_,_,x) -> area x)) listOfTripels


quadSplit :: (Eq a) => [RTree a] -> [RTree a] -> [RTree a] -> ([RTree a], [RTree a])
quadSplit left right [] = (left, right)
quadSplit left right unfinished
    | (length left) + (length unfinished)  <= m = (left ++ unfinished, right)
    | (length right) + (length unfinished) <= m = (left, right ++ unfinished)
    | isLeft''  = quadSplit (minimumElem : left) right  newRest
    | otherwise = quadSplit left  (minimumElem : right) newRest
        where
--      makeTripel :: RTree a -> (RTree a, Bool, Double)
        makeTripel x = (x, isLeft, growth)
            where
            isLeft = (areaIncreasesWithLeft) < (areaIncreasesWithRight)
            growth = case isLeft of
                True -> areaIncreasesWithLeft
                False -> areaIncreasesWithRight
            areaIncreasesWithLeft  = (areaIncreasesWith x (combineChildren left))
            areaIncreasesWithRight = (areaIncreasesWith x (combineChildren right))
        (minimumElem, isLeft'', _) = minimumBy (compare `on` (\(_,_,g) -> g)) $ makeTripel <$> unfinished
        newRest = (filter (\x -> x /= minimumElem) unfinished)

mergeNodes :: RTree a -> RTree a -> RTree a
mergeNodes x@Node{} y@Node{} = Node (unifyMBB x y) (on (++) getChilderen x y)
mergeNodes _ _               = error "no merge for Leafs"

fromList :: (Eq a) => [RTree a] -> RTree a
fromList = foldr1 insert'
-- ------------
-- helpers

area :: MBB -> Double
area ((x1, y1), (x2, y2)) = (x2 - x1) * (y2 - y1)

areaIncreasesWith :: RTree a -> (RTree a) -> Double
areaIncreasesWith newElem current = newArea - currentArea
    where
    currentArea = area $ getMBB current
    newArea = area $ unifyMBB newElem current


-- ----------------
t_mbb1, t_mbb2 , t_mbb3, t_mbb4:: MBB
t_mbb1 = ((0.0,0.0),(1.0,1.0))
t_mbb2 = ((5.0,0.0),(6.0,1.0))
t_mbb3 = ((1.0,2.0),(2.0,3.0))
t_mbb4 = ((6.0,2.0),(7.0,3.0))
t_1, t_2, t_3, t_4 :: RTree String
t_1 = singleton t_mbb1 "a"
t_2 = singleton t_mbb2 "b"
t_3 = singleton t_mbb3 "c"
t_4 = singleton t_mbb4 "d"
t_5 = fromList [t_1, t_2, t_3, t_4]



nodeToPath :: RTree a -> [(Double, Double)]
nodeToPath e = [(ulx, uly),(brx, uly),(brx, bry),(ulx, bry),(ulx, uly)]
    where
    ((ulx, uly),(brx, bry))  = getMBB e

rtreeToPaths :: RTree a -> [[(Double, Double)]]
rtreeToPaths e@Leaf{} = [nodeToPath e]
rtreeToPaths e@Node{} = [nodeToPath e] ++ (concat $ rtreeToPaths <$> (getChilderen e))

plotRtree :: RTree a -> IO ()
plotRtree tree = do
    print [p20 ulx brx, p20 uly bry]
    print [ulx, brx, uly, bry]
    plotPaths [XRange $ p20 ulx brx, YRange $ p20 uly bry] $ rtreeToPaths tree
    where
    ((ulx, uly),(brx, bry))  = getMBB tree
    p20 l r = (l - ((r-l) / 5), r + ((r-l) / 5))
