{-# LANGUAGE NoMonomorphismRestriction #-}

-- Copyright (c) 2014, Birte Wagner, Sebastian Philipp
--

module RTree where

import Data.Function
import Data.List (maximumBy, minimumBy)
import Control.Applicative ((<$>))

type Point = (Double, Double)

type Rect = (Point, Point)
type MBB = Rect

data RTree a = 
      Node {getMBB :: MBB, getChilderen :: [RTree a] }
    | Leaf {getMBB :: MBB, getElem :: a}
    deriving (Show, Eq)

m = 5
n = 10
order = (m, n)

empty :: RTree a
empty = undefined -- Node []

singleton :: MBB -> a -> RTree a
singleton mbb x = Leaf mbb x

calcMBB :: RTree a -> RTree a -> MBB
calcMBB x y = calcMBB' [getMBB x, getMBB y]

calcMBB' :: [MBB] -> MBB
calcMBB' [] = error "no MBB"
calcMBB' [x] = x
calcMBB' ((ul,br):xs) = (minUl, maxBr)
    where
    (ul', br') = calcMBB' xs
    minUl :: Point
    minUl = ((min `on` fst) ul ul', (min `on` snd) ul ul')
    maxBr :: Point
    maxBr = ((max `on` fst) br br', (max `on` snd) br br')

validRtree :: RTree a -> Bool
validRtree (Leaf mbb _) = True
validRtree x@(Node mbb c) = length c >= 2 && (and $ validRtree <$> c) && (isBalanced x)

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

insert :: (Eq a) => a -> MBB -> RTree a -> RTree a
insert e mbb oldRoot = case maybeSplitNode $ addLeaf (Leaf mbb e) oldRoot of
        [root] -> root
        [r1, r2] -> Node (calcMBB' [getMBB r1, getMBB r2]) [r1, r2]

addLeaf :: (Eq a) => RTree a -> RTree a -> RTree a
addLeaf newLeaf@Leaf{} old 
    | depth old == 1 = Node (calcMBB' [getMBB newLeaf, getMBB old]) (newLeaf : getChilderen old)
    | otherwise      = Node (calcMBB' [getMBB newLeaf, getMBB old]) newChildren
        where
--        newChildren :: [RTree a]
        newChildren = insertLeaf newLeaf (getChilderen old)
addLeaf _ _ = error "addLeaf: node"

insertLeaf :: (Eq a) => RTree a -> [RTree a] -> [RTree a]
insertLeaf newLeaf oldC = findNodeWithMinimalAreaIncrease (maybeSplitNode . addLeaf newLeaf) (getMBB newLeaf) oldC

findNodeWithMinimalAreaIncrease :: (RTree a -> [RTree a]) -> MBB -> [RTree a] -> [RTree a]
findNodeWithMinimalAreaIncrease f mbb xs = concat $ xsAndIncrease'
    where
--    xsAndIncrease :: [(RTree a, Double)]    
    xsAndIncrease = zip xs ((areaIncreasesWith mbb) <$> xs)
    minimalIncrease = minimum $ snd <$> xsAndIncrease
--    xsAndIncrease' :: [(RTree a, Double)]    
    xsAndIncrease' = map mapIf xsAndIncrease
    mapIf (x, increase) = if increase == minimalIncrease then
            f x
        else
            [x]

maybeSplitNode :: (Eq a) => RTree a -> [RTree a]
maybeSplitNode x
    | (length $ getChilderen x) > n = splitNode x
    | otherwise = [x]

-- how to split?
splitNode :: (Eq a) => RTree a -> [RTree a]
splitNode n = [x1, x2]
    where
    (l, r) = findGreatestArea $ getChilderen n
    (x1, x2) = quadSplit l r (filter (\x -> x /= l && x /= r) $ getChilderen n)

findGreatestArea :: (Eq a) => [RTree a] -> (RTree a, RTree a)
findGreatestArea list = (x, y)
    where
    listOfTripels = [(x, y, calcMBB x y) | x <- list, y <- list , x /= y]
    (x, y, _) = maximumBy (compare `on` (\(_,_,x) -> x)) listOfTripels


quadSplit :: (Eq a) => RTree a -> RTree a -> [RTree a] -> (RTree a, RTree a)
quadSplit left right [] = (left, right)
quadSplit left right unfinished
    | isLeft''  = quadSplit (mergeNodes left minimumElem) right  (filter (\x -> x /= minimumElem) unfinished)
    | otherwise = quadSplit left  (mergeNodes right minimumElem) (filter (\x -> x /= minimumElem) unfinished)
        where
        tripels = (\x -> (x, isLeft x, growth (isLeft x) x)) <$> unfinished
        isLeft x = (areaIncreasesWith (getMBB x) left) < (areaIncreasesWith (getMBB x) right)
        growth isLeft' x = case isLeft' of
            True -> areaIncreasesWith (getMBB x) left
            False -> areaIncreasesWith (getMBB x) right
        (minimumElem, isLeft'', _) = minimumBy (compare `on` (\(_,_,g) -> g)) tripels

mergeNodes :: RTree a -> RTree a -> RTree a
mergeNodes (Node mbb1 c1) (Node mbb2 c2) = Node (calcMBB' [mbb1, mbb2]) (c1 ++ c2)
mergeNodes _ _ = error "no merge for Leafs"

-- ------------
-- helpers

area :: MBB -> Double
area ((x1, y1), (x2, y2)) = (x2 - x1) * (y2 - y1)

areaIncreasesWith :: MBB -> (RTree a) -> Double
areaIncreasesWith newElem current = newArea - currentArea
    where
    currentArea = area $ getMBB current
    newArea = area $ calcMBB' [newElem, getMBB current]
