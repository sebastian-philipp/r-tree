{-# LANGUAGE NoMonomorphismRestriction, DeriveFunctor, OverlappingInstances #-}

-- Copyright (c) 2014, Birte Wagner, Sebastian Philipp
--

module Data.RTree where

import Prelude hiding (lookup)
import Data.Function
import Data.List (maximumBy, minimumBy, nub, partition)
import Data.Maybe (catMaybes)
import Control.Applicative ((<$>))

import Graphics.Gnuplot.Simple

type Point = (Double, Double)

type Rect = (Point, Point)
type MBB = Rect

data RTree a = 
      Node {getMBB :: MBB, getChilderen :: [RTree a] }
    | Leaf {getMBB :: MBB, getElem :: a}
    deriving (Show, Eq, Functor)

m, n :: Int
m = 5
n = 10

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

validRtree :: Show b => b -> RTree a -> Bool
validRtree context Leaf{} = True
validRtree context x@(Node mbb c) = case length c >= m && length c <= n && (and $ (validRtree context) <$> c) && (isBalanced x) of
    True -> True
    False -> error ( "invalid " ++ show (length c) ++ " " ++ show context )
    where
    isBalanced :: RTree a -> Bool 
    isBalanced (Leaf _ _ ) = True
    isBalanced (Node _ c) = (and $ isBalanced <$> c) && (and $ (== depth (head c)) <$> (depth <$> c))

depth :: RTree a -> Int
depth (Leaf _ _ ) = 0
depth (Node _ c) = 1 + (depth $ head c)


length' :: RTree a -> Int 
length' (Leaf {}) = 1
length' (Node _ c) = sum $ length' <$> c

-- ----------------------------------

insert :: a -> MBB -> RTree a -> RTree a
insert e mbb oldRoot = insert' (Leaf mbb e) oldRoot

insert' :: RTree a-> RTree a -> RTree a
insert' Node{} _ = error "insert: node"
insert' newLeaf oldRoot = case addAndSplit newLeaf oldRoot of
        [root] -> root
        [r1, r2] -> combineChildren [r1, r2]

addAndSplit :: RTree a -> RTree a -> [RTree a]
addAndSplit leaf e@Node{} = maybeSplitNode $ (addLeaf leaf) e
addAndSplit leaf e@Leaf{} = [leaf, e] -- root case

addLeaf :: RTree a -> RTree a -> RTree a
addLeaf leaf@Leaf{} e 
    | depth e == 1 = Node (leaf `unifyMBB` e) (leaf : (filter (on (/=) getMBB leaf) $ getChilderen e))
    | otherwise    = Node (leaf `unifyMBB` e) (insertLeaf leaf (getChilderen e))
addLeaf _ _ = error "addLeaf: node"


maybeSplitNode :: RTree a -> [RTree a]
maybeSplitNode Leaf{} = error "splitNode: Leaf"
maybeSplitNode x
    | (length $ getChilderen x) > n = splitNode x
    | otherwise = [x]

insertLeaf :: RTree a -> [RTree a] -> [RTree a]
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
splitNode :: RTree a -> [RTree a]
splitNode Leaf{} = error "splitNode: Leaf"
splitNode e = [combineChildren x1, combineChildren x2]
    where
    (l, r) = findGreatestArea $ getChilderen e
    (x1, x2) = quadSplit [l] [r] unfinished
    unfinished = filter (on (/=) getMBB l) $ filter (on (/=) getMBB r) $ getChilderen e

findGreatestArea :: [RTree a] -> (RTree a, RTree a)
findGreatestArea xs = (x', y')
    where
    xs' = zip xs [1..]
    listOfTripels = [(fst x, fst y, on unifyMBB fst x y) | x <- xs', y <- xs', ((<) `on` snd) x y]
    (x', y', _) = maximumBy (compare `on` (\(_,_,x) -> area x)) listOfTripels


quadSplit :: [RTree a] -> [RTree a] -> [RTree a] -> ([RTree a], [RTree a])
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
        newRest = (filter (on (/=) getMBB minimumElem) unfinished)

mergeNodes :: RTree a -> RTree a -> RTree a
mergeNodes x@Node{} y@Node{} = Node (unifyMBB x y) (on (++) getChilderen x y)
mergeNodes _ _               = error "no merge for Leafs"

fromList :: [(MBB, a)] -> RTree a
fromList l = fromList' $ (uncurry singleton) <$> l

fromList' :: [RTree a] -> RTree a
fromList' [] = error "fromList' empty"
fromList' [t] = t
fromList' ts = foldr1 insert' ts
-- ------------
-- helpers

area :: MBB -> Double
area ((x1, y1), (x2, y2)) = (x2 - x1) * (y2 - y1)

areaIncreasesWith :: RTree a -> (RTree a) -> Double
areaIncreasesWith newElem current = newArea - currentArea
    where
    currentArea = area $ getMBB current
    newArea = area $ unifyMBB newElem current

-- -----------------
-- lookup

isIn :: MBB -> MBB -> Bool
isIn ((x11, y11), (x12, y12)) ((x21, y21), (x22, y22)) =  x11 <= x21 && y11 <= y21 && x12 >= x22 && y21 >= y22


lookup :: MBB -> RTree a -> Maybe a
lookup mbb t@Leaf{}
    | mbb == getMBB t = Just $ getElem t
    | otherwise = Nothing
lookup mbb t@Node{} = case founds of 
    [] -> Nothing
    x:_ -> Just x
    where
    matches = filter (\x -> mbb `isIn` (getMBB x)) $ getChilderen t
    founds = catMaybes $ map (lookup mbb) matches


-- -----------
-- delete


delete :: MBB -> RTree a -> RTree a
delete mbb t@Leaf{} = error "TODO: empty R-Tree"
delete mbb t@Node{} = fromList' $ orphans ++ [newValidNode]
    where
    (matches, noMatches) = partition (\x -> mbb `isIn` (getMBB x)) $ getChilderen t
    matches' = case matches of
        [] -> []
        [Leaf{}] -> []
        xs -> map (delete mbb) xs
    (orphans, validMatches) = foldr handleInvalid ([], []) matches'
--  handleInvalid :: RTree a -> ([RTree a], [RTree a]) -> ([RTree a], [RTree a])
    handleInvalid (Node _ children) (orphans', validMatches')
        | length children < m = (children ++ orphans', validMatches')
        | otherwise = (orphans', t:validMatches')
    handleInvalid _ _ = error "delete/handleInvalid: leaf"    
    newValidNode = combineChildren $ validMatches ++ noMatches



--delete' :: MBB -> RTree a -> Either (RTree a) [(MBB, a)]

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
t_5 = fromList' [t_1, t_2, t_3, t_4]
t_p = Node {getMBB = ((6469.0,9103.0),(6656.0,9721.0)), getChilderen = [
    Leaf {getMBB = ((6469.0,9103.0),(6469.0,9721.0)), getElem = ()},
    Leaf {getMBB = ((6786.0,9678.0),(6656.0,9651.0)), getElem = ()},
    Leaf {getMBB = ((6593.0,9103.0),(6593.0,9721.0)), getElem = ()}]}
t_pp = Leaf {getMBB = ((6531.0,9103.0),(6531.0,9721.0)), getElem = ()}
t_ppp = insert' t_pp t_p


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
    plotPaths [Key Nothing, XRange $ p20 ulx brx, YRange $ p20 uly bry] $ rtreeToPaths tree
    where
    ((ulx, uly),(brx, bry))  = getMBB tree
    p20 l r = (l - ((r-l) / 5), r + ((r-l) / 5))


testData :: FilePath -> IO (RTree ())
testData p = do
    d <- lines <$> readFile p
    let pairs = zip (listToMBB <$> (map read d)) (replicate 100000000 ())
    return $ fromList pairs
    where
        listToMBB :: [Double] -> MBB
        listToMBB [ulx, uly, brx, bry] = ((ulx, uly),(brx, bry))

