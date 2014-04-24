{-# LANGUAGE NoMonomorphismRestriction, DeriveFunctor, OverlappingInstances #-}

-- Copyright (c) 2014, Birte Wagner, Sebastian Philipp
--

module Data.RTree where

import           Prelude hiding (lookup, length)

import           Data.Function
import           Data.List (maximumBy, minimumBy, nub, partition)
import qualified Data.List as L (length)
import           Data.Maybe (catMaybes)

import           Control.Applicative ((<$>))

import Graphics.Gnuplot.Simple

type Point = (Double, Double)

type Rect = (Point, Point)
type MBB = Rect

data RTree a = 
      Node4 {getMBB :: MBB, getC1 :: RTree a, getC2 :: RTree a, getC3 :: RTree a, getC4 :: RTree a }
    | Node3 {getMBB :: MBB, getC1 :: RTree a, getC2 :: RTree a, getC3 :: RTree a }
    | Node2 {getMBB :: MBB, getC1 :: RTree a, getC2 :: RTree a }
    | Node {getMBB :: MBB, getChildren' :: [RTree a] }
    | Leaf {getMBB :: MBB, getElem :: a}
    | Empty
    deriving (Show, Eq, Functor)

m, n :: Int
m = 2
n = 4


-- --------------------------
-- MBB Ops

unionMBB' :: [MBB] -> MBB
unionMBB' [] = error "no MBB"
unionMBB' [x] = x
unionMBB' ((ul,br):xs) = (minUl, maxBr)
    where
    (ul', br') = unionMBB' xs
    minUl :: Point
    minUl = ((min `on` fst) ul ul', (min `on` snd) ul ul')
    maxBr :: Point
    maxBr = ((max `on` fst) br br', (max `on` snd) br br')

area :: MBB -> Double
area ((x1, y1), (x2, y2)) = (x2 - x1) * (y2 - y1)

isIn :: MBB -> MBB -> Bool
isIn ((x11, y11), (x12, y12)) ((x21, y21), (x22, y22)) =  x11 <= x21 && y11 <= y21 && x12 >= x22 && y12 >= y22

unionMBB :: RTree a -> RTree a -> MBB
unionMBB x y = unionMBB' [getMBB x, getMBB y]

-- ---------------
-- smart constuctors

empty :: RTree a
empty = Empty

singleton :: MBB -> a -> RTree a
singleton mbb x = Leaf mbb x

node :: MBB -> [RTree a] -> RTree a
node mbb [x,y]     = Node2 mbb x y
node mbb [x,y,z]   = Node3 mbb x y z
node mbb [x,y,z,w] = Node4 mbb x y z w
node _   []        = error "node: empty"
node mbb xs        = Node mbb xs

createNodeWithChildren :: [RTree a] -> RTree a
createNodeWithChildren c = node (unionMBB' $ getMBB <$> c) c

norm :: RTree a -> RTree a
norm (Node4 mbb x y z w) = Node mbb [x,y,z,w]
norm (Node3 mbb x y z)   = Node mbb [x,y,z]
norm (Node2 mbb x y)     = Node mbb [x,y]
norm x = x

getChildren :: RTree a -> [RTree a]
getChildren Empty = error "getChildren: Empty"
getChildren Leaf{} = error "getChildren: Leaf"
getChildren t = getChildren' $ norm t

-- ----------------------------------
-- creation

fromList :: [(MBB, a)] -> RTree a
fromList l = fromList' $ (uncurry singleton) <$> l

fromList' :: [RTree a] -> RTree a
fromList' [] = error "fromList' empty"
fromList' [t] = t
fromList' ts = foldr1 union ts

toList :: RTree a -> [(MBB, a)]
toList Empty = []
toList (Leaf mbb x) = [(mbb, x)]
toList t = concatMap toList $ getChildren t
-- ----------------------------------
-- insert 

insert :: MBB -> a -> RTree a -> RTree a
insert mbb e oldRoot = union (singleton mbb e) oldRoot

union :: RTree a -> RTree a -> RTree a
union Empty{} t           = t
union t       Empty{}     = t
union t1@Leaf{} t2@Leaf{} = createNodeWithChildren [t1, t2] -- root case
union left right
    | depth left > depth right              = union right left
    | depth left == depth right             = fromList' $ (getChildren left) ++ [right]
    | (L.length $ getChildren newNode) > n = createNodeWithChildren $ splitNode newNode
    | otherwise                             = newNode
    where
    newNode = addLeaf left right

addLeaf :: RTree a -> RTree a -> RTree a
addLeaf left right 
    | depth left + 1 == depth right = node (left `unionMBB` right) (left : (filter (on (/=) getMBB left) $ getChildren right))
    | otherwise                 = node (left `unionMBB` right) newChildren
    where
    newChildren = findNodeWithMinimalAreaIncrease left (getChildren right)

findNodeWithMinimalAreaIncrease :: RTree a -> [RTree a] -> [RTree a]
findNodeWithMinimalAreaIncrease leaf children = concat $ xsAndIncrease'
    where
--  xsAndIncrease :: [(RTree a, Double)]    
    xsAndIncrease = zip children ((areaIncreasesWith leaf) <$> children)
    minimalIncrease = minimum $ snd <$> xsAndIncrease
--  xsAndIncrease' :: [(RTree a, Double)]    
    xsAndIncrease' = map mapIf xsAndIncrease
    mapIf (x, increase) = if increase == minimalIncrease then
            unionSplit leaf x
        else
            [x]

unionSplit :: RTree a -> RTree a -> [RTree a]
unionSplit leaf e
    | (L.length $ getChildren newLeaf) > n = splitNode newLeaf
    | otherwise = [newLeaf]
    where
    newLeaf = addLeaf leaf e

-- | /O(n²)/ solution
splitNode :: RTree a -> [RTree a]
splitNode Leaf{} = error "splitNode: Leaf"
splitNode e = [createNodeWithChildren x1, createNodeWithChildren x2]
    where
    (l, r) = findGreatestArea $ getChildren e
    (x1, x2) = quadSplit [l] [r] unfinished
    unfinished = filter (on (/=) getMBB l) $ filter (on (/=) getMBB r) $ getChildren e

findGreatestArea :: [RTree a] -> (RTree a, RTree a)
findGreatestArea xs = (x', y')
    where
    xs' = zip xs [(1::Int)..]
    listOfTripels = [(fst x, fst y, on unionMBB fst x y) | x <- xs', y <- xs', ((<) `on` snd) x y]
    (x', y', _) = maximumBy (compare `on` (\(_,_,x) -> area x)) listOfTripels


quadSplit :: [RTree a] -> [RTree a] -> [RTree a] -> ([RTree a], [RTree a])
quadSplit left right [] = (left, right)
quadSplit left right unfinished
    | (L.length left) + (L.length unfinished)  <= m = (left ++ unfinished, right)
    | (L.length right) + (L.length unfinished) <= m = (left, right ++ unfinished)
    | isLeft''                                      = quadSplit (minimumElem : left) right  newRest
    | otherwise                                     = quadSplit left  (minimumElem : right) newRest
        where
--      makeTripel :: RTree a -> (RTree a, Bool, Double)
        makeTripel x = (x, isLeft, growth)
            where
            isLeft = (areaIncreasesWithLeft) < (areaIncreasesWithRight)
            growth = case isLeft of
                True -> areaIncreasesWithLeft
                False -> areaIncreasesWithRight
            areaIncreasesWithLeft  = (areaIncreasesWith x (createNodeWithChildren left))
            areaIncreasesWithRight = (areaIncreasesWith x (createNodeWithChildren right))
        (minimumElem, isLeft'', _) = minimumBy (compare `on` (\(_,_,g) -> g)) $ makeTripel <$> unfinished
        newRest = (filter (on (/=) getMBB minimumElem) unfinished)

--mergeNodes :: RTree a -> RTree a -> RTree a
--mergeNodes x@Node{} y@Node{} = node (unionMBB x y) (on (++) getChildren x y)
--mergeNodes _ _               = error "no merge for Leafs"

-- ------------
-- helpers


areaIncreasesWith :: RTree a -> (RTree a) -> Double
areaIncreasesWith newElem current = newArea - currentArea
    where
    currentArea = area $ getMBB current
    newArea = area $ unionMBB newElem current

-- -----------------
-- lookup


lookup :: MBB -> RTree a -> Maybe a
lookup mbb t@Leaf{}
    | mbb == getMBB t = Just $ getElem t
    | otherwise = Nothing
lookup mbb t@Node{} = case founds of 
    [] -> Nothing
    x:_ -> Just x
    where
    matches = filter (\x -> mbb `isIn` (getMBB x)) $ getChildren t
    founds = catMaybes $ map (lookup mbb) matches


-- -----------
-- delete

delete :: MBB -> RTree a -> RTree a
delete mbb root = if L.length (getChildren newRoot) == 1 then
        head $ getChildren newRoot
    else
        newRoot
    where
    newRoot = delete' mbb root


delete' :: MBB -> RTree a -> RTree a
delete' _   Leaf{}   = error "TODO: empty R-Tree"
delete' mbb t@Node{} = fromList' $ orphans ++ [newValidNode]
    where
    (matches, noMatches) = partition (\x -> mbb `isIn` (getMBB x)) $ getChildren t
    matches' = case matches of
        [] -> []
        [Leaf{}] -> []
        xs -> map (delete' mbb) xs
    (orphans, validMatches) = foldr handleInvalid ([], []) matches'
--  handleInvalid :: RTree a -> ([RTree a], [RTree a]) -> ([RTree a], [RTree a])
    handleInvalid invalidNode (orphans', validMatches')
        | L.length children < m = (children ++ orphans', validMatches')
        | otherwise = (orphans', t:validMatches')
        where
        children = getChildren invalidNode
    newValidNode = createNodeWithChildren $ validMatches ++ noMatches

-- ---------------

isValid :: Show b => b -> RTree a -> Bool
isValid context Leaf{} = True
isValid context x = case L.length c >= m && L.length c <= n && (and $ (isValid context) <$> c) && (isBalanced x) of
    True -> True
    False -> error ( "invalid " ++ show (L.length c) ++ " " ++ show context )
    where
    isBalanced :: RTree a -> Bool 
    isBalanced (Leaf _ _ ) = True
    isBalanced x' = (and $ isBalanced <$> c') && (and $ (== depth (head c')) <$> (depth <$> c'))
        where
        c' = getChildren x'
    c = getChildren x

depth :: RTree a -> Int
depth (Leaf _ _ ) = 0
depth t = 1 + (depth $ head $ getChildren t)


length :: RTree a -> Int 
length (Leaf {}) = 1
length (Node _ c) = sum $ length <$> c

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
t_p = node ((6469.0,9103.0),(6656.0,9721.0)) [
    Leaf {getMBB = ((6469.0,9103.0),(6469.0,9721.0)), getElem = ()},
    Leaf {getMBB = ((6786.0,9678.0),(6656.0,9651.0)), getElem = ()},
    Leaf {getMBB = ((6593.0,9103.0),(6593.0,9721.0)), getElem = ()}]
t_pp = Leaf {getMBB = ((6531.0,9103.0),(6531.0,9721.0)), getElem = ()}
t_ppp = union t_pp t_p


nodeToPath :: RTree a -> [(Double, Double)]
nodeToPath e = [(ulx, uly),(brx, uly),(brx, bry),(ulx, bry),(ulx, uly)]
    where
    ((ulx, uly),(brx, bry))  = getMBB e

rtreeToPaths :: RTree a -> [[(Double, Double)]]
rtreeToPaths e@Leaf{} = [nodeToPath e]
rtreeToPaths e@Node{} = [nodeToPath e] ++ (concat $ rtreeToPaths <$> (getChildren e))
   

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

