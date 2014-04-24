{-# LANGUAGE NoMonomorphismRestriction, DeriveFunctor, OverlappingInstances, DeriveDataTypeable #-}


{- |
  Module     : Data.RTree
  Copyright  : Copyright (c) 2014, Birte Wagner, Sebastian Philipp
  License    : MIT

  Maintainer : Birte Wagner, Sebastian Philipp (sebastian@spawnhost.de)
  Stability  : experimental
  Portability: not portable


  Some function names clash with "Prelude" names, therefore this module is usually
  imported @qualified@, e.g.

  > import           Data.RTree (RTree)
  > import qualified Data.RTree as RT

-}


module Data.RTree 
(
    MBB (..),
    RTree,
    empty,
    singleton,
    insert,
    lookup,
    lookupRange,
    union,
    fromList,
    toList,
    delete,
    length,
    null,
    foldWithMBB,
    getMBB
)
where

import           Prelude hiding (lookup, length, null)

import           Data.Function
import           Data.List (maximumBy, minimumBy, nub, partition)
import qualified Data.List as L (length)
import           Data.Maybe (catMaybes, mapMaybe, isJust)
import           Data.Typeable (Typeable)

import           Control.Applicative ((<$>))

data MBB = MBB {getUlx :: {-# UNPACK #-} ! Double, getUly :: {-# UNPACK #-} ! Double, getBrx :: {-# UNPACK #-} ! Double, getBry :: {-# UNPACK #-} ! Double}
    deriving (Show, Eq)

data RTree a = 
      Node4 {getMBB :: {-# UNPACK #-} ! MBB, getC1 :: ! (RTree a), getC2 :: ! (RTree a), getC3 :: ! (RTree a), getC4 :: ! (RTree a) }
    | Node3 {getMBB :: {-# UNPACK #-} ! MBB, getC1 :: ! (RTree a), getC2 :: ! (RTree a), getC3 :: ! (RTree a) }
    | Node2 {getMBB :: {-# UNPACK #-} ! MBB, getC1 :: ! (RTree a), getC2 :: ! (RTree a) }
    | Node  {getMBB ::                  MBB, getChildren' :: [RTree a] }
    | Leaf  {getMBB :: {-# UNPACK #-} ! MBB, getElem :: a}
    | Empty
    deriving (Show, Eq, Functor, Typeable)

m, n :: Int
m = 2
n = 4


-- --------------------------
-- MBB Ops

unionMBB' :: [MBB] -> MBB
unionMBB' [] = error "unionMBB': []"
unionMBB' xs = foldr1 f xs
    where
    f (MBB ulx uly brx bry) (MBB ulx' uly' brx' bry') = MBB (min ulx ulx') (min uly uly') (max brx brx') (max bry bry')

area :: MBB -> Double
area (MBB ulx uly brx bry) = (brx - ulx) * (bry - uly)

isIn :: MBB -> MBB -> Bool
isIn (MBB x11 y11 x12 y12) (MBB x21 y21 x22 y22) =  x11 <= x21 && y11 <= y21 && x12 >= x22 && y12 >= y22

unionMBB :: RTree a -> RTree a -> MBB
unionMBB x y = unionMBB' [getMBB x, getMBB y]

intersectMBB :: MBB -> MBB -> Maybe MBB
intersectMBB (MBB ulx uly brx bry) (MBB ulx' uly' brx' bry')
    | ulx'' <= brx'' && uly'' <= bry'' = Just $ MBB ulx'' uly'' brx'' bry''
    | otherwise                        = Nothing
    where
    ulx'' = max ulx ulx'
    uly'' = max uly uly'
    brx'' = min brx brx'
    bry'' = min bry bry'
-- ---------------
-- smart constuctors

empty :: RTree a
empty = Empty

null :: RTree a -> Bool
null Empty = True
null _     = False

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
lookup _   Empty = Nothing
lookup mbb t@Leaf{}
    | mbb == getMBB t = Just $ getElem t
    | otherwise = Nothing
lookup mbb t = case founds of 
    [] -> Nothing
    x:_ -> Just x
    where
    matches = filter (\x -> mbb `isIn` (getMBB x)) $ getChildren t
    founds = catMaybes $ map (lookup mbb) matches


lookupRange :: MBB -> RTree a -> [a]
lookupRange _ Empty = []
lookupRange mbb t@Leaf{}
    | getMBB t `isIn` mbb = [getElem t]
    | otherwise = []
lookupRange mbb t = founds
    where
    matches = filter intersectRTree $ getChildren t
    founds = concatMap (lookupRange mbb) matches
    intersectRTree x = isJust $ mbb `intersectMBB` (getMBB x)


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
delete' _   Empty    = Empty
delete' _   Leaf{}   = error "TODO: empty R-Tree"
delete' mbb t = fromList' $ orphans ++ [newValidNode]
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
foldWithMBB :: (MBB -> a -> b) -> (MBB -> [b] -> b) -> b -> RTree a -> b
foldWithMBB _ _ n Empty    = n
foldWithMBB f _ _ t@Leaf{} = f (getMBB t) (getElem t)
foldWithMBB f g n t        = g (getMBB t) $ foldWithMBB f g n <$> (getChildren t)
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
length Empty = 0
length (Leaf {}) = 1
length t = sum $ length <$> (getChildren t)

--delete' :: MBB -> RTree a -> Either (RTree a) [(MBB, a)]
