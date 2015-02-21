{-# LANGUAGE NoMonomorphismRestriction, DeriveFunctor, OverlappingInstances, DeriveDataTypeable, BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
    Module     : Data.RTree.Base
    Copyright  : Copyright (c) 2015, Birte Wagner, Sebastian Philipp
    License    : MIT

    Maintainer : Birte Wagner, Sebastian Philipp (sebastian@spawnhost.de)
    Stability  : experimental
    Portability: not portable

    Internal implementations. Use 'Data.RTree' instead or use at you own risc.
-}


module Data.RTree.Base
(
    -- * Data Type
    RTree (..)
    -- * Constructors
    , empty
    , singleton
    -- * Modification
    , insert
    , insertWith
    , delete
    , mapMaybe
    -- ** Merging
    , union
    , unionWith
    -- * Searching and Properties
    , lookup
    , lookupRange
    , lookupRangeWithKey
    , lookupContainsRangeWithKey
    , lookupContainsRange
    , length
    , null
    , keys
    , values
    -- * Lists
    , fromList
    , toList
    -- * Internal and Testing
    , foldWithMBB
    , pp
    , isValid
    , unionDistinct
    , unionDistinctWith
    , fromList'
    , unionDistinctSplit
    , depth
    , areaIncreasesWith
    , partition
    , getChildren
    , unionMBB'
    , createNodeWithChildren
    , n
    , splitNode
    , node
)
where

import           Prelude hiding (lookup, length, null, map)

import           Data.Binary
import           Data.Function (on)
import           Data.List (maximumBy, minimumBy, partition)
import qualified Data.List as L (length,map)
import           Data.Maybe (catMaybes, isJust)
import qualified Data.Maybe as Maybe (mapMaybe)
import           Data.Monoid (Monoid, mempty, mappend)
import           Data.Typeable (Typeable)

import           Control.Applicative ((<$>))
import           Control.DeepSeq (NFData, rnf)

import           GHC.Generics (Generic)

import           Data.RTree.MBB hiding (mbb)

data RTree a =
      Node4 {getMBB :: {-# UNPACK #-} ! MBB, getC1 :: ! (RTree a), getC2 :: ! (RTree a), getC3 :: ! (RTree a), getC4 :: ! (RTree a) }
    | Node3 {getMBB :: {-# UNPACK #-} ! MBB, getC1 :: ! (RTree a), getC2 :: ! (RTree a), getC3 :: ! (RTree a) }
    | Node2 {getMBB :: {-# UNPACK #-} ! MBB, getC1 :: ! (RTree a), getC2 :: ! (RTree a) }
    | Node  {getMBB ::                  MBB, getChildren' :: [RTree a] }
    | Leaf  {getMBB :: {-# UNPACK #-} ! MBB, getElem :: a}
    | Empty
    deriving (Show, Eq, Typeable, Generic, Functor)

-- | It is possible, to change these constants, but the tree won't be space optimal anymore.
m, n :: Int
m = 2
n = 4


unionMBB' :: RTree a -> RTree a -> MBB
unionMBB' = unionMBB `on` getMBB

-- ---------------
-- smart constuctors

-- | creates an empty tree
empty :: RTree a
empty = Empty

-- | returns 'True', if empty
--
-- prop> null empty = True
null :: RTree a -> Bool
null Empty = True
null _     = False

-- | creates a single element tree
singleton :: MBB -> a -> RTree a
singleton mbb x = Leaf mbb x

node :: MBB -> [RTree a] -> RTree a
node mbb [x,y]     = Node2 mbb x y
node mbb [x,y,z]   = Node3 mbb x y z
node mbb [x,y,z,w] = Node4 mbb x y z w
node _   []        = error "node: empty"
node mbb xs        = Node mbb xs

createNodeWithChildren :: [RTree a] -> RTree a
createNodeWithChildren c = node (unionsMBB $ getMBB <$> c) c

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
-- Lists

-- | creates a tree out of pairs
fromList :: [(MBB, a)] -> RTree a
fromList l = fromList' $ (uncurry singleton) <$> l

-- | merges all singletons into a single tree.
fromList' :: [RTree a] -> RTree a
fromList' [] = empty
fromList' ts = foldr1 unionDistinct ts

-- | creates a list of pairs out of a tree
--
-- prop> toList t = zip (keys t) (values t)
toList :: RTree a -> [(MBB, a)]
toList Empty = []
toList (Leaf mbb x) = [(mbb, x)]
toList t = concatMap toList $ getChildren t

-- | returns all keys in this tree
--
-- prop> toList t = zip (keys t) (values t)
keys :: RTree a -> [MBB]
keys = foldWithMBB handleLeaf handleNode []
    where
    handleLeaf mbb _ = [mbb]
    handleNode _ xs  = concat xs

-- | returns all values in this tree
--
-- prop> toList t = zip (keys t) (values t)
values :: RTree a -> [a]
values = foldWithMBB handleLeaf handleNode []
    where
    handleLeaf _ x = [x]
    handleNode _ xs  = concat xs


-- ----------------------------------
-- insert

-- | Inserts an element whith the given 'MBB' and a value in a tree. The combining function will be used if the value already exists.
insertWith :: (a -> a -> a) -> MBB -> a -> RTree a -> RTree a
insertWith f mbb e oldRoot = unionDistinctWith f (singleton mbb e) oldRoot

-- | Inserts an element whith the given 'MBB' and a value in a tree. An existing value will be overwritten with the given one.
--
-- prop> insert = insertWith const
insert :: MBB -> a -> RTree a -> RTree a
insert = insertWith const

simpleMergeEqNode :: (a -> a -> a) -> RTree a -> RTree a -> RTree a
simpleMergeEqNode f l@Leaf{} r = Leaf (getMBB l) (on f getElem l r)
simpleMergeEqNode _ l _ = l

-- | Unifies left and right 'RTree'. Will create invalid trees, if the tree is not a leaf and contains 'MBB's which
--  also exists in the left tree. Much faster than union, though.
unionDistinctWith :: (a -> a -> a) -> RTree a -> RTree a -> RTree a
unionDistinctWith _ Empty{} t           = t
unionDistinctWith _ t       Empty{}     = t
unionDistinctWith f t1@Leaf{} t2@Leaf{}
    | on (==) getMBB t1 t2              = simpleMergeEqNode f t1 t2
    | otherwise                         = createNodeWithChildren [t1, t2] -- root case
unionDistinctWith f left right
    | depth left > depth right              = unionDistinctWith f right left
    | depth left == depth right             = fromList' $ (getChildren left) ++ [right]
    | (L.length $ getChildren newNode) > n  = createNodeWithChildren $ splitNode newNode
    | otherwise                             = newNode
    where
    newNode = addLeaf f left right

-- | Únifies left and right 'RTree'. Will create invalid trees, if the tree is not a leaf and contains 'MBB's which
--  also exists in the left tree. Much faster than union, though.
unionDistinct :: RTree a -> RTree a -> RTree a
unionDistinct = unionDistinctWith const

addLeaf :: (a -> a -> a) -> RTree a -> RTree a -> RTree a
addLeaf f left right
    | depth left + 1 == depth right = node (newNode `unionMBB'` right) (newNode : nonEq)
    | otherwise                     = node (left `unionMBB'` right) newChildren
    where
    newChildren = findNodeWithMinimalAreaIncrease f left (getChildren right)
    (eq, nonEq) = partition (on (==) getMBB left) $ getChildren right
    newNode = case eq of
        [] -> left
        [x] -> simpleMergeEqNode f left x
        _ -> error "addLeaf: invalid RTree"

findNodeWithMinimalAreaIncrease :: (a -> a -> a) -> RTree a -> [RTree a] -> [RTree a]
findNodeWithMinimalAreaIncrease f leaf children = splitMinimal xsAndIncrease
    where
--  xsAndIncrease :: [(RTree a, Double)]
    xsAndIncrease = zip children ((areaIncreasesWith leaf) <$> children)
    minimalIncrease = minimum $ snd <$> xsAndIncrease
--  xsAndIncrease' :: [(RTree a, Double)]
    splitMinimal [] = []
    splitMinimal ((t,mbb):xs)
        | mbb == minimalIncrease = unionDistinctSplit f leaf t ++ (fst <$> xs)
        | otherwise            = t : splitMinimal xs

unionDistinctSplit :: (a -> a -> a) -> RTree a -> RTree a -> [RTree a]
unionDistinctSplit f leaf e
    | (L.length $ getChildren newLeaf) > n = splitNode newLeaf
    | otherwise = [newLeaf]
    where
    newLeaf = addLeaf f leaf e

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
    listOfTripels = [(fst x, fst y, on unionMBB' fst x y) | x <- xs', y <- xs', ((<) `on` snd) x y]
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
--mergeNodes x@Node{} y@Node{} = node (unionMBB' x y) (on (++) getChildren x y)
--mergeNodes _ _               = error "no merge for Leafs"

-- ------------
-- helpers


areaIncreasesWith :: RTree a -> (RTree a) -> Double
areaIncreasesWith newElem current = newArea - currentArea
    where
    currentArea = area $ getMBB current
    newArea = area $ unionMBB' newElem current

-- -----------------
-- lookup

-- | returns the value if it exists in the tree
lookup :: MBB -> RTree a -> Maybe a
lookup _   Empty = Nothing
lookup mbb t@Leaf{}
    | mbb == getMBB t = Just $ getElem t
    | otherwise = Nothing
lookup mbb t = case founds of
    [] -> Nothing
    x:_ -> Just x
    where
    matches = filter (\x -> (getMBB x) `containsMBB` mbb) $ getChildren t
    founds = catMaybes $ L.map (lookup mbb) matches

-- | returns all keys and values, which are located in the given bounding box.
lookupRangeWithKey :: MBB -> RTree a -> [(MBB, a)]
lookupRangeWithKey _ Empty = []
lookupRangeWithKey mbb t@Leaf{}
    | mbb `containsMBB` (getMBB t) = [(getMBB t, getElem t)]
    | otherwise = []
lookupRangeWithKey mbb t = founds
    where
    matches = filter intersectRTree $ getChildren t
    founds = concatMap (lookupRangeWithKey mbb) matches
    intersectRTree x = isJust $ mbb `intersectMBB` (getMBB x)

-- | returns all values, which are located in the given bounding box.
lookupRange :: MBB -> RTree a -> [a]
lookupRange mbb t = snd <$> (lookupRangeWithKey mbb t)

-- | returns all keys and values containing the given bounding box
lookupContainsRangeWithKey :: MBB -> RTree a -> [(MBB, a)]
lookupContainsRangeWithKey _ Empty = []
lookupContainsRangeWithKey mbb t@Leaf{}
    | (getMBB t) `containsMBB` mbb = [(getMBB t, getElem t)]
    | otherwise = []
lookupContainsRangeWithKey mbb t = founds
    where
    matches = filter intersectRTree $ getChildren t
    founds = concatMap (lookupContainsRangeWithKey mbb) matches
    intersectRTree x = (getMBB x) `containsMBB` mbb

-- | returns all values containing the given bounding box
lookupContainsRange :: MBB -> RTree a -> [a]
lookupContainsRange mbb t = snd <$> (lookupContainsRangeWithKey mbb t)

-- -----------
-- delete

-- | Delete a key and its value from the RTree. When the key is not a member of the tree, the original tree is returned.
delete :: MBB -> RTree a -> RTree a
delete _   Empty = Empty
delete mbb  t@Leaf{}
    | mbb == getMBB t = Empty
    | otherwise       = t
delete mbb root
    | L.length (getChildren newRoot) == 1 = head $ getChildren newRoot
    | otherwise                           = newRoot
    where
    newRoot = delete' mbb root


delete' :: MBB -> RTree a -> RTree a
delete' mbb  t@Leaf{}
    | mbb == getMBB t = Empty
    | otherwise       = t
delete' mbb t = fromList' $ orphans ++ [newValidNode]
    where
    (matches, noMatches) = partition (\x -> (getMBB x) `containsMBB` mbb) $ getChildren t
    matches' = filter (not . null) $ L.map (delete' mbb) matches
    (orphans, validMatches) = foldr handleInvalid ([], []) matches'
--  handleInvalid :: RTree a -> ([RTree a], [RTree a]) -> ([RTree a], [RTree a])
    handleInvalid l@Leaf{} (orphans', validMatches') = (orphans', l:validMatches')
    handleInvalid invalidNode (orphans', validMatches')
        | L.length children < m = (children ++ orphans', validMatches')
        | otherwise = (orphans', invalidNode:validMatches')
        where
        children = getChildren invalidNode
    newValidNode = createNodeWithChildren $ validMatches ++ noMatches

-- ---------------
foldWithMBB :: (MBB -> a -> b) -> (MBB -> [b] -> b) -> b -> RTree a -> b
foldWithMBB _ _ n' Empty    = n'
foldWithMBB f _ _ t@Leaf{} = f (getMBB t) (getElem t)
foldWithMBB f g n' t        = g (getMBB t) $ foldWithMBB f g n' <$> (getChildren t)

-- | Unifies the first and the second tree into one. The combining function is used for elemets which exists in both trees.
unionWith :: (a -> a -> a) -> RTree a -> RTree a -> RTree a
unionWith _ l     Empty    = l
unionWith _ Empty r        = r
unionWith f t1 t2
    | depth t1 <= depth t2 = foldr (uncurry (insertWith f)) t2 (toList t1)
    | otherwise            = unionWith f t2 t1

-- | Unifies the first and the second tree into one.
-- If an 'MBB' is a key in both trees, the value from the left tree is chosen.
--
-- prop> union = unionWith const
union :: RTree a -> RTree a -> RTree a
union = unionWith const

-- | map, which also filters Nothing values
mapMaybe :: (a -> Maybe b) -> RTree a -> RTree b
mapMaybe f t = fromList $ Maybe.mapMaybe func $ toList t
    where
    func (mbb,x) = case f x of
            Nothing -> Nothing
            Just x' -> Just (mbb, x')

-- ---------------

isValid :: Show b => b -> RTree a -> Bool
isValid _ Empty = True
isValid _ Leaf{} = True
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

-- ----------------------
i_ :: String
i_ = "    "

pp :: (Show a) => RTree a -> IO ()
pp = pp' ""

pp' :: (Show a) => String -> RTree a -> IO ()
pp' i Empty                 = putStrLn $ i ++ "Empty"
pp' i (Leaf mbb x)          = putStrLn $ i ++ "Leaf " ++ (show mbb) ++ " " ++ (show x)
pp' i (Node mbb cs)         = do
    putStrLn $ i ++ "Node " ++ (show mbb)
    mapM_ (pp' (i ++ i_)) cs
pp' i (Node2 mbb c1 c2)       = do
    putStrLn $ i ++ "Node2 " ++ (show mbb)
    mapM_ (pp' (i ++ i_)) [c1, c2]
pp' i (Node3 mbb c1 c2 c3)    = do
    putStrLn $ i ++ "Node3 " ++ (show mbb)
    mapM_ (pp' (i ++ i_)) [c1, c2, c3]
pp' i (Node4 mbb c1 c2 c3 c4) = do
    putStrLn $ i ++ "Node4 " ++ (show mbb)
    mapM_ (pp' (i ++ i_)) [c1, c2, c3, c4]

-- ----------------------

depth :: RTree a -> Int
depth Empty = 0
depth (Leaf _ _ ) = 1
depth t = 1 + (depth $ head $ getChildren t)

-- | returns the number of elements in a tree
length :: RTree a -> Int
length Empty = 0
length (Leaf {}) = 1
length t = sum $ length <$> (getChildren t)

--delete' :: MBB -> RTree a -> Either (RTree a) [(MBB, a)]

instance NFData a => NFData (RTree a) where
    rnf (Empty)               = ()
    rnf (Leaf _ e)            = {-rnf m `seq`-} rnf e
    rnf (Node _ cs)           = {-rnf m `seq`-} rnf cs
    rnf (Node2 _ c1 c2)       = {-rnf m `seq`-} rnf c1 `seq` rnf c2
    rnf (Node3 _ c1 c2 c3)    = {-rnf m `seq`-} rnf c1 `seq` rnf c2 `seq` rnf c3
    rnf (Node4 _ c1 c2 c3 c4) = {-rnf m `seq`-} rnf c1 `seq` rnf c2 `seq` rnf c3 `seq` rnf c4


instance  (Binary a) => Binary (RTree a) where
    put (Empty)         = put (0::Word8)
    put (Leaf mbb e)    = put (1::Word8)  >> put mbb >> put e
    put t               = put (2::Word8)  >> put (getMBB t) >> put (getChildren t)

    get = do
          !tag <- getWord8
          case tag of
                   0 -> return Empty
                   1 -> do
                        !mbb <- get
                        !e <- get
                        return $! Leaf mbb e
                   2 -> do
                        !mbb <- get
                        !c <- get
                        return $! node mbb c
                   _ -> fail "RTree.get: error while decoding RTree"


instance (Monoid a) => Monoid (RTree a) where
    mempty = empty
    mappend = unionWith mappend
