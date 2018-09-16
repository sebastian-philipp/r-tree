{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
    Module     : Data.RTree.Strict
    Copyright  : Copyright (c) 2015, Birte Wagner, Sebastian Philipp
    License    : MIT

    Maintainer : Birte Wagner, Sebastian Philipp (sebastian@spawnhost.de)
    Stability  : experimental
    Portability: not portable

    This is the Strict version of 'Data.RTree.RTree'

    the following property should be true (by using 'GHC.AssertNF.isNF' ) :

    >>> propNF :: RTree a -> IO Bool
    >>> propNF e = isNF $! e

-}


module Data.RTree.Strict
(
    -- * 'MBB'
    MBB
    , MBB.mbb
    -- * Data Type
    , RTree ()
    , toLazy
    , toStrict
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
    , intersectWithKey
    , intersect
    , lookupRange
    , lookupRangeWithKey
    , lookupContainsRange
    , lookupContainsRangeWithKey
    , length
    , null
    , keys
    , values
    -- * Lists
    , fromList
    , toList
) where

import           Prelude hiding (lookup, length, null, map)

import           Data.Binary
import           Data.Function (on)
import qualified Data.List as L (length)
import qualified Data.Maybe as Maybe (mapMaybe)
import           Data.Semigroup
import           Data.Typeable (Typeable)

import           Control.DeepSeq (NFData)
import           GHC.Generics (Generic)
--import           Data.RTree.Base hiding (RTree, singleton, fromList, insertWith, unionDistinctWith, unionWith, insert, mapMaybe, union, fromList', unionDistinct, unionDistinctSplit)
import qualified Data.RTree.Base as Lazy
import           Data.RTree.MBB hiding (mbb)
import qualified Data.RTree.MBB as MBB


newtype RTree a = RTree {toLazy' :: Lazy.RTree a}
    deriving (Show, Eq, Typeable, Generic, NFData, Binary, Monoid, Semigroup)

-- | converts a lazy RTree into a strict RTree
-- /O(n)/
toStrict :: Lazy.RTree a -> RTree a
toStrict t = map id (RTree t)

-- | converts a strict RTree into a lazy RTree
-- /O(1)/
toLazy :: RTree a -> Lazy.RTree a
toLazy = toLazy'
-- ---------------
-- smart constuctors

-- | creates an empty tree
empty :: RTree a
empty = RTree Lazy.Empty

-- | returns 'True', if empty
--
-- prop> null empty = True
null :: RTree a -> Bool
null = Lazy.null . toLazy

-- | creates a single element tree
singleton :: MBB -> a -> RTree a
singleton mbb !x = RTree $ Lazy.Leaf mbb x

-- ----------------------------------
-- Lists

-- | creates a tree out of pairs
fromList :: [(MBB, a)] -> RTree a
fromList l = RTree $ fromList' $ (toLazy . (uncurry singleton)) <$> l

-- | merges all singletons into a single tree.
fromList' :: [Lazy.RTree a] -> Lazy.RTree a
fromList' [] = Lazy.empty
fromList' ts = foldr1 unionDistinct ts

-- | creates a list of pairs out of a tree
--
-- prop> toList t = zip (keys t) (values t)
toList :: RTree a -> [(MBB, a)]
toList = Lazy.toList . toLazy

-- | returns all keys in this tree
--
-- prop> toList t = zip (keys t) (values t)
keys :: RTree a -> [MBB]
keys = Lazy.keys . toLazy
-- | returns all values in this tree
--
-- prop> toList t = zip (keys t) (values t)
values :: RTree a -> [a]
values = Lazy.values . toLazy


-- ----------------------------------
-- insert

-- | Inserts an element whith the given 'MBB' and a value in a tree. The combining function will be used if the value already exists.
insertWith :: (a -> a -> a) -> MBB -> a -> RTree a -> RTree a
insertWith f mbb e oldRoot = RTree $ insertWithStrictLazy f mbb e (toLazy oldRoot)

insertWithStrictLazy :: (a -> a -> a) -> MBB -> a -> Lazy.RTree a -> Lazy.RTree a
insertWithStrictLazy f mbb e oldRoot = unionDistinctWith f (toLazy $ singleton mbb e) oldRoot
-- | Inserts an element whith the given 'MBB' and a value in a tree. An existing value will be overwritten with the given one.
--
-- prop> insert = insertWith const
insert :: MBB -> a -> RTree a -> RTree a
insert = insertWith const

simpleMergeEqNode :: (a -> a -> a) -> Lazy.RTree a -> Lazy.RTree a -> Lazy.RTree a
simpleMergeEqNode f l@Lazy.Leaf{} r = Lazy.Leaf (Lazy.getMBB l) $! (on f Lazy.getElem l r)
simpleMergeEqNode _ l _ = l

-- | Unifies left and right 'RTree'. Will create invalid trees, if the tree is not a leaf and contains 'MBB's which
--  also exists in the left tree. Much faster than union, though.
unionDistinctWith :: (a -> a -> a) -> Lazy.RTree a -> Lazy.RTree a -> Lazy.RTree a
unionDistinctWith _ Lazy.Empty{}   t                = t
unionDistinctWith _ t              Lazy.Empty{}     = t
unionDistinctWith f t1@Lazy.Leaf{} t2@Lazy.Leaf{}
    | on (==) Lazy.getMBB t1 t2       = simpleMergeEqNode f t1 t2
    | otherwise                       = Lazy.createNodeWithChildren [t1, t2] -- root case
unionDistinctWith f left right
    | Lazy.depth left > Lazy.depth right              = unionDistinctWith f right left
    | Lazy.depth left == Lazy.depth right             = fromList' $ (Lazy.getChildren left) ++ [right]
    | (L.length $ Lazy.getChildren newNode) > Lazy.n  = Lazy.createNodeWithChildren $ Lazy.splitNode newNode
    | otherwise                                       = newNode
    where
    newNode = addLeaf f left right

-- | Unifies left and right 'RTree'. Will create invalid trees, if the tree is not a leaf and contains 'MBB's which
--  also exists in the left tree. Much faster than union, though.
unionDistinct :: Lazy.RTree a -> Lazy.RTree a -> Lazy.RTree a
unionDistinct = unionDistinctWith const

addLeaf :: (a -> a -> a) -> Lazy.RTree a -> Lazy.RTree a -> Lazy.RTree a
addLeaf f left right
    | Lazy.depth left + 1 == Lazy.depth right = Lazy.node (newNode `Lazy.unionMBB'` right) (newNode : nonEq)
    | otherwise                               = Lazy.node (left `Lazy.unionMBB'` right) newChildren
    where
    newChildren = findNodeWithMinimalAreaIncrease f left (Lazy.getChildren right)
    (eq, nonEq) = Lazy.partition (on (==) Lazy.getMBB left) $ Lazy.getChildren right
    newNode = case eq of
        [] -> left
        [x] -> simpleMergeEqNode f left x
        _ -> error "addLeaf: invalid RTree"

findNodeWithMinimalAreaIncrease :: (a -> a -> a) -> Lazy.RTree a -> [Lazy.RTree a] -> [Lazy.RTree a]
findNodeWithMinimalAreaIncrease f leaf children = splitMinimal xsAndIncrease
    where
--  xsAndIncrease :: [(RTree a, Double)]
    xsAndIncrease = zip children ((Lazy.areaIncreasesWith leaf) <$> children)
    minimalIncrease = minimum $ snd <$> xsAndIncrease
--  xsAndIncrease' :: [(RTree a, Double)]
    splitMinimal [] = []
    splitMinimal ((t,mbb):xs)
        | mbb == minimalIncrease = unionDistinctSplit f leaf t ++ (fst <$> xs)
        | otherwise              = t : splitMinimal xs

unionDistinctSplit :: (a -> a -> a) -> Lazy.RTree a -> Lazy.RTree a -> [Lazy.RTree a]
unionDistinctSplit f leaf e
    | (L.length $ Lazy.getChildren newLeaf) > Lazy.n = Lazy.splitNode newLeaf
    | otherwise                                      = [newLeaf]
    where
    newLeaf = addLeaf f leaf e

-- -----------------
-- lookup

-- | returns the value if it exists in the tree
lookup :: MBB -> RTree a -> Maybe a
lookup mbb = Lazy.lookup mbb . toLazy

-- | returns all keys and values, which intersect with the given bounding box.
intersectWithKey :: MBB -> RTree a -> [(MBB, a)]
intersectWithKey mbb = Lazy.intersectWithKey mbb . toLazy

-- | returns all values, which intersect with the given bounding box
intersect :: MBB -> RTree a -> [a]
intersect mbb = Lazy.intersect mbb . toLazy

-- | returns all keys and values, which are located in the given bounding box.
lookupRangeWithKey :: MBB -> RTree a -> [(MBB, a)]
lookupRangeWithKey mbb = Lazy.lookupRangeWithKey mbb . toLazy

-- | returns all values, which are located in the given bounding box.
lookupRange :: MBB -> RTree a -> [a]
lookupRange mbb = Lazy.lookupRange mbb . toLazy

-- | returns all keys and values containing the given bounding box
lookupContainsRangeWithKey :: MBB -> RTree a -> [(MBB, a)]
lookupContainsRangeWithKey mbb = Lazy.lookupContainsRangeWithKey mbb . toLazy

-- | returns all values containing the given bounding box
lookupContainsRange :: MBB -> RTree a -> [a]
lookupContainsRange mbb = Lazy.lookupContainsRange mbb .toLazy

-- -----------
-- delete

-- | Delete a key and its value from the RTree. When the key is not a member of the tree, the original tree is returned.
delete :: MBB -> RTree a -> RTree a
delete mbb = RTree . Lazy.delete mbb . toLazy
-- ---------------

-- | Unifies the first and the second tree into one. The combining function is used for elemets which exists in both trees.
unionWith :: (a -> a -> a) -> RTree a -> RTree a -> RTree a
unionWith f' l' r' = RTree $ unionWith' f' (toLazy l') (toLazy r')
    where
    unionWith' _ l          Lazy.Empty   = l
    unionWith' _ Lazy.Empty r            = r
    unionWith' f t1 t2
        | Lazy.depth t1 <= Lazy.depth t2 = foldr (uncurry (insertWithStrictLazy f)) t2 (Lazy.toList t1)
        | otherwise            = unionWith' f t2 t1

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





-- | maps strictly over the 'RTree'
map :: (a -> b) -> RTree a -> RTree b
map f' = RTree . map' f' . toLazy
    where
    map' f (Lazy.Node4 mbb x y z w) = Lazy.Node4 mbb (map' f x) (map' f y) (map' f z) (map' f w)
    map' f (Lazy.Node3 mbb x y z)   = Lazy.Node3 mbb (map' f x) (map' f y) (map' f z)
    map' f (Lazy.Node2 mbb x y)     = Lazy.Node2 mbb (map' f x) (map' f y)
    map' f (Lazy.Node mbb xs)       = Lazy.Node mbb (map' f <$> xs)
    map' f (Lazy.Leaf mbb e)        = toLazy $ singleton mbb (f e)
    map' _ Lazy.Empty = Lazy.Empty
-- ----------------------

-- | returns the number of elements in a tree
length :: RTree a -> Int
length = Lazy.length . toLazy

-- | 'RTree' is not really a Functor.
-- Because this law doesn't hold:
--
-- prop> fmap id = id
instance Functor RTree where
    fmap = map
