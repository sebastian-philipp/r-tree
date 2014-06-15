{-# LANGUAGE BangPatterns               #-}
{- |
    Module     : Data.RTree.Strict
    Copyright  : Copyright (c) 2014, Birte Wagner, Sebastian Philipp
    License    : MIT

    Maintainer : Birte Wagner, Sebastian Philipp (sebastian@spawnhost.de)
    Stability  : experimental
    Portability: not portable

    This is the Strict version of 'Data.RTree'

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
    , RTree
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
    , length
    , null
    , keys
    , values
    -- * Lists
    , fromList
    , toList
) where

import           Prelude hiding (lookup, length, null)
import           Data.Function (on)
import qualified Data.List as L (length)
import qualified Data.Maybe as Maybe (mapMaybe)

import           Control.Applicative ((<$>))
import           Data.RTree.Base hiding (singleton, fromList, insertWith, unionDistinctWith, unionWith, insert, mapMaybe, union, fromList', unionDistinct, unionDistinctSplit)
import           Data.RTree.MBB hiding (mbb)
import qualified  Data.RTree.MBB as MBB

-- ---------------
-- smart constuctors

-- | creates a single element tree
singleton :: MBB -> a -> RTree a
singleton mbb !x = Leaf mbb x

-- ----------------------------------
-- Lists

-- | creates a tree out of pairs
fromList :: [(MBB, a)] -> RTree a
fromList l = fromList' $ (uncurry singleton) <$> l

-- | merges all singletons into a single tree.
fromList' :: [RTree a] -> RTree a
fromList' [] = empty
fromList' ts = foldr1 unionDistinct ts
-- ----------------------------------
-- insert 

-- | Inserts an element whith the given 'MBB' and a value in a tree. The combining function will be used if the value already exists.
insertWith :: (a -> a -> a) -> MBB -> a -> RTree a -> RTree a
insertWith f mbb !e oldRoot = unionDistinctWith f (singleton mbb e) oldRoot

-- | Inserts an element whith the given 'MBB' and a value in a tree. An existing value will be overwritten with the given one.
--
-- prop> insert = insertWith const
insert :: MBB -> a -> RTree a -> RTree a
insert = insertWith const

simpleMergeEqNode :: (a -> a -> a) -> RTree a -> RTree a -> RTree a
simpleMergeEqNode f l@Leaf{} r = Leaf (getMBB l) $! (on f getElem l r)
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

-- | Únifies left and right 'RTree'. Will create invalid trees, if the tree is not a leaf and contains 'MBB'"'s which
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
