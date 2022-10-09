{-# LANGUAGE CPP
           , MagicHash
           , UnboxedTuples #-}

{-# OPTIONS_HADDOCK not-home #-}

{- |
     Module     : Data.RTree.Internal
     Copyright  : Copyright (c) 2015, Birte Wagner, Sebastian Philipp
                  Copyright (c) 2022, Oleksii Divak
     License    : MIT

     Maintainer : Oleksii Divak
     Stability  : experimental
     Portability: not portable

     Internal implementation. Depend on it at your own risk.
-}

module Data.RTree.Internal
  ( RTree (..)
  , Node (..)
    -- * Construction
  , empty
  , singleton
    -- * List
  , toList
  , boxes
  , elems
    -- * Comparisons
  , Predicate (..)
  , onNode
  , onElement
  , wildcard
  , equals
  , Data.RTree.Internal.intersects
  , Data.RTree.Internal.intersects'
  , Data.RTree.Internal.contains
  , Data.RTree.Internal.contains'
  , within
  , within'
    -- * Size
  , null
  , length
  , depth
  , equideep
    -- * Traversal
    -- ** Map
  , Data.RTree.Internal.map
  , mapWithKey
  , Data.RTree.Internal.traverse
  , traverseWithKey
    -- * Folds
  , foldMap
  , foldMapWithKey
  , foldr
  , foldrWithKey
  , foldl
  , foldlWithKey
    -- ** Strict
#if __GLASGOW_HASKELL__ >= 808
  , foldMap'
  , foldMapWithKey'
#endif
  , foldr'
  , foldrWithKey'
  , foldl'
  , foldlWithKey'
    -- ** Splitting
  , split
  , Split
  , quad
  , sorted
    -- ** Choosing
  , leastEnlargement
    -- * MBR
  , MBR (..)
  , ordX
  , ordY
  , highestDistance
  , Data.RTree.Internal.union
    -- * Array
  , part
  , discard
  , swap
  ) where

import           Data.RTree.MBR
import qualified Data.RTree.MBR as MBR
import           Data.RTree.Internal.Constants

import           Control.DeepSeq
import           Control.Monad
import qualified Data.Foldable as Fold
import           Data.Function
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Prelude hiding (Foldable (..), length, lookup, null)



data RTree r a = Root  !(MBR r) !(Node r a)
               | Leaf1 !(MBR r) a
               | Empty

instance (Show r, Show a) => Show (RTree r a) where
  show = showString "fromList " . flip showList "" . Data.RTree.Internal.toList

instance Functor (RTree r) where
  fmap f (Root  ba a) = Root ba $ fmap f a
  fmap f (Leaf1 ba a) = Leaf1 ba $ f a
  fmap _ Empty        = Empty

instance Fold.Foldable (RTree r) where
  foldMap f (Root  _ba a) = Fold.foldMap f a
  foldMap f (Leaf1 _ba a) = f a
  foldMap _ Empty         = mempty
#if __GLASGOW_HASKELL__ >= 808
  foldMap' f (Root  _ba a) = Fold.foldMap' f a
  foldMap' f (Leaf1 _ba a) = f a
  foldMap' _ Empty         = mempty
#endif
  foldr f z (Root  _ba a) = Fold.foldr f z a
  foldr f z (Leaf1 _ba a) = f a z
  foldr _ z Empty         = z

  foldr' f z (Root  _ba a) = Fold.foldr' f z a
  foldr' f z (Leaf1 _ba a) = f a z
  foldr' _ z Empty         = z

  foldl f z (Root  _ba a) = Fold.foldl f z a
  foldl f z (Leaf1 _ba a) = f z a
  foldl _ z Empty         = z

  foldl' f z (Root  _ba a) = Fold.foldl' f z a
  foldl' f z (Leaf1 _ba a) = f z a
  foldl' _ z Empty         = z

instance Traversable (RTree r) where
  traverse f (Root  ba a) = Root ba <$> Prelude.traverse f a
  traverse f (Leaf1 ba a) = Leaf1 ba <$> f a
  traverse _ Empty        = pure Empty

instance (NFData r, NFData a) => NFData (RTree r a) where
  rnf (Root bn n)  = rnf bn `seq` rnf n
  rnf (Leaf1 ba a) = rnf ba `seq` rnf a
  rnf Empty        = ()

instance NFData r => NFData1 (RTree r) where
  liftRnf f (Root bn n)  = rnf bn `seq` liftRnf f n
  liftRnf f (Leaf1 ba a) = rnf ba `seq`         f a
  liftRnf _ Empty        = ()

instance NFData2 RTree where
  liftRnf2 f g (Root bn n)  = liftRnf f bn `seq` liftRnf2 f g n
  liftRnf2 f g (Leaf1 ba a) = liftRnf f ba `seq`            g a
  liftRnf2 _ _ Empty        = ()




data Node r a = Node {-# UNPACK #-} !(NonEmpty (MBR r, Node r a))
              | Leaf {-# UNPACK #-} !(NonEmpty (MBR r, a))

instance Functor (Node r) where
  fmap f (Node as) = Node $ fmap (fmap f) <$> as
  fmap f (Leaf as) = Leaf $ fmap       f  <$> as

instance Fold.Foldable (Node r) where
  foldMap f (Node as) = Fold.foldMap (Fold.foldMap f . snd) as
  foldMap f (Leaf as) = Fold.foldMap              (f . snd) as
#if __GLASGOW_HASKELL__ >= 808
  foldMap' f (Node as) = Fold.foldMap' (Fold.foldMap' f . snd) as
  foldMap' f (Leaf as) = Fold.foldMap'               (f . snd) as
#endif
  foldr f z (Node as) = Fold.foldr (flip (Fold.foldr f) . snd) z as
  foldr f z (Leaf as) = Fold.foldr                  (f  . snd) z as

  foldr' f z (Node as) = Fold.foldr' (flip (Fold.foldr' f) . snd) z as
  foldr' f z (Leaf as) = Fold.foldr'                   (f  . snd) z as

  foldl f z (Node as) = Fold.foldl (\acc -> Fold.foldl f acc . snd) z as
  foldl f z (Leaf as) = Fold.foldl            (\acc -> f acc . snd) z as

  foldl' f z (Node as) = Fold.foldl' (\acc -> Fold.foldl' f acc . snd) z as
  foldl' f z (Leaf as) = Fold.foldl'             (\acc -> f acc . snd) z as

instance Traversable (Node r) where
  traverse f (Node as) = Node <$> Prelude.traverse (Prelude.traverse $ Prelude.traverse f) as
  traverse f (Leaf as) = Leaf <$> Prelude.traverse                    (Prelude.traverse f) as

instance (NFData r, NFData a) => NFData (Node r a) where
  rnf (Node as) = Fold.foldMap (\(ba, a) -> rnf ba `seq` rnf a) as
  rnf (Leaf as) = Fold.foldMap (\(ba, a) -> rnf ba `seq` rnf a) as

instance NFData r => NFData1 (Node r) where
  liftRnf f (Node as) = Fold.foldMap (\(ba, a) -> rnf ba `seq` liftRnf f a) as
  liftRnf f (Leaf as) = Fold.foldMap (\(ba, a) -> rnf ba `seq`         f a) as

instance NFData2 Node where
  liftRnf2 f g (Node as) = Fold.foldMap (\(ba, a) -> liftRnf f ba `seq` liftRnf2 f g a) as
  liftRnf2 f g (Leaf as) = Fold.foldMap (\(ba, a) -> liftRnf f ba `seq`            g a) as



{-# INLINEABLE empty #-}
-- | \(O (1)\). Empty tree.
empty :: RTree r a
empty = Empty

{-# INLINEABLE singleton #-}
-- | \(O (1)\). Tree with a single entry.
singleton :: MBR r -> a -> RTree r a
singleton = Leaf1



-- | \(O (n)\). Convert the tree to a list of bounding rectangle/value pairs.
toList :: RTree r a -> [(MBR r, a)]
toList (Root _ n)   = collect n
  where
    collect (Node as)        = Fold.foldMap (collect . snd) as
    collect (Leaf (a :| as)) = a : as

toList (Leaf1 br a) = [(br, a)]
toList Empty        = []



-- | \(O (n)\). Convert the tree to a list of values.
elems :: RTree r a -> [a]
elems (Root _ n)  = collect n
  where
    collect (Node as)        = Fold.foldMap (collect . snd) as
    collect (Leaf (a :| as)) = snd <$> (a : as)

elems (Leaf1 _ a) = [a]
elems Empty       = []



-- | \(O (n)\). Convert the tree to a list of bounding rectangles.
boxes :: RTree r a -> [MBR r]
boxes (Root _ n)   = collect n
  where
    collect (Node as)        = Fold.foldMap (collect . snd) as
    collect (Leaf (a :| as)) = fst <$> (a : as)

boxes (Leaf1 br _) = [br]
boxes Empty        = []



-- | Comparisons between parts of the R-Tree.
data Predicate r = -- | First predicate matches nodes, second one is for elements.
                   Predicate (MBR r -> Bool) (MBR r -> Bool)

{-# INLINE onNode #-}
onNode :: Predicate r -> MBR r -> Bool
onNode (Predicate o _) = o

{-# INLINE onElement #-}
onElement :: Predicate r -> MBR r -> Bool
onElement (Predicate _ e) = e



-- | Always succeeds.
wildcard :: Ord r => Predicate r
wildcard = Predicate (const True) (const True)

{-# INLINE equals #-}
-- | Matches specifically the provided 'MBR'.
equals :: Ord r => MBR r -> Predicate r
equals = Predicate <$> MBR.contains <*> (==)

{-# INLINE intersects #-}
-- | Matches any 'MBR' that intersects the provided one.
intersects :: Ord r => MBR r -> Predicate r
intersects = Predicate <$> MBR.intersects <*> MBR.intersects

{-# INLINE intersects' #-}
-- | Same as 'Data.RTree.Internal.intersects', but the checks are strict
--   (rectangles that only intersect on a side or a point are excluded).
intersects' :: Ord r => MBR r -> Predicate r
intersects' = Predicate <$> MBR.intersects' <*> MBR.intersects'

{-# INLINE contains #-}
-- | Matches any 'MBR' that contains the provided one.
contains :: Ord r => MBR r -> Predicate r
contains = Predicate <$> MBR.contains <*> MBR.contains

{-# INLINE contains' #-}
-- | Same as 'Data.RTree.Internal.contains', but the checks are strict
--   (rectangles that touch any sides of the given one are excluded).
contains' :: Ord r => MBR r -> Predicate r
contains' = Predicate <$> MBR.contains' <*> MBR.contains'

{-# INLINE within #-}
-- | Matches any 'MBR' that is fully contained within the provided one.
within :: Ord r => MBR r -> Predicate r
within = Predicate <$> MBR.intersects <*> flip MBR.contains

{-# INLINE within' #-}
-- | Same as 'Data.RTree.Internal.within', but the checks are strict
--   (rectangles that touch any sides of the given one are excluded).
within' :: Ord r => MBR r -> Predicate r
within' = Predicate <$> MBR.intersects <*> flip MBR.contains'



{-# INLINEABLE map #-}
-- | Map a function over values whose bounding rectangles match a 'Predicate'.
map
  :: Ord r
  => Predicate r
  -> (a -> a)
  -> RTree r a
  -> RTree r a
map pre = mapWithKey pre . const

{-# INLINEABLE mapWithKey #-}
-- | Version of 'Data.RTree.Internal.map' that includes the bounding rectangle.
mapWithKey
  :: Ord r
  => Predicate r
  -> (MBR r -> a -> a)
  -> RTree r a
  -> RTree r a
mapWithKey pre f r =
  case r of
    Root ba a  -> Root ba $ go ba a
    Leaf1 ba a | onElement pre ba -> Leaf1 ba $ f ba a
               | otherwise        -> r
    Empty      -> Empty
  where
    go bx x
      | onNode pre bx =
          case x of
            Node as -> Node $ (\(ba, a) -> (ba, go ba a)) <$> as

            Leaf as -> let g (ba, a) | onElement pre ba = (ba, f ba a)
                                     | otherwise        = (ba, a)
                       in Leaf $ g <$> as

      | otherwise     = x



{-# INLINEABLE foldMap #-}
-- | Fold the tree over values whose bounding rectangles match a 'Predicate'
--   using the given 'Monoid'.
foldMap
  :: (Monoid m, Ord r)
  => Predicate r
  -> (a -> m)
  -> RTree r a
  -> m
foldMap pre = foldMapWithKey pre . const

{-# INLINEABLE foldMapWithKey #-}
-- | Version of 'Data.RTree.Internal.foldMap' that
--   includes the bounding rectangle in the operator.
foldMapWithKey
  :: (Monoid m, Ord r)
  => Predicate r
  -> (MBR r -> a -> m)
  -> RTree r a
  -> m
foldMapWithKey pre f r =
  case r of
    Root  ba a -> go (ba, a)
    Leaf1 ba a | onElement pre ba -> f ba a
               | otherwise        -> mempty
    Empty      -> mempty
  where
    go (bx, x)
      | onNode pre bx =
          case x of
            Node as -> Fold.foldMap go as
            Leaf as -> let g (ba, a) | onElement pre ba = f ba a
                                     | otherwise        = mempty
                       in Fold.foldMap g as

      | otherwise            = mempty

#if __GLASGOW_HASKELL__ >= 808

{-# INLINEABLE foldMap' #-}
-- | Version of 'Data.RTree.Internal.foldMap' that is strict in the accumulator.
foldMap'
  :: (Monoid m, Ord r)
  => Predicate r
  -> (a -> m)
  -> RTree r a
  -> m
foldMap' pre = foldMapWithKey' pre . const

{-# INLINEABLE foldMapWithKey' #-}
-- | Version of 'foldMapWithKey' that is strict in the accumulator.
foldMapWithKey'
  :: (Monoid m, Ord r)
  => Predicate r
  -> (MBR r -> a -> m)
  -> RTree r a
  -> m
foldMapWithKey' pre f r =
  case r of
    Root  ba a -> go (ba, a)
    Leaf1 ba a | onElement pre ba -> f ba a
               | otherwise        -> mempty
    Empty      -> mempty
  where
    go (bx, x)
      | onNode pre bx =
          case x of
            Node as -> Fold.foldMap' go as
            Leaf as -> let g (ba, a) | onElement pre ba = f ba a
                                     | otherwise        = mempty
                       in Fold.foldMap' g as

      | otherwise     = mempty

#endif

{-# INLINEABLE foldr #-}
-- | Fold the tree right-to-left over values whose bounding rectangles match a 'Predicate'.
foldr
  :: Ord r
  => Predicate r
  -> (a -> b -> b)
  -> b
  -> RTree r a
  -> b
foldr pre = foldrWithKey pre . const

{-# INLINEABLE foldrWithKey #-}
-- | Version of 'Data.RTree.Internal.foldr' that
--   includes the bounding rectangle in the operator.
foldrWithKey
  :: Ord r
  => Predicate r
  -> (MBR r -> a -> b -> b)
  -> b
  -> RTree r a
  -> b
foldrWithKey pre f z r =
  case r of
    Root  ba a -> go (ba, a) z
    Leaf1 ba a | onElement pre ba -> f ba a z
               | otherwise        -> z
    Empty      -> z
  where
    go (bx, x) t
      | onNode pre bx =
          case x of
            Node as -> Fold.foldr go t as
            Leaf as -> let g (ba, a) acc | onElement pre ba = f ba a acc
                                         | otherwise        = acc
                       in Fold.foldr g t as
      | otherwise     = t



{-# INLINEABLE foldr' #-}
-- | Version of 'Data.RTree.Internal.foldr' that is
--   strict in the application of the operator.
foldr'
  :: Ord r
  => Predicate r
  -> (a -> b -> b)
  -> b
  -> RTree r a
  -> b
foldr' pre = foldrWithKey' pre . const

{-# INLINEABLE foldrWithKey' #-}
-- | Version of 'foldrWithKey' that is strict in the application of the operator.
foldrWithKey'
  :: Ord r
  => Predicate r
  -> (MBR r -> a -> b -> b)
  -> b
  -> RTree r a
  -> b
foldrWithKey' pre f z r =
  case r of
    Root  ba a -> go (ba, a) z
    Leaf1 ba a | onElement pre ba -> f ba a z
               | otherwise        -> z
    Empty      -> z
  where
    go (bx, x) t
      | onNode pre bx =
          case x of
            Node as -> Fold.foldr' go t as
            Leaf as -> let g (ba, a) acc | onElement pre ba = f ba a acc
                                         | otherwise        = acc
                       in Fold.foldr' g t as
      | otherwise     = t



{-# INLINEABLE foldl #-}
-- | Fold the tree left-to-right over values whose bounding rectangles match a 'Predicate'.
foldl
  :: Ord r
  => Predicate r
  -> (b -> a -> b)
  -> b
  -> RTree r a
  -> b
foldl pre = foldlWithKey pre . (.) const

{-# INLINEABLE foldlWithKey #-}
-- | Version of 'Data.RTree.Internal.foldl' that
--   includes the bounding rectangle in the operator.
foldlWithKey
  :: Ord r
  => Predicate r
  -> (b -> MBR r -> a -> b)
  -> b
  -> RTree r a
  -> b
foldlWithKey pre f z r =
  case r of
    Root  ba a -> go z (ba, a)
    Leaf1 ba a | onElement pre ba -> f z ba a
               | otherwise        -> z
    Empty      -> z
  where
    go t (bx, x)
      | onNode pre bx =
          case x of
            Node as -> Fold.foldl go t as
            Leaf as -> let g acc (ba, a) | onElement pre ba = f acc ba a
                                         | otherwise        = acc
                       in Fold.foldl g t as
      | otherwise      = t



{-# INLINEABLE foldl' #-}
-- | Version of 'Data.RTree.Internal.foldl' that is
--   strict in the application of the operator.
foldl'
  :: Ord r
  => Predicate r
  -> (b -> a -> b)
  -> b
  -> RTree r a
  -> b
foldl' pre = foldlWithKey' pre . (.) const

{-# INLINEABLE foldlWithKey' #-}
-- | Version of 'foldlWithKey' that is strict in the application of the operator.
foldlWithKey'
  :: Ord r
  => Predicate r
  -> (b -> MBR r -> a -> b)
  -> b
  -> RTree r a
  -> b
foldlWithKey' pre f z r =
  case r of
    Root  ba a -> go z (ba, a)
    Leaf1 ba a | onElement pre ba -> f z ba a
               | otherwise        -> z
    Empty      -> z
  where
    go t (bx, x)
      | onNode pre bx =
          case x of
            Node as -> Fold.foldl' go t as
            Leaf as -> let g acc (ba, a) | onElement pre ba = f acc ba a
                                         | otherwise        = acc
                       in Fold.foldl' g t as
      | otherwise      = t



{-# INLINEABLE traverse #-}
-- | Map each value whose bounding rectangle matches a 'Predicate' to an action,
--   evaluate these actions from left to right, and replace the results.
traverse
  :: (Applicative f, Ord r)
  => Predicate r
  -> (a -> f a)
  -> RTree r a
  -> f (RTree r a)
traverse pre = traverseWithKey pre . const

{-# INLINEABLE traverseWithKey #-}
-- | Version of 'Data.RTree.Internal.traverse' that
--   includes the bounding rectangle in the operator.
traverseWithKey
  :: (Applicative f, Ord r)
  => Predicate r
  -> (MBR r -> a -> f a)
  -> RTree r a
  -> f (RTree r a)
traverseWithKey pre f r =
  case r of
    Root  ba a -> Root ba <$> go ba a
    Leaf1 ba a | onElement pre ba -> Leaf1 ba <$> f ba a
               | otherwise        -> pure r
    Empty      -> pure Empty
  where
    go bx x
      | onNode pre bx =
          case x of
            Node as ->
              Node <$> Prelude.traverse (\(ba, a) -> (,) ba <$> go ba a) as

            Leaf as ->
              let g (ba, a) | onElement pre ba = (,) ba <$> f ba a
                            | otherwise        = pure (ba, a)

              in Leaf <$> Prelude.traverse g as

      | otherwise      = pure x



{-# INLINEABLE null #-}
-- | \(O (1)\). Check whether the tree is empty.
null :: RTree r a -> Bool
null Empty = True
null _     = False



-- | \(O (n)\). Total number of entries within the tree.
length :: RTree r a -> Int
length r =
  case r of
    Root _ n  -> getSum $ measure n
    Leaf1 _ _ -> 1
    Empty     -> 0
  where
    measure (Node as) = Fold.foldMap (measure . snd) as
    measure (Leaf as) = Sum $ Fold.length as


-- | \(O (\log_M n)\). Height of the tree.
depth :: RTree r a -> Int
depth r =
  case r of
    Root _ n  -> measure n
    Leaf1 _ _ -> 1
    Empty     -> 0
  where
    measure (Node as) = 1 + measure (snd $ NonEmpty.head as)
    measure (Leaf _ ) = 1



-- | Calculates the depth fully, traversing every leaf.
--   Only returns said depth if all leaves are on it.
equideep :: RTree r a -> Maybe Int
equideep r =
  case r of
    Root _ n  -> measure n
    Leaf1 _ _ -> Just 1
    Empty     -> Just 0
  where
    same n i | Just _ <- i, measure n == i = i
             | otherwise                   = Nothing

    measure (Node (a :| as)) = Fold.foldr (same . snd) (measure $ snd a) as
    measure (Leaf _)         = Just 1



{-# INLINEABLE leastEnlargement #-}
leastEnlargement :: (Num r, Ord r) => MBR r -> NonEmpty (MBR r, a) -> (Int, a)
leastEnlargement bx brs = (\(i, _, a) -> (i, a)) $ Fold.foldr move z zs
  where
    z :| zs = NonEmpty.zipWith (\i (a, b) -> (i, a, b)) (0 :| [1..]) brs

    move x@(_, a, _) y@(_, b, _) = case smaller a b of
                                     GT -> y
                                     _  -> x
    smaller ba bb =
      case compare (MBR.area (MBR.union ba bx) - MBR.area ba)
                   (MBR.area (MBR.union bb bx) - MBR.area bb) of
        EQ -> MBR.area ba `compare` MBR.area bb
        eq -> eq



mostWasteful :: (Num r, Ord r) => MBR r -> MBR r -> r
mostWasteful ba bb = MBR.area (MBR.union ba bb) - MBR.area ba - MBR.area bb



weave :: [a] -> [(a, a, [a])]
weave (x:xs) = (($ x) <$> sub xs) <> ((\(a, b, zs) -> (a, b, x:zs)) <$> weave xs)
  where
    sub :: [a] -> [a -> (a, a, [a])]
    sub (y:ys) = (\a -> (a, y, ys)) : ((\f -> (\(a, b, zs) -> (a, b, y:zs)) . f) <$> sub ys)
    sub    []  = []

weave    []  = []



compareEnlargement
  :: (Num r, Ord r) => MBR r -> Int -> MBR r -> Int -> MBR r -> Ordering
compareEnlargement bx n ba m bb =
  case compare (MBR.area (MBR.union ba bx) - MBR.area ba)
               (MBR.area (MBR.union bb bx) - MBR.area bb) of
    EQ -> case MBR.area ba `compare` MBR.area bb of
            EQ -> n `compare` m
            eq -> eq
    eq -> eq



type Split r a = [(MBR r, a)] -> ([(MBR r, a)], [(MBR r, a)])



{-# INLINEABLE split #-}
split :: (NonEmpty (MBR r, a) -> b) -> Split r a -> NonEmpty (MBR r, a) -> (b, b)
split f s (x :| xs) = let (ls, rs) = s (x:xs)
                      in (f $ NonEmpty.fromList ls, f $ NonEmpty.fromList rs)



{-# INLINEABLE mbr #-}
mbr :: (Fold.Foldable t, Functor t, Ord r) => t (MBR r, a) -> MBR r
mbr as = Fold.foldr1 MBR.union $ fst <$> as

{-# INLINEABLE quad #-}
quad :: (Num r, Ord r) => Split r a
quad xs =
  let (a, b, cs) = List.maximumBy (compare `on` (\((x, _), (y, _), _) -> mostWasteful x y)) $ weave xs
  in quad' [a] [b] cs

quad'
  :: (Num r, Ord r)
  => [(MBR r, a)] -> [(MBR r, a)] -> [(MBR r, a)] -> ([(MBR r, a)], [(MBR r, a)])
quad' left right          []  = (left, right)
quad' left right xss@(x : xs)
  | List.length left  + List.length xss == smallM = (left <> xss, right)
  | List.length right + List.length xss == smallM = (left, right <> xss)
  | otherwise =
      case compareEnlargement (fst x) (List.length left)  (mbr left)
                                      (List.length right) (mbr right) of
        GT -> quad'      left  (x : right) xs
        _  -> quad' (x : left)      right  xs



distributions :: [Int]
distributions = let smallK = [1 .. bigM - 2 * smallM + 2]
                in (+) (smallM - 1) <$> smallK


smallestOverlap
  :: (Num r, Ord r)
  => ([(MBR r, a)], [(MBR r, a)]) -> ([(MBR r, a)], [(MBR r, a)]) -> Ordering
smallestOverlap (a, b) (c, d) =
  case MBR.overlap (mbr a) (mbr b) `compare` MBR.overlap (mbr c) (mbr d) of
    EQ -> (MBR.area (mbr a) + MBR.area (mbr b)) `compare` (MBR.area (mbr c) + MBR.area (mbr d))
    eq -> eq


{-# INLINEABLE sorted #-}
sorted :: (Num r, Ord r) => Split r a
sorted as =
  let hori = flip List.sortBy as $ \(MBR xmin _ xmax _, _) (MBR xmin' _ xmax' _, _) ->
                                      case xmin `compare` xmin' of
                                        EQ -> xmax `compare` xmax'
                                        eq -> eq

      vert = flip List.sortBy as $ \(MBR _ ymin _ ymax, _) (MBR _ ymin' _ ymax', _) ->
                                      case ymin `compare` ymin' of
                                        EQ -> ymax `compare` ymax'
                                        eq -> eq

      margins a b = MBR.margin (mbr a) + MBR.margin (mbr b)

      marginSum xs = List.sum $ uncurry margins . flip splitAt xs <$> distributions

  in if marginSum hori <= marginSum vert
       then List.minimumBy smallestOverlap $ flip splitAt hori <$> distributions
       else List.minimumBy smallestOverlap $ flip splitAt vert <$> distributions



{-# INLINEABLE highestDistance #-}
highestDistance :: (Num r, Ord r) => MBR r -> MBR r -> MBR r -> Ordering
highestDistance = on (comparing Down) . MBR.distance



-- | Slices a list up into parts of given length.
part :: Int -> [a] -> [[a]]
part n xs@(_:_) = take n xs : part n (drop n xs)
part _ []       = []


{-# INLINEABLE ordX #-}
-- | Ordering based on horizontal center points.
ordX :: (Num r, Ord r) => MBR r -> MBR r -> Ordering
ordX (MBR xmin _ xmax _) (MBR xmin' _ xmax' _) = compare (xmin + xmax) (xmin' + xmax')

{-# INLINEABLE ordY #-}
-- | Ordering based on vertical center points.
ordY :: (Num r, Ord r) => MBR r -> MBR r -> Ordering
ordY (MBR _ ymin _ ymax) (MBR _ ymin' _ ymax') = compare (ymin + ymax) (ymin' + ymax')



{-# INLINEABLE discard #-}
-- | Removes an element at an index within a nonempty list.
discard :: Int -> NonEmpty a -> [a]
discard i as = NonEmpty.take i as <> NonEmpty.drop (i + 1) as

{-# INLINEABLE swap #-}
-- | Replaces an element at an index within a nonempty list.
swap :: Int -> a -> NonEmpty a -> [a]
swap i a as = NonEmpty.take i as <> (a : NonEmpty.drop (i + 1) as)



{-# INLINEABLE union #-}
union :: Ord r => Node r a -> MBR r
union (Node (a :| as)) = Fold.foldr MBR.union (fst a) (fst <$> as)
union (Leaf (a :| as)) = Fold.foldr MBR.union (fst a) (fst <$> as)
