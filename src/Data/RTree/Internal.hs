{-# LANGUAGE MagicHash
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
  , nodes
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
  , foldMap'
  , foldMapWithKey'
  , foldr'
  , foldrWithKey'
  , foldl'
  , foldlWithKey'
    -- * Node
  , mk
  , snoc
  , substitute
  , replace
  , replaceSnoc
  , lose
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
  , arrayToList
  , discard
  ) where

import qualified Data.Primitive.Array.Extra as Array
import           Data.RTree.MBR
import qualified Data.RTree.MBR as MBR
import           Data.RTree.MBR.Internal
import           Data.RTree.Internal.Constants

import           Control.DeepSeq
import           Control.Monad
import qualified Data.Foldable as Fold
import           Data.Function
import           Data.Functor.Compose
import qualified Data.List as List
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Primitive.Array
import           Data.Primitive.Types
import           Prelude hiding (Foldable (..), length, lookup, null)



data RTree r a = Root  {-# UNPACK #-} !(MBR r) !(Node r a)
               | Leaf1 {-# UNPACK #-} !(MBR r) a
               | Empty

instance (Prim r, Show r, Show a) => Show (RTree r a) where
  show = showString "fromList " . flip showList "" . Data.RTree.Internal.toList

instance Functor (RTree r) where
  fmap f (Root  ba a) = Root ba $ fmap f a
  fmap f (Leaf1 ba a) = Leaf1 ba $ f a
  fmap _ Empty        = Empty

instance Fold.Foldable (RTree r) where
  foldMap f (Root  _ba a) = Fold.foldMap f a
  foldMap f (Leaf1 _ba a) = f a
  foldMap _ Empty         = mempty

  foldMap' f (Root  _ba a) = Fold.foldMap' f a
  foldMap' f (Leaf1 _ba a) = f a
  foldMap' _ Empty         = mempty

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
  rnf = rnf1

instance NFData r => NFData1 (RTree r) where
  liftRnf f (Root _ n)  = liftRnf f n
  liftRnf f (Leaf1 _ a) = f a
  liftRnf _ Empty       = ()



data Node r a = Node {-# UNPACK #-} !Int {-# UNPACK #-} !(Array (MBR r)) {-# UNPACK #-} !(Array (Node r a))
              | Leaf {-# UNPACK #-} !Int {-# UNPACK #-} !(Array (MBR r)) {-# UNPACK #-} !(Array a)

instance Functor (Node r) where
  fmap f (Node n brs as) = Node n brs $ Array.fmap (fmap f) n as
  fmap f (Leaf n brs as) = Leaf n brs $ Array.fmap       f  n as

instance Fold.Foldable (Node r) where
  foldMap f (Node n _brs as) = Array.foldMap (Fold.foldMap f) n as
  foldMap f (Leaf n _brs as) = Array.foldMap               f  n as

  foldMap' f (Node n _brs as) = Array.foldMap' (Fold.foldMap' f) n as
  foldMap' f (Leaf n _brs as) = Array.foldMap'                f  n as

  foldr f z (Node n _brs as) = Array.foldr (flip $ Fold.foldr f) z n as
  foldr f z (Leaf n _brs as) = Array.foldr                    f  z n as

  foldr' f z (Node n _brs as) = Array.foldr' (flip $ Fold.foldr' f) z n as
  foldr' f z (Leaf n _brs as) = Array.foldr'                     f  z n as

  foldl f z (Node n _brs as) = Array.foldl (Fold.foldl f) z n as
  foldl f z (Leaf n _brs as) = Array.foldl             f  z n as

  foldl' f z (Node n _brs as) = Array.foldl' (Fold.foldl' f) z n as
  foldl' f z (Leaf n _brs as) = Array.foldl'              f  z n as

instance Traversable (Node r) where
  traverse f (Node n brs as) = Node n brs <$> Array.traverse (Prelude.traverse f) n as
  traverse f (Leaf n brs as) = Leaf n brs <$> Array.traverse                   f  n as

instance (NFData r, NFData a) => NFData (Node r a) where
  rnf = rnf1

instance NFData r => NFData1 (Node r) where
  liftRnf f (Node n brs as) = flip Fold.foldMap [0 .. n - 1] $ \i ->
                                     let (# br #) = indexArray## brs i
                                         (# a #)  = indexArray## as i
                                     in rnf br `seq` liftRnf f a
  liftRnf f (Leaf n brs as) = flip Fold.foldMap [0 .. n - 1] $ \i ->
                                     let (# br #) = indexArray## brs i
                                         (# a #)  = indexArray## as i
                                     in rnf br `seq` f a



{-# INLINEABLE empty #-}
-- | \(O (1)\). Empty tree.
empty :: RTree r a
empty = Empty

{-# INLINEABLE singleton #-}
-- | \(O (1)\). Tree with a single entry.
singleton :: MBR r -> a -> RTree r a
singleton = Leaf1



-- | \(O (n)\). Convert the tree to a list of bounding rectangle/value pairs.
toList :: Prim r => RTree r a -> [(MBR r, a)]
toList (Root _ n)   = collect n
  where
    collect (Node m _   as) = Array.foldMap collect m as
    collect (Leaf m brs as) = Array.zipMap ((.) pure . (,)) m brs as

toList (Leaf1 br a) = [(br, a)]
toList Empty        = []



-- | \(O (n)\). Convert the tree to a list of values.
elems :: Prim r => RTree r a -> [a]
elems (Root _ n) = collect n
  where
    collect (Node m _brs as) = Array.foldMap collect m as
    collect (Leaf m _brs as) = Array.foldMap pure    m as

elems (Leaf1 _ a)= [a]
elems (Empty)    = []

-- | \(O (n)\). Convert the tree to a list of bounding rectangles.
boxes :: Prim r => RTree r a -> [MBR r]
boxes (Root _ n)   = collect n
  where
    collect (Node m _brs as) = Array.foldMap collect m as
    collect (Leaf m brs _as) = Array.foldMap pure    m brs

boxes (Leaf1 br _) = [br]
boxes (Empty)      = []



{-# INLINEABLE nodes #-}
nodes :: Prim r => Int -> Array (MBR r) -> Array a -> [(MBR r, a)]
nodes = Array.zipMap $ (.) pure . (,)



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
wildcard :: (Ord r, Prim r) => Predicate r
wildcard = Predicate (const True) (const True)

{-# INLINE equals #-}
-- | Matches specifically the provided 'MBR'.
equals :: (Ord r, Prim r) => MBR r -> Predicate r
equals = Predicate <$> MBR.contains <*> (==)

{-# INLINE intersects #-}
-- | Matches any 'MBR' that intersects the provided one.
intersects :: (Ord r, Prim r) => MBR r -> Predicate r
intersects = Predicate <$> MBR.intersects <*> MBR.intersects

{-# INLINE intersects' #-}
-- | Same as 'intersects', but the checks are strict
--   (rectangles that only intersect on a side or a point are excluded).
intersects' :: (Ord r, Prim r) => MBR r -> Predicate r
intersects' = Predicate <$> MBR.intersects' <*> MBR.intersects'

{-# INLINE contains #-}
-- | Matches any 'MBR' that contains the provided one.
contains :: (Ord r, Prim r) => MBR r -> Predicate r
contains = Predicate <$> MBR.contains <*> MBR.contains

{-# INLINE contains' #-}
-- | Same as 'contains', but the checks are strict
--   (rectangles that touch any sides of the given one are excluded).
contains' :: (Ord r, Prim r) => MBR r -> Predicate r
contains' = Predicate <$> MBR.contains' <*> MBR.contains'

{-# INLINE within #-}
-- | Matches any 'MBR' that is fully contained within the provided one.
within :: (Ord r, Prim r) => MBR r -> Predicate r
within = Predicate <$> MBR.intersects <*> flip MBR.contains

{-# INLINE within' #-}
-- | Same as 'within', but the checks are strict
--   (rectangles that touch any sides of the given one are excluded).
within' :: (Ord r, Prim r) => MBR r -> Predicate r
within' = Predicate <$> MBR.intersects <*> flip MBR.contains'



{-# INLINEABLE map #-}
-- | Map a function over values whose bounding rectangles match a 'Predicate'.
map
  :: (Ord r, Prim r)
  => Predicate r
  -> (a -> a)
  -> RTree r a
  -> RTree r a
map pre = mapWithKey pre . const

{-# INLINEABLE mapWithKey #-}
-- | Version of 'map' that includes the bounding rectangle.
mapWithKey
  :: (Ord r, Prim r)
  => Predicate r
  -> (MBR r -> a -> a)
  -> RTree r a
  -> RTree r a
mapWithKey pre f r =
  case r of
    Root  ba a -> let (Any altered, b) = lookupNode ba a
                  in if altered
                       then Root ba b
                       else r
    Leaf1 ba a | onElement pre ba -> Leaf1 ba $ f ba a
               | otherwise        -> r
    Empty      -> Empty
  where
    lookupNode bx x
      | onNode pre bx =
          (,) (Any True) $
            case x of
              Node n brs as ->
                let (Any altered, bs) = Array.zipA lookupNode n brs as
                in if altered
                     then Node n brs bs
                     else x

              Leaf n brs as ->
                let g ba a | onElement pre ba = (Any True, f ba a)
                           | otherwise        = (Any False, a)

                    (Any altered, bs) = Array.zipA g n brs as

                in if altered
                     then Leaf n brs bs
                     else x

      | otherwise            = (Any False, x)



{-# INLINEABLE foldMap #-}
-- | Fold the tree over values whose bounding rectangles match a 'Predicate'
--   using the given 'Monoid'.
foldMap
  :: (Monoid m, Ord r, Prim r)
  => Predicate r
  -> (a -> m)
  -> RTree r a
  -> m
foldMap pre = foldMapWithKey pre . const

{-# INLINEABLE foldMapWithKey #-}
-- | Version of 'foldMap' that includes the bounding rectangle in the operator.
foldMapWithKey
  :: (Monoid m, Ord r, Prim r)
  => Predicate r
  -> (MBR r -> a -> m)
  -> RTree r a
  -> m
foldMapWithKey pre f r =
  case r of
    Root  ba a -> lookupNode ba a
    Leaf1 ba a | onElement pre ba -> f ba a
               | otherwise        -> mempty
    Empty      -> mempty
  where
    lookupNode bx x
      | onNode pre bx =
          case x of
            Node n brs as -> Array.zipMap lookupNode n brs as
            Leaf n brs as -> let g ba a | onElement pre ba = f ba a
                                        | otherwise        = mempty
                             in Array.zipMap g n brs as

      | otherwise            = mempty

{-# INLINEABLE foldMap' #-}
-- | Version of 'foldMap' that is strict in the accumulator.
foldMap'
  :: (Monoid m, Ord r, Prim r)
  => Predicate r
  -> (a -> m)
  -> RTree r a
  -> m
foldMap' pre = foldMapWithKey' pre . const

{-# INLINEABLE foldMapWithKey' #-}
-- | Version of 'foldMapWithKey' that is strict in the accumulator.
foldMapWithKey'
  :: (Monoid m, Ord r, Prim r)
  => Predicate r
  -> (MBR r -> a -> m)
  -> RTree r a
  -> m
foldMapWithKey' pre f r =
  case r of
    Root  ba a -> lookupNode ba a
    Leaf1 ba a | onElement pre ba -> f ba a
               | otherwise        -> mempty
    Empty      -> mempty
  where
    lookupNode bx x
      | onNode pre bx =
          case x of
            Node n brs as -> Array.zipMap' lookupNode n brs as
            Leaf n brs as -> let g ba a | onElement pre ba = f ba a
                                        | otherwise        = mempty
                             in Array.zipMap' g n brs as

      | otherwise      = mempty



{-# INLINEABLE foldr #-}
-- | Fold the tree right-to-left over values whose bounding rectangles match a 'Predicate'.
foldr
  :: (Ord r, Prim r)
  => Predicate r
  -> (a -> b -> b)
  -> b
  -> RTree r a
  -> b
foldr pre = foldrWithKey pre . const

{-# INLINEABLE foldrWithKey #-}
-- | Version of 'foldr' that includes the bounding rectangle in the operator.
foldrWithKey
  :: (Ord r, Prim r)
  => Predicate r
  -> (MBR r -> a -> b -> b)
  -> b
  -> RTree r a
  -> b
foldrWithKey pre f z r =
  case r of
    Root  ba a -> lookupNode ba a z
    Leaf1 ba a | onElement pre ba -> f ba a z
               | otherwise        -> z
    Empty      -> z
  where
    lookupNode bx x t
      | onNode pre bx =
          case x of
            Node n brs as -> Array.zipr lookupNode t n brs as
            Leaf n brs as -> let g ba a acc | onElement pre ba = f ba a acc
                                            | otherwise        = acc
                             in Array.zipr g t n brs as
      | otherwise      = t



{-# INLINEABLE foldr' #-}
-- | Version of 'foldr' that is strict in the application of the operator.
foldr'
  :: (Ord r, Prim r)
  => Predicate r
  -> (a -> b -> b)
  -> b
  -> RTree r a
  -> b
foldr' pre = foldrWithKey' pre . const

{-# INLINEABLE foldrWithKey' #-}
-- | Version of 'foldrWithKey' that is strict in the application of the operator.
foldrWithKey'
  :: (Ord r, Prim r)
  => Predicate r
  -> (MBR r -> a -> b -> b)
  -> b
  -> RTree r a
  -> b
foldrWithKey' pre f z r =
  case r of
    Root  ba a -> lookupNode ba a z
    Leaf1 ba a | onElement pre ba -> f ba a z
               | otherwise        -> z
    Empty      -> z
  where
    lookupNode bx x t
      | onNode pre bx =
          case x of
            Node n brs as -> Array.zipr' lookupNode t n brs as
            Leaf n brs as -> let g ba a acc | onElement pre ba = f ba a acc
                                            | otherwise        = acc
                             in Array.zipr' g t n brs as
      | otherwise      = t



{-# INLINEABLE foldl #-}
-- | Fold the tree left-to-right over values whose bounding rectangles match a 'Predicate'.
foldl
  :: (Ord r, Prim r)
  => Predicate r
  -> (b -> a -> b)
  -> b
  -> RTree r a
  -> b
foldl pre = foldlWithKey pre . (.) const

{-# INLINEABLE foldlWithKey #-}
-- | Version of 'foldl' that includes the bounding rectangle in the operator.
foldlWithKey
  :: (Ord r, Prim r)
  => Predicate r
  -> (b -> MBR r -> a -> b)
  -> b
  -> RTree r a
  -> b
foldlWithKey pre f z r =
  case r of
    Root  ba a -> lookupNode z ba a
    Leaf1 ba a | onElement pre ba -> f z ba a
               | otherwise        -> z
    Empty      -> z
  where
    lookupNode t bx x
      | onNode pre bx =
          case x of
            Node n brs as -> Array.zipl lookupNode t n brs as
            Leaf n brs as -> let g acc ba a | onElement pre ba = f acc ba a
                                            | otherwise        = acc
                             in Array.zipl g t n brs as
      | otherwise      = t



{-# INLINEABLE foldl' #-}
-- | Version of 'foldl' that is strict in the application of the operator.
foldl'
  :: (Ord r, Prim r)
  => Predicate r
  -> (b -> a -> b)
  -> b
  -> RTree r a
  -> b
foldl' pre = foldlWithKey' pre . (.) const

{-# INLINEABLE foldlWithKey' #-}
-- | Version of 'foldlWithKey' that is strict in the application of the operator.
foldlWithKey'
  :: (Ord r, Prim r)
  => Predicate r
  -> (b -> MBR r -> a -> b)
  -> b
  -> RTree r a
  -> b
foldlWithKey' pre f z r =
  case r of
    Root  ba a -> lookupNode z ba a
    Leaf1 ba a | onElement pre ba -> f z ba a
               | otherwise        -> z
    Empty      -> z
  where
    lookupNode t bx x
      | onNode pre bx =
          case x of
            Node n brs as -> Array.zipl' lookupNode t n brs as
            Leaf n brs as -> let g acc ba a | onElement pre ba = f acc ba a
                                            | otherwise        = acc
                             in Array.zipl' g t n brs as
      | otherwise      = t



{-# INLINEABLE traverse #-}
-- | Map each value whose bounding rectangle matches a 'Predicate' to an action,
--   evaluate these actions from left to right, and replace the results.
traverse
  :: (Applicative f, Ord r, Prim r)
  => Predicate r
  -> (a -> f a)
  -> RTree r a
  -> f (RTree r a)
traverse pre = traverseWithKey pre . const

{-# INLINEABLE traverseWithKey #-}
-- | Version of 'traverse' that includes the bounding rectangle in the operator.
traverseWithKey
  :: (Applicative f, Ord r, Prim r)
  => Predicate r
  -> (MBR r -> a -> f a)
  -> RTree r a
  -> f (RTree r a)
traverseWithKey pre f r =
  case r of
    Root  ba a -> let (Any altered, b) = lookupNode ba a
                  in if altered
                       then Root ba <$> b
                       else pure r

    Leaf1 ba a | onElement pre ba -> Leaf1 ba <$> f ba a
               | otherwise        -> pure r
    Empty      -> pure Empty
  where
    lookupNode bx x
      | onNode pre bx =
          (,) (Any True) $
            case x of
              Node n brs as ->
                let Compose (Any altered, bs) =
                      Array.zipA ((.) Compose . lookupNode) n brs as

                in if altered
                     then Node n brs <$> bs
                     else pure x

              Leaf n brs as ->
                let g ba a | onElement pre ba = Compose (Any True , f ba a)
                           | otherwise        = Compose (Any False, pure a)

                    Compose (Any altered, bs) = Array.zipA g n brs as

                in if altered
                     then Leaf n brs <$> bs
                     else pure x

      | otherwise      = (Any False, pure x)



{-# INLINEABLE null #-}
-- | \(O (1)\). Check whether the tree is empty.
null :: RTree r a -> Bool
null Empty = True
null _     = False



-- | \(O (n)\). Total number of entries within the tree.
length :: Prim r => RTree r a -> Int
length r =
  case r of
    Root _ n  -> getSum $ measure n
    Leaf1 _ _ -> 1
    Empty     -> 0
  where
    measure (Node n _ as) = Array.foldMap measure n as
    measure (Leaf n _ _)  = Sum n


-- | \(O (\log_M n)\). Height of the tree.
depth :: Prim r => RTree r a -> Int
depth r =
  case r of
    Root _ n  -> measure n
    Leaf1 _ _ -> 1
    Empty     -> 0
  where
    measure (Node _ _ as) = 1 + measure (indexArray as 0)
    measure (Leaf _ _ _)  = 1



-- | Calculates the depth fully, traversing every leaf.
--   Only returns said depth if all leaves are on it.
equideep :: Prim r => RTree r a -> Maybe Int
equideep r =
  case r of
    Root _ n  -> measure n
    Leaf1 _ _ -> Just 1
    Empty     -> Just 0
  where
    same n i | Just _ <- i, measure n == i = i
             | otherwise                   = Nothing

    measure (Node n _ as)
      | n <= 0    = errorWithoutStackTrace
                      "Data.RTree.Internal.equideep: node length below one"
      | otherwise = let (# a #) = indexArray## as (n - 1)
                    in Array.foldr same (measure a) (n - 1) as
    measure (Leaf _ _ _)  = Just 1



{-# INLINEABLE snoc #-}
snoc :: (Int -> Array (MBR r) -> Array a -> b) -> Int -> Array (MBR r) -> Array a -> MBR r -> a -> b
snoc f n brs as br a = f (n + 1) (Array.snoc n brs br) (Array.snoc n as a)

{-# INLINEABLE substitute #-}
substitute
  :: (a -> a) -> (Int -> Array (MBR r) -> Array a -> b) -> Int -> Array (MBR r) -> Array a -> Int -> b
substitute g f n brs as i = f n brs $ Array.update n as i g

{-# INLINEABLE replace #-}
replace :: (Int -> Array (MBR r) -> Array a -> b) -> Int -> Array (MBR r) -> Array a -> Int -> MBR r -> a -> b
replace f n brs as i br a = f n (Array.replace n brs i br) (Array.replace n as i a)

{-# INLINEABLE replaceSnoc #-}
replaceSnoc
  :: (Int -> Array (MBR r) -> Array a -> b) -> Int -> Array (MBR r) -> Array a -> Int -> MBR r -> a -> MBR r -> a -> b
replaceSnoc f n brs as i ba a bb b =
  f (n + 1) (Array.replaceSnoc n brs i ba bb) (Array.replaceSnoc n as i a b)

{-# INLINEABLE lose #-}
lose :: (Int -> Array (MBR r) -> Array a -> b) -> Int -> Array (MBR r) -> Array a -> Int -> b
lose f n brs as i = f (n - 1) (Array.lose n brs i) (Array.lose n as i)


{-# INLINEABLE mk #-}
mk :: (Int -> Array (MBR r) -> Array a -> b) -> [(MBR r, a)] -> b
mk f as = f (List.length as) (arrayFromList $ fst <$> as) (arrayFromList $ snd <$> as)



{-# INLINEABLE leastEnlargement #-}
leastEnlargement :: (Num r, Ord r, Prim r) => MBR r -> Int -> Array (MBR r) -> Int
leastEnlargement bx n brs = loop 1 (indexArray brs 0) 0
  where
    loop m ba i | m > n - 1 = i
                | otherwise = let (# bb #) = indexArray## brs m
                              in case smaller ba bb of
                                   GT -> loop (m + 1) bb m
                                   _  -> loop (m + 1) ba i

    smaller ba bb =
      case compare (MBR.area (MBR.union ba bx) - MBR.area ba)
                   (MBR.area (MBR.union bb bx) - MBR.area bb) of
        EQ -> MBR.area ba `compare` MBR.area bb
        eq -> eq



mostWasteful :: (Num r, Ord r, Prim r) => MBR r -> MBR r -> r
mostWasteful ba bb = MBR.area (MBR.union ba bb) - MBR.area ba - MBR.area bb



weave :: [a] -> [(a, a, [a])]
weave (x:xs) = (($ x) <$> sub xs) <> ((\(a, b, zs) -> (a, b, x:zs)) <$> weave xs)
  where
    sub :: [a] -> [a -> (a, a, [a])]
    sub (y:ys) = (\a -> (a, y, ys)) : ((\f -> (\(a, b, zs) -> (a, b, y:zs)) . f) <$> sub ys)
    sub    []  = []

weave    []  = []



compareEnlargement
  :: (Num r, Ord r, Prim r) => MBR r -> Int -> MBR r -> Int -> MBR r -> Ordering
compareEnlargement bx n ba m bb =
  case compare (MBR.area (MBR.union ba bx) - MBR.area ba)
               (MBR.area (MBR.union bb bx) - MBR.area bb) of
    EQ -> case MBR.area ba `compare` MBR.area bb of
            EQ -> n `compare` m
            eq -> eq
    eq -> eq



type Split r a = [(MBR r, a)] -> ([(MBR r, a)], [(MBR r, a)])



{-# INLINEABLE split #-}
split
  :: (Num r, Ord r, Prim r)
  => (Int -> Array (MBR r) -> Array a -> b) -> Split r a -> [(MBR r, a)] -> (b, b)
split f s xs = let (ls, rs) = s xs in (mk f ls, mk f rs)



mbr :: (Fold.Foldable t, Functor t, Ord r, Prim r) => t (MBR r, a) -> MBR r
mbr as = Fold.foldr1 MBR.union $ fst <$> as

{-# INLINEABLE quad #-}
quad :: (Num r, Ord r, Prim r) => Split r a
quad xs =
  let (a, b, cs) = List.maximumBy (compare `on` (\((x, _), (y, _), _) -> mostWasteful x y)) $ weave xs
  in quad' [a] [b] cs

quad'
  :: (Num r, Ord r, Prim r)
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
  :: (Num r, Ord r, Prim r)
  => ([(MBR r, a)], [(MBR r, a)]) -> ([(MBR r, a)], [(MBR r, a)]) -> Ordering
smallestOverlap (a, b) (c, d) =
  case MBR.overlap (mbr a) (mbr b) `compare` MBR.overlap (mbr c) (mbr d) of
    EQ -> (MBR.area (mbr a) + MBR.area (mbr b)) `compare` (MBR.area (mbr c) + MBR.area (mbr d))
    eq -> eq


{-# INLINEABLE sorted #-}
sorted :: (Num r, Ord r, Prim r) => Split r a
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
highestDistance :: (Num r, Ord r, Prim r) => MBR r -> MBR r -> MBR r -> Ordering
highestDistance = on (comparing Down) . MBR.distance



-- | Slices a list up into parts of given length.
part :: Int -> [a] -> [[a]]
part n xs@(_:_) = take n xs : part n (drop n xs)
part _ []       = []


{-# INLINEABLE ordX #-}
-- | Ordering based on horizontal center points.
ordX :: (Num r, Ord r, Prim r) => MBR r -> MBR r -> Ordering
ordX (MBR xmin _ xmax _) (MBR xmin' _ xmax' _) = compare (xmin + xmax) (xmin' + xmax')

{-# INLINEABLE ordY #-}
-- | Ordering based on vertical center points.
ordY :: (Num r, Ord r, Prim r) => MBR r -> MBR r -> Ordering
ordY (MBR _ ymin _ ymax) (MBR _ ymin' _ ymax') = compare (ymin + ymax) (ymin' + ymax')



{-# INLINEABLE arrayToList #-}
-- | Helper for converting to list based on known length.
arrayToList :: Int -> Array a -> [a]
arrayToList n arr = Fold.foldr (\i -> (:) $ indexArray arr i) [] [0 .. n - 1]

{-# INLINEABLE discard #-}
-- | Removes an element from the list based on index.
discard :: Int -> [a] -> [a]
discard i as = take i as <> drop (i + 1) as



{-# INLINEABLE union #-}
-- | Common bounding rectangle for a 'Node'.
--   Will error out if the bounding rectangle array is empty.
union :: (Ord r, Prim r) => Node r a -> MBR r
union (Node n brs _) = Array.foldr1 MBR.union n brs
union (Leaf n brs _) = Array.foldr1 MBR.union n brs
