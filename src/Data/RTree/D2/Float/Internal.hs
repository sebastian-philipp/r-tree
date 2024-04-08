{-# LANGUAGE BangPatterns
           , PatternSynonyms
           , RankNTypes
           , ViewPatterns
           , UnboxedTuples #-}

module Data.RTree.D2.Float.Internal
  ( MBR (UnsafeMBR, MBR)
  , validMBR
  , eqMBR
  , unionMBR
  , areaMBR
  , marginMBR
  , distanceMBR
  , containsMBR
  , containsMBR'
  , intersectionMBR
  , intersectionMBR'

  , Predicate (..)
  , equals
  , intersects
  , intersects'
  , contains
  , contains'
  , containedBy
  , containedBy'

  , RTree (..)

  , Data.RTree.D2.Float.Internal.null
  , Data.RTree.D2.Float.Internal.size

  , Data.RTree.D2.Float.Internal.map
  , map'
  , mapWithKey
  , mapWithKey'
  , adjustRangeWithKey
  , adjustRangeWithKey'

  , Data.RTree.D2.Float.Internal.foldl
  , Data.RTree.D2.Float.Internal.foldl'
  , foldlWithKey
  , foldlWithKey'
  , foldlRangeWithKey
  , foldlRangeWithKey'

  , Data.RTree.D2.Float.Internal.foldr
  , Data.RTree.D2.Float.Internal.foldr'
  , foldrWithKey
  , foldrWithKey'
  , foldrRangeWithKey
  , foldrRangeWithKey'

  , Data.RTree.D2.Float.Internal.foldMap
  , foldMapWithKey
  , foldMapRangeWithKey

  , Data.RTree.D2.Float.Internal.traverse
  , traverseWithKey
  , traverseRangeWithKey

  , insertGut
  , insert
  , delete

  , bulkSTR
  ) where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Exception (assert)
import           Data.Bits
import           Data.Foldable
import           Data.Functor.Classes
import           Data.Function
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty (..), (<|))
import           Text.Show



-- | Two-dimensional minimum bounding rectangle is defined as two intervals,
--   each along a separate axis, where every endpoint is either
--   bounded and closed (i.e. \( [a, b] \)), or infinity (i.e. \((\pm \infty, b]\)).
--
--   Degenerate intervals (i.e. \([a,a]\)) are permitted.
data MBR = -- | Invariants: \( x_{min} \le x_{max}, y_{min} \le y_{max} \).
           UnsafeMBR
             {-# UNPACK #-} !Float -- ^ \( x_{min} \)
             {-# UNPACK #-} !Float -- ^ \( y_{min} \)
             {-# UNPACK #-} !Float -- ^ \( x_{max} \)
             {-# UNPACK #-} !Float -- ^ \( y_{max} \)

{-# COMPLETE MBR #-}
-- | Reorders coordinates to fit internal invariants.
--
--   Pattern matching guarantees \( x_{0} \le x_{1}, y_{0} \le y_{1} \).
pattern MBR
  :: Float -- ^ \( x_0 \)
  -> Float -- ^ \( y_0 \)
  -> Float -- ^ \( x_1 \)
  -> Float -- ^ \( y_1 \)
  -> MBR
pattern MBR xmin ymin xmax ymax <- UnsafeMBR xmin ymin xmax ymax
  where
    MBR x0 y0 x1 y1 =
      let !(# xmin, xmax #) | x0 <= x1  = (# x0, x1 #)
                            | otherwise = (# x1, x0 #)

          !(# ymin, ymax #) | y0 <= y1  = (# y0, y1 #)
                            | otherwise = (# y1, y0 #)

      in UnsafeMBR xmin ymin xmax ymax

instance Show MBR where
  showsPrec d (UnsafeMBR xmin ymin xmax ymax) =
    showParen (d > 10) $ showString "MBR " . showsPrec 11 xmin
                            . showChar ' ' . showsPrec 11 ymin
                            . showChar ' ' . showsPrec 11 xmax
                            . showChar ' ' . showsPrec 11 ymax

instance Eq MBR where
  (==) = eqMBR



-- | Check whether lower endpoints are smaller or equal to the respective upper ones.
validMBR :: MBR -> Bool
validMBR (MBR xmin ymin xmax ymax) = xmin <= xmax && ymin <= ymax

{-# INLINE eqMBR #-}
-- | Check whether two rectangles are equal.
eqMBR :: MBR -> MBR -> Bool
eqMBR (MBR xmin ymin xmax ymax) (MBR xmin' ymin' xmax' ymax') =
  xmin == xmin' && ymin == ymin' && xmax == xmax' && ymax == ymax'


{-# INLINE unionMBR #-}
-- | Resulting rectangle contains both input rectangles.
unionMBR :: MBR -> MBR -> MBR
unionMBR (MBR xmin ymin xmax ymax) (MBR xmin' ymin' xmax' ymax') =
  MBR (min xmin xmin') (min ymin ymin') (max xmax xmax') (max ymax ymax')


{-# INLINE areaMBR #-}
-- | Proper area.
areaMBR :: MBR -> Float
areaMBR (MBR xmin ymin xmax ymax) = (xmax - xmin) * (ymax - ymin)

{-# INLINE marginMBR #-}
-- | Half a perimeter.
marginMBR :: MBR -> Float
marginMBR (MBR xmin ymin xmax ymax) = (xmax - xmin) + (ymax - ymin)

{-# INLINE overlapMBR #-}
overlapMBR :: MBR -> MBR -> Float
overlapMBR =
  intersectionMBR_ $ \x y x' y' ->
    if x < x' && y < y'
      then areaMBR (MBR x y x' y')
      else 0


{-# INLINE distanceMBR #-}
-- | Square distance between double the centers of two rectangles.
distanceMBR :: MBR -> MBR -> Float
distanceMBR (MBR xmin ymin xmax ymax) (MBR xmin' ymin' xmax' ymax') =
  let x = (xmax' + xmin') - (xmax + xmin)
      y = (ymax' + ymin') - (ymax + ymin)
  in x * x + y * y


{-# INLINE containsMBR #-}
-- | Whether left rectangle contains right one.
containsMBR :: MBR -> MBR -> Bool
containsMBR (MBR xmin ymin xmax ymax) (MBR xmin' ymin' xmax' ymax') =
  xmin <= xmin' && ymin <= ymin' && xmax >= xmax' && ymax >= ymax'

{-# INLINE containsMBR' #-}
-- | Whether left rectangle contains right one without touching any of the sides.
containsMBR' :: MBR -> MBR -> Bool
containsMBR' (MBR xmin ymin xmax ymax) (MBR xmin' ymin' xmax' ymax') =
  xmin < xmin' && ymin < ymin' && xmax > xmax' && ymax > ymax'



{-# INLINE intersectionMBR #-}
-- | Intersection of two rectangles, if any exists.
intersectionMBR :: MBR -> MBR -> Maybe MBR
intersectionMBR =
  intersectionMBR_ $ \x y x' y' ->
    if x <= x' && y <= y'
      then Just (MBR x y x' y')
      else Nothing

{-# INLINE intersectionMBR' #-}
-- | Intersection of two rectangles, if any exists, excluding the side cases where
--   the result would be a point or a line.
intersectionMBR' :: MBR -> MBR -> Maybe MBR
intersectionMBR' =
  intersectionMBR_ $ \x y x' y' ->
    if x < x' && y < y'
      then Just (MBR x y x' y')
      else Nothing

{-# INLINE intersectionMBR_ #-}
intersectionMBR_ :: (Float -> Float -> Float -> Float -> a) -> MBR -> MBR -> a
intersectionMBR_ f (MBR xmin ymin xmax ymax) (MBR xmin' ymin' xmax' ymax') =
  let x  = max xmin xmin'
      y  = max ymin ymin'
      x' = min xmax xmax'
      y' = min ymax ymax'

  in f x y x' y'

{-# INLINE intersectsMBR #-}
intersectsMBR :: MBR -> MBR -> Bool
intersectsMBR = intersectionMBR_ $ \x y x' y' -> x <= x' && y <= y'

{-# INLINE intersectsMBR' #-}
intersectsMBR' :: MBR -> MBR -> Bool
intersectsMBR' = intersectionMBR_ $ \x y x' y' -> x < x' && y < y'



-- | Comparison function.
data Predicate = Predicate
                   (MBR -> Bool) -- ^ Matches nodes
                   (MBR -> Bool) -- ^ Matches leaves

{-# INLINE equals #-}
-- | Matches exactly the provided t'MBR'.
equals :: MBR -> Predicate
equals bx = Predicate (\ba -> containsMBR ba bx) (eqMBR bx)

{-# INLINE intersects #-}
-- | Matches any t'MBR' that intersects the provided one.
intersects:: MBR -> Predicate
intersects bx = Predicate (intersectsMBR bx) (intersectsMBR bx)

{-# INLINE intersects' #-}
-- | Matches any t'MBR' that intersects the provided one, if the
--   intersection is not a line or a point.
intersects' :: MBR -> Predicate
intersects' bx = Predicate (intersectsMBR' bx) (intersectsMBR' bx)

{-# INLINE contains #-}
-- | Matches any t'MBR' that contains the provided one.
contains :: MBR -> Predicate
contains bx = Predicate (\ba -> containsMBR ba bx) (\ba -> containsMBR ba bx)

{-# INLINE contains' #-}
-- | Matches any t'MBR' that contains the provided one,
--   excluding ones that touch it on one or more sides.
contains' :: MBR -> Predicate
contains' bx = Predicate (\ba -> containsMBR ba bx) (\ba -> containsMBR' ba bx)

{-# INLINE containedBy #-}
-- | Matches any t'MBR' that is contained within the provided one.
containedBy :: MBR -> Predicate
containedBy bx = Predicate (intersectsMBR bx) (containsMBR bx)

{-# INLINE containedBy' #-}
-- | Matches any t'MBR' that is contained within the provided one,
--   excluding ones that touch it on one or more sides.
containedBy' :: MBR -> Predicate
containedBy' bx = Predicate (intersectsMBR bx) (containsMBR' bx)



instance Show a => Show (RTree a) where
  showsPrec = liftShowsPrec showsPrec showList

instance Show1 RTree where
  liftShowsPrec showsPrec_ showList_ t r =
    showParen (t > 10) $
      showListWith (liftShowsPrec showsPrec_ showList_ 0) $
        foldrWithKey (\k a -> (:) (k, a)) [] r

instance Eq a => Eq (RTree a) where
  (==) = liftEq (==)

instance Eq1 RTree where
  liftEq f = go
    where
      {-# INLINE node #-}
      node ba a bb b = eqMBR ba bb && go a b

      {-# INLINE leaf #-}
      leaf ba a bb b = eqMBR ba bb && f a b

      go m n =
        case m of
          Node2 ba a bb b ->
            case n of
              Node2 be e bg g -> node ba a be e && node bb b bg g
              _               -> False

          Node3 ba a bb b bc c ->
            case n of
              Node3 be e bg g bh h -> node ba a be e && node bb b bg g && node bc c bh h
              _                    -> False

          Node4 ba a bb b bc c bd d ->
            case n of
              Node4 be e bg g bh h bi i ->
                node ba a be e && node bb b bg g && node bc c bh h && node bd d bi i

              _                         -> False

          Leaf2 ba a bb b ->
            case n of
              Leaf2 be e bg g -> leaf ba a be e && leaf bb b bg g
              _               -> False

          Leaf3 ba a bb b bc c ->
            case n of
              Leaf3 be e bg g bh h -> leaf ba a be e && leaf bb b bg g && leaf bc c bh h
              _                    -> False

          Leaf4 ba a bb b bc c bd d ->
            case n of
              Leaf4 be e bg g bh h bi i ->
                leaf ba a be e && leaf bb b bg g && leaf bc c bh h && leaf bd d bi i

              _                     -> False

          Leaf1 ba a ->
            case n of
              Leaf1 bb b -> eqMBR ba bb && f a b
              _          -> False

          Empty      ->
            case n of
              Empty -> True
              _     -> False



instance NFData a => NFData (RTree a) where
  rnf = liftRnf rnf

instance NFData1 RTree where
  liftRnf f = go
    where
      go n =
        case n of
          Node2 _ a _ b         -> go a `seq` go b
          Node3 _ a _ b _ c     -> go a `seq` go b `seq` go c
          Node4 _ a _ b _ c _ d -> go a `seq` go b `seq` go c `seq` go d

          Leaf2 _ a _ b         -> f a `seq` f b
          Leaf3 _ a _ b _ c     -> f a `seq` f b `seq` f c
          Leaf4 _ a _ b _ c _ d -> f a `seq` f b `seq` f c `seq` f d

          Leaf1 _ a             -> f a
          Empty                 -> ()



-- | Uses 'Data.RTree.D2.Float.Internal.map'.
instance Functor RTree where
  fmap = Data.RTree.D2.Float.Internal.map

instance Foldable RTree where
  foldl = Data.RTree.D2.Float.Internal.foldl

  foldr = Data.RTree.D2.Float.Internal.foldr

  foldMap = Data.RTree.D2.Float.Internal.foldMap

  foldl' = Data.RTree.D2.Float.Internal.foldl'

  foldr' = Data.RTree.D2.Float.Internal.foldr'

  null = Data.RTree.D2.Float.Internal.null

  length = size


instance Traversable RTree where
  traverse = Data.RTree.D2.Float.Internal.traverse



-- | Spine-strict two-dimensional R-tree.
data RTree a = Node2 {-# UNPACK #-} !MBR !(RTree a) {-# UNPACK #-} !MBR !(RTree a)
             | Node3 {-# UNPACK #-} !MBR !(RTree a) {-# UNPACK #-} !MBR !(RTree a) {-# UNPACK #-} !MBR !(RTree a)
             | Node4 {-# UNPACK #-} !MBR !(RTree a) {-# UNPACK #-} !MBR !(RTree a) {-# UNPACK #-} !MBR !(RTree a) {-# UNPACK #-} !MBR !(RTree a)

             | Leaf2 {-# UNPACK #-} !MBR a {-# UNPACK #-} !MBR a
             | Leaf3 {-# UNPACK #-} !MBR a {-# UNPACK #-} !MBR a {-# UNPACK #-} !MBR a
             | Leaf4 {-# UNPACK #-} !MBR a {-# UNPACK #-} !MBR a {-# UNPACK #-} !MBR a {-# UNPACK #-} !MBR a

               -- | Invariant: only allowed as the root node.
             | Leaf1 {-# UNPACK #-} !MBR a

               -- | Invariant: only allowed as the root node.
             | Empty



-- | \(\mathcal{O}(1)\).
--   Check if the tree is empty.
null :: RTree a -> Bool
null Empty = True
null _     = False

-- | \(\mathcal{O}(n)\).
--   Calculate the number of elements stored in the tree.
--   The returned number is guaranteed to be non-negative.
size :: RTree a -> Int
size = go
  where
    go n =
      case n of
        Node2 _ a _ b         -> let !w = go a
                                     !x = go b

                                 in w + x

        Node3 _ a _ b _ c     -> let !w = go a
                                     !x = go b
                                     !y = go c

                                 in w + x + y

        Node4 _ a _ b _ c _ d -> let !w = go a
                                     !x = go b
                                     !y = go c
                                     !z = go d

                                 in w + x + y + z

        Leaf2 _ _ _ _         -> 2
        Leaf3 _ _ _ _ _ _     -> 3
        Leaf4 _ _ _ _ _ _ _ _ -> 4

        Leaf1 _ _             -> 1
        Empty                 -> 0



-- | \(\mathcal{O}(n)\).
--   Map a function over all values.
map :: (a -> b) -> RTree a -> RTree b
map f = go
  where
    go n =
      case n of
        Node2 ba a bb b           ->
          Node2 ba (go a) bb (go b)

        Node3 ba a bb b bc c      ->
          Node3 ba (go a) bb (go b) bc (go c)

        Node4 ba a bb b bc c bd d ->
          Node4 ba (go a) bb (go b) bc (go c) bd (go d)

        Leaf2 ba a bb b           ->
          Leaf2 ba (f a) bb (f b)

        Leaf3 ba a bb b bc c      ->
          Leaf3 ba (f a) bb (f b) bc (f c)

        Leaf4 ba a bb b bc c bd d ->
          Leaf4 ba (f a) bb (f b) bc (f c) bd (f d)

        Leaf1 ba a                ->
          Leaf1 ba (f a)

        Empty                     -> Empty

-- | \(\mathcal{O}(n)\).
--   Map a function over all values and evaluate the results to WHNF.
map' :: (a -> b) -> RTree a -> RTree b
map' f = go
  where
    go n =
      case n of
        Node2 ba a bb b           ->
          Node2 ba (go a) bb (go b)

        Node3 ba a bb b bc c      ->
          Node3 ba (go a) bb (go b) bc (go c)

        Node4 ba a bb b bc c bd d ->
          Node4 ba (go a) bb (go b) bc (go c) bd (go d)

        Leaf2 ba a bb b           ->
          let !a' = f a
              !b' = f b

          in Leaf2 ba a' bb b'

        Leaf3 ba a bb b bc c      ->
          let !a' = f a
              !b' = f b
              !c' = f c

          in Leaf3 ba a' bb b' bc c'

        Leaf4 ba a bb b bc c bd d ->
          let !a' = f a
              !b' = f b
              !c' = f c
              !d' = f d

          in Leaf4 ba a' bb b' bc c' bd d'

        Leaf1 ba a                ->
          Leaf1 ba $! f a
        
        Empty                     -> Empty


-- | \(\mathcal{O}(n)\).
--   Map a function over all t'MBR's and their respective values.
mapWithKey :: (MBR -> a -> b) -> RTree a -> RTree b
mapWithKey f = go
  where
    go n =
      case n of
        Node2 ba a bb b           ->
          Node2 ba (go a) bb (go b)

        Node3 ba a bb b bc c      ->
          Node3 ba (go a) bb (go b) bc (go c)

        Node4 ba a bb b bc c bd d ->
          Node4 ba (go a) bb (go b) bc (go c) bd (go d)

        Leaf2 ba a bb b           ->
          Leaf2 ba (f ba a) bb (f bb b)

        Leaf3 ba a bb b bc c      ->
          Leaf3 ba (f ba a) bb (f bb b) bc (f bc c)

        Leaf4 ba a bb b bc c bd d ->
          Leaf4 ba (f ba a) bb (f bb b) bc (f bc c) bd (f bd d)

        Leaf1 ba a                ->
          Leaf1 ba (f ba a)

        Empty                     -> Empty

-- | \(\mathcal{O}(n)\).
--   Map a function over all t'MBR's and their respective values
--   and evaluate the results to WHNF.
mapWithKey' :: (MBR -> a -> b) -> RTree a -> RTree b
mapWithKey' f = go
  where
    go n =
      case n of
        Node2 ba a bb b           ->
          Node2 ba (go a) bb (go b)

        Node3 ba a bb b bc c      ->
          Node3 ba (go a) bb (go b) bc (go c)

        Node4 ba a bb b bc c bd d ->
          Node4 ba (go a) bb (go b) bc (go c) bd (go d)

        Leaf2 ba a bb b           ->
          let !a' = f ba a
              !b' = f bb b

          in Leaf2 ba a' bb b'

        Leaf3 ba a bb b bc c      ->
          let !a' = f ba a
              !b' = f bb b
              !c' = f bc c

          in Leaf3 ba a' bb b' bc c'

        Leaf4 ba a bb b bc c bd d ->
          let !a' = f ba a
              !b' = f bb b
              !c' = f bc c
              !d' = f bd d

          in Leaf4 ba a' bb b' bc c' bd d'

        Leaf1 ba a                ->
          Leaf1 ba $! f ba a

        Empty                     -> Empty



{-# INLINE adjustRangeWithKey #-}
-- | \(\mathcal{O}(r + n_{range})\).
--   Map a function over t'MBR's that match the 'Predicate' and their respective values.
adjustRangeWithKey :: Predicate -> (MBR -> a -> a) -> RTree a -> RTree a
adjustRangeWithKey (Predicate nodePred leafPred) f = go
  where
    {-# INLINE node #-}
    node bx x
      | nodePred bx = go x
      | otherwise   = x

    {-# INLINE leaf #-}
    leaf bx x
      | leafPred bx = f bx x
      | otherwise   = x

    go n =
      case n of
        Node2 ba a bb b           ->
          Node2 ba (node ba a) bb (node bb b)

        Node3 ba a bb b bc c      ->
          Node3 ba (node ba a) bb (node bb b) bc (node bc c)

        Node4 ba a bb b bc c bd d ->
          Node4 ba (node ba a) bb (node bb b) bc (node bc c) bd (node bd d)

        Leaf2 ba a bb b           ->
          Leaf2 ba (leaf ba a) bb (leaf bb b)

        Leaf3 ba a bb b bc c      ->
          Leaf3 ba (leaf ba a) bb (leaf bb b) bc (leaf bc c)

        Leaf4 ba a bb b bc c bd d ->
          Leaf4 ba (leaf ba a) bb (leaf bb b) bc (leaf bc c) bd (leaf bd d)

        Leaf1 ba a                ->
          Leaf1 ba (leaf ba a)

        Empty                     -> Empty

{-# INLINE adjustRangeWithKey' #-}
-- | \(\mathcal{O}(r + n_{range})\).
--   Map a function over t'MBR's that match the 'Predicate' and their respective values
--   and evaluate the results to WHNF.
adjustRangeWithKey' :: Predicate -> (MBR -> a -> a) -> RTree a -> RTree a
adjustRangeWithKey' (Predicate nodePred leafPred) f = go
  where
    {-# INLINE node #-}
    node bx x
      | nodePred bx = go x
      | otherwise   = x

    {-# INLINE leaf #-}
    leaf bx x
      | leafPred bx = f bx x
      | otherwise   = x

    go n =
      case n of
        Node2 ba a bb b           ->
          Node2 ba (node ba a) bb (node bb b)

        Node3 ba a bb b bc c      ->
          Node3 ba (node ba a) bb (node bb b) bc (node bc c)

        Node4 ba a bb b bc c bd d ->
          Node4 ba (node ba a) bb (node bb b) bc (node bc c) bd (node bd d)

        Leaf2 ba a bb b           ->
          let !a' = leaf ba a
              !b' = leaf bb b

          in Leaf2 ba a' bb b'

        Leaf3 ba a bb b bc c      ->
          let !a' = leaf ba a
              !b' = leaf bb b
              !c' = leaf bc c

          in Leaf3 ba a' bb b' bc c'

        Leaf4 ba a bb b bc c bd d ->
          let !a' = leaf ba a
              !b' = leaf bb b
              !c' = leaf bc c
              !d' = leaf bd d

          in Leaf4 ba a' bb b' bc c' bd d'

        Leaf1 ba a                ->
          Leaf1 ba $! leaf ba a

        Empty                     -> Empty



-- | \(\mathcal{O}(n_R)\).
--   Fold left-to-right over all values.
foldl :: (b -> a -> b) -> b -> RTree a -> b
foldl f = go
  where
    go z n =
      case n of
        Node2 _ a _ b         ->         go (go z a) b
        Node3 _ a _ b _ c     ->     go (go (go z a) b) c
        Node4 _ a _ b _ c _ d -> go (go (go (go z a) b) c) d

        Leaf2 _ a _ b         ->       f (f z a) b
        Leaf3 _ a _ b _ c     ->    f (f (f z a) b) c
        Leaf4 _ a _ b _ c _ d -> f (f (f (f z a) b) c) d

        Leaf1 _ a             -> f z a
        Empty                 -> z

-- | \(\mathcal{O}(n)\).
--   Fold left-to-right over all values, applying the operator function strictly.
foldl' :: (b -> a -> b) -> b -> RTree a -> b
foldl' f = go
  where
    {-# INLINE leaf #-}
    leaf !z x = f z x

    go !z n =
      case n of
        Node2 _ a _ b         ->         go (go z a) b
        Node3 _ a _ b _ c     ->     go (go (go z a) b) c
        Node4 _ a _ b _ c _ d -> go (go (go (go z a) b) c) d

        Leaf2 _ a _ b         ->             leaf (leaf z a) b
        Leaf3 _ a _ b _ c     ->       leaf (leaf (leaf z a) b) c
        Leaf4 _ a _ b _ c _ d -> leaf (leaf (leaf (leaf z a) b) c) d

        Leaf1 _ a             -> leaf z a
        Empty                 -> z


-- | \(\mathcal{O}(n_R)\).
--   Fold left-to-right over all t'MBR's and their respective values.
foldlWithKey :: (b -> MBR -> a -> b) -> b -> RTree a -> b
foldlWithKey f = go
  where
    go z n =
      case n of
        Node2 _  a _  b           ->         go (go z a) b
        Node3 _  a _  b _  c      ->     go (go (go z a) b) c
        Node4 _  a _  b _  c _  d -> go (go (go (go z a) b) c) d

        Leaf2 ba a bb b           ->       f (f z ba a) bb b
        Leaf3 ba a bb b bc c      ->    f (f (f z ba a) bb b) bc c
        Leaf4 ba a bb b bc c bd d -> f (f (f (f z ba a) bb b) bc c) bd d

        Leaf1 ba a                -> f z ba a
        Empty                     -> z

-- | \(\mathcal{O}(n)\).
--   Fold left-to-right over all t'MBR's and their respective values,
--   applying the operator function strictly.
foldlWithKey' :: (b -> MBR -> a -> b) -> b -> RTree a -> b
foldlWithKey' f = go
  where
    {-# INLINE leaf #-}
    leaf !z bx x = f z bx x

    go z n =
      case n of
        Node2 _  a _  b           ->         go (go z a) b
        Node3 _  a _  b _  c      ->     go (go (go z a) b) c
        Node4 _  a _  b _  c _  d -> go (go (go (go z a) b) c) d

        Leaf2 ba a bb b           ->             leaf (leaf z ba a) bb b
        Leaf3 ba a bb b bc c      ->       leaf (leaf (leaf z ba a) bb b) bc c
        Leaf4 ba a bb b bc c bd d -> leaf (leaf (leaf (leaf z ba a) bb b) bc c) bd d
 
        Leaf1 ba a                -> leaf z ba a
        Empty                     -> z


{-# INLINE foldlRangeWithKey #-}
-- | \(\mathcal{O}(r + n_{{range}_R})\).
--   Fold left-to-right over t'MBR's that match the 'Predicate'
--   and their respective values.
foldlRangeWithKey :: Predicate -> (b -> MBR -> a -> b) -> b -> RTree a -> b
foldlRangeWithKey (Predicate nodePred leafPred) f = go
  where
    {-# INLINE node #-}
    node z bx x
      | nodePred bx = go z x
      | otherwise   = z

    {-# INLINE leaf #-}
    leaf z bx x
      | leafPred bx = f z bx x
      | otherwise   = z

    go z n =
      case n of
        Node2 ba a bb b           ->             node (node z ba a) bb b
        Node3 ba a bb b bc c      ->       node (node (node z ba a) bb b) bc c
        Node4 ba a bb b bc c bd d -> node (node (node (node z ba a) bb b) bc c) bd d

        Leaf2 ba a bb b           ->             leaf (leaf z ba a) bb b
        Leaf3 ba a bb b bc c      ->       leaf (leaf (leaf z ba a) bb b) bc c
        Leaf4 ba a bb b bc c bd d -> leaf (leaf (leaf (leaf z ba a) bb b) bc c) bd d

        Leaf1 ba a                -> leaf z ba a
        Empty                     -> z

{-# INLINE foldlRangeWithKey' #-}
-- | \(\mathcal{O}(r + n_{range})\).
--   Fold left-to-right over t'MBR's that match the 'Predicate'
--   and their respective values, applying the operator function strictly.
foldlRangeWithKey' :: Predicate -> (b -> MBR -> a -> b) -> b -> RTree a -> b
foldlRangeWithKey' (Predicate nodePred leafPred) f = go
  where
    {-# INLINE node #-}
    node z bx x
      | nodePred bx = go z x
      | otherwise   = z

    {-# INLINE leaf #-}
    leaf !z bx x
      | leafPred bx = f z bx x
      | otherwise   = z

    go z n =
      case n of
        Node2 ba a bb b           ->             node (node z ba a) bb b
        Node3 ba a bb b bc c      ->       node (node (node z ba a) bb b) bc c
        Node4 ba a bb b bc c bd d -> node (node (node (node z ba a) bb b) bc c) bd d

        Leaf2 ba a bb b           ->             leaf (leaf z ba a) bb b
        Leaf3 ba a bb b bc c      ->       leaf (leaf (leaf z ba a) bb b) bc c
        Leaf4 ba a bb b bc c bd d -> leaf (leaf (leaf (leaf z ba a) bb b) bc c) bd d

        Leaf1 ba a                -> leaf z ba a
        Empty                     -> z



-- | \(\mathcal{O}(n_L)\).
--   Fold right-to-left over all values.
foldr :: (a -> b -> b) -> b -> RTree a -> b
foldr f = go
  where
    go z n =
      case n of
        Node2 _  a _  b           -> go (go         z       b) a
        Node3 _  a _  b _  c      -> go (go (go     z    c) b) a
        Node4 _  a _  b _  c _  d -> go (go (go (go z d) c) b) a

        Leaf2 _  a _  b           -> f a (f b           z)
        Leaf3 _  a _  b _  c      -> f a (f b (f c      z))
        Leaf4 _  a _  b _  c _  d -> f a (f b (f c (f d z)))

        Leaf1 _ a                 -> f a z
        Empty                     -> z

-- | \(\mathcal{O}(n)\).
--   Fold right-to-left over all values, applying the operator function strictly.
foldr' :: (a -> b -> b) -> b -> RTree a -> b
foldr' f = go
  where
    {-# INLINE leaf #-}
    leaf x !z = f x z

    go z n =
      case n of
        Node2 _  a _  b           -> go (go         z       b) a
        Node3 _  a _  b _  c      -> go (go (go     z    c) b) a
        Node4 _  a _  b _  c _  d -> go (go (go (go z d) c) b) a

        Leaf2 _  a _  b           -> leaf a (leaf b                 z)
        Leaf3 _  a _  b _  c      -> leaf a (leaf b (leaf c         z))
        Leaf4 _  a _  b _  c _  d -> leaf a (leaf b (leaf c (leaf d z)))

        Leaf1 _ a                 -> leaf a z
        Empty                     -> z


-- | \(\mathcal{O}(n_L)\).
--   Fold right-to-left over all t'MBR's and their respective values.
foldrWithKey :: (MBR -> a -> b -> b) -> b -> RTree a -> b
foldrWithKey f = go
  where
    go z n =
      case n of
        Node2 _  a _  b           -> go (go         z       b) a
        Node3 _  a _  b _  c      -> go (go (go     z    c) b) a
        Node4 _  a _  b _  c _  d -> go (go (go (go z d) c) b) a

        Leaf2 ba a bb b           -> f ba a (f bb b                 z)
        Leaf3 ba a bb b bc c      -> f ba a (f bb b (f bc c         z))
        Leaf4 ba a bb b bc c bd d -> f ba a (f bb b (f bc c (f bd d z)))

        Leaf1 ba a                -> f ba a z
        Empty                     -> z

-- | \(\mathcal{O}(n)\).
--   Fold right-to-left over all t'MBR's and their respective values,
--   applying the operator function strictly.
foldrWithKey' :: (MBR -> a -> b -> b) -> b -> RTree a -> b
foldrWithKey' f = go
  where
    {-# INLINE leaf #-}
    leaf bx x !z = f bx x z

    go z n =
      case n of
        Node2 _  a _  b           -> go (go         z       b) a
        Node3 _  a _  b _  c      -> go (go (go     z    c) b) a
        Node4 _  a _  b _  c _  d -> go (go (go (go z d) c) b) a

        Leaf2 ba a bb b           -> leaf ba a (leaf bb b                       z)
        Leaf3 ba a bb b bc c      -> leaf ba a (leaf bb b (leaf bc c            z))
        Leaf4 ba a bb b bc c bd d -> leaf ba a (leaf bb b (leaf bc c (leaf bd d z)))

        Leaf1 ba a                -> leaf ba a z
        Empty                     -> z


{-# INLINE foldrRangeWithKey #-}
-- | \(\mathcal{O}(r + n_{{range}_L})\).
--   Fold right-to-left over t'MBR's that match the 'Predicate'
--   and their respective values.
foldrRangeWithKey :: Predicate -> (MBR -> a -> b -> b) -> b -> RTree a -> b
foldrRangeWithKey (Predicate nodePred leafPred) f = go
  where
    {-# INLINE node #-}
    node z bx x
      | nodePred bx = go z x
      | otherwise   = z

    {-# INLINE leaf #-}
    leaf bx x z
      | leafPred bx = f bx x z
      | otherwise   = z

    go z n =
      case n of
        Node2 ba a bb b           -> node (node             z             bb b) ba a
        Node3 ba a bb b bc c      -> node (node (node       z       bc c) bb b) ba a
        Node4 ba a bb b bc c bd d -> node (node (node (node z bd d) bc c) bb b) ba a

        Leaf2 ba a bb b           -> leaf ba a (leaf bb b                       z)
        Leaf3 ba a bb b bc c      -> leaf ba a (leaf bb b (leaf bc c            z))
        Leaf4 ba a bb b bc c bd d -> leaf ba a (leaf bb b (leaf bc c (leaf bd d z)))

        Leaf1 ba a -> leaf ba a z
        Empty      -> z

{-# INLINE foldrRangeWithKey' #-}
-- | \(\mathcal{O}(r + n_{range})\).
--   Fold right-to-left over t'MBR's that match the 'Predicate'
--   and their respective values, applying the operator function strictly.
foldrRangeWithKey' :: Predicate -> (MBR -> a -> b -> b) -> b -> RTree a -> b
foldrRangeWithKey' (Predicate nodePred leafPred) f = go
  where
    {-# INLINE node #-}
    node z bx x
      | nodePred bx = go z x
      | otherwise   = z

    {-# INLINE leaf #-}
    leaf bx x !z
      | leafPred bx = f bx x z
      | otherwise   = z

    go z n =
      case n of
        Node2 ba a bb b           -> node (node             z             bb b) ba a
        Node3 ba a bb b bc c      -> node (node (node       z       bc c) bb b) ba a
        Node4 ba a bb b bc c bd d -> node (node (node (node z bd d) bc c) bb b) ba a

        Leaf2 ba a bb b           -> leaf ba a (leaf bb b                       z)
        Leaf3 ba a bb b bc c      -> leaf ba a (leaf bb b (leaf bc c            z))
        Leaf4 ba a bb b bc c bd d -> leaf ba a (leaf bb b (leaf bc c (leaf bd d z)))

        Leaf1 ba a                -> leaf ba a z
        Empty                     -> z



-- | \(\mathcal{O}(n_M)\).
--   Map each value to a monoid and combine the results.
foldMap :: Monoid m => (a -> m) -> RTree a -> m
foldMap f = go
  where
    go n =
      case n of
        Node2 _  a _  b           -> go a <> go b
        Node3 _  a _  b _  c      -> go a <> go b <> go c
        Node4 _  a _  b _  c _  d -> go a <> go b <> go c <> go d

        Leaf2 _  a _  b           -> f a <> f b
        Leaf3 _  a _  b _  c      -> f a <> f b <> f c
        Leaf4 _  a _  b _  c _  d -> f a <> f b <> f c <> f d

        Leaf1 _ a                 -> f a
        Empty                     -> mempty


-- | \(\mathcal{O}(n_M)\).
--   Map each t'MBR' and its respective value to a monoid and combine the results.
foldMapWithKey :: Monoid m => (MBR -> a -> m) -> RTree a -> m
foldMapWithKey f = go
  where
    go n =
      case n of
        Node2 _  a _  b           -> go a <> go b
        Node3 _  a _  b _  c      -> go a <> go b <> go c
        Node4 _  a _  b _  c _  d -> go a <> go b <> go c <> go d

        Leaf2 ba a bb b           -> f ba a <> f bb b
        Leaf3 ba a bb b bc c      -> f ba a <> f bb b <> f bc c
        Leaf4 ba a bb b bc c bd d -> f ba a <> f bb b <> f bc c <> f bd d

        Leaf1 ba a                -> f ba a
        Empty                     -> mempty


{-# INLINE foldMapRangeWithKey #-}
-- | \(\mathcal{O}(r + n_{{range}_M})\).
--   Map each t'MBR' that matches the 'Predicate' and its respective value to a monoid
--   and combine the results.
foldMapRangeWithKey :: Monoid m => Predicate -> (MBR -> a -> m) -> RTree a -> m
foldMapRangeWithKey (Predicate nodePred leafPred) f = go
  where
    {-# INLINE node #-}
    node bx x
      | nodePred bx = go x
      | otherwise   = mempty

    {-# INLINE leaf #-}
    leaf bx x
      | leafPred bx = f bx x
      | otherwise   = mempty

    go n =
      case n of
        Node2 ba a bb b           -> node ba a <> node bb b
        Node3 ba a bb b bc c      -> node ba a <> node bb b <> node bc c
        Node4 ba a bb b bc c bd d -> node ba a <> node bb b <> node bc c <> node bd d

        Leaf2 ba a bb b           -> leaf ba a <> leaf bb b
        Leaf3 ba a bb b bc c      -> leaf ba a <> leaf bb b <> leaf bc c
        Leaf4 ba a bb b bc c bd d -> leaf ba a <> leaf bb b <> leaf bc c <> leaf bd d

        Leaf1 ba a                -> leaf ba a
        Empty                     -> mempty



{-# INLINE traverse #-}
-- | \(\mathcal{O}(n)\).
--   Map each value to an action, evaluate the actions left-to-right and
--   collect the results.
traverse :: Applicative f => (a -> f b) -> RTree a -> f (RTree b)
traverse f = go
  where
    go n =
      case n of
        Node2 ba a bb b           ->
          liftA2 (\a' b' -> Node2 ba a' bb b')
            (go a) (go b)

        Node3 ba a bb b bc c      ->
          liftA2 (\a' b' c' -> Node3 ba a' bb b' bc c')
            (go a) (go b) <*> go c

        Node4 ba a bb b bc c bd d ->
          liftA2 (\a' b' c' d' -> Node4 ba a' bb b' bc c' bd d')
            (go a) (go b) <*> go c <*> go d

        Leaf2 ba a bb b           ->
          liftA2 (\a' b' -> Leaf2 ba a' bb b')
            (f a) (f b)

        Leaf3 ba a bb b bc c      ->
          liftA2 (\a' b' c' -> Leaf3 ba a' bb b' bc c')
            (f a) (f b) <*> f c

        Leaf4 ba a bb b bc c bd d ->
          liftA2 (\a' b' c' d' -> Leaf4 ba a' bb b' bc c' bd d')
            (f a) (f b) <*> f c <*> f d

        Leaf1 ba a                ->
          Leaf1 ba <$> f a

        Empty                     -> pure Empty


{-# INLINE traverseWithKey #-}
-- | \(\mathcal{O}(n)\).
--   Map each t'MBR' and its respective value to an action,
--   evaluate the actions left-to-right and collect the results.
traverseWithKey :: Applicative f => (MBR -> a -> f b) -> RTree a -> f (RTree b)
traverseWithKey f = go
  where
    go n =
      case n of
        Node2 ba a bb b           ->
          liftA2 (\a' b' -> Node2 ba a' bb b')
            (go a) (go b)

        Node3 ba a bb b bc c      ->
          liftA2 (\a' b' c' -> Node3 ba a' bb b' bc c')
            (go a) (go b) <*> go c

        Node4 ba a bb b bc c bd d ->
          liftA2 (\a' b' c' d' -> Node4 ba a' bb b' bc c' bd d')
            (go a) (go b) <*> go c <*> go d

        Leaf2 ba a bb b           ->
          liftA2 (\a' b' -> Leaf2 ba a' bb b')
            (f ba a) (f bb b)

        Leaf3 ba a bb b bc c      ->
          liftA2 (\a' b' c' -> Leaf3 ba a' bb b' bc c')
            (f ba a) (f bb b) <*> f bc c

        Leaf4 ba a bb b bc c bd d ->
          liftA2 (\a' b' c' d' -> Leaf4 ba a' bb b' bc c' bd d')
            (f ba a) (f bb b) <*> f bc c <*> f bd d

        Leaf1 ba a                ->
          Leaf1 ba <$> f ba a

        Empty                     -> pure Empty


{-# INLINE traverseRangeWithKey #-}
-- | \(\mathcal{O}(r + n_{range})\).
--   Map each t'MBR' that matches the 'Predicate' and its respective value to an action,
--   evaluate the actions left-to-right and collect the results.
traverseRangeWithKey
  :: Applicative f => Predicate -> (MBR -> a -> f a) -> RTree a -> f (RTree a)
traverseRangeWithKey (Predicate nodePred leafPred) f = go
  where
    {-# INLINE node #-}
    node bx x
      | nodePred bx = go x
      | otherwise   = pure x

    {-# INLINE leaf #-}
    leaf bx x
      | leafPred bx = f bx x
      | otherwise   = pure x

    go n =
      case n of
        Node2 ba a bb b           ->
          liftA2 (\a' b' -> Node2 ba a' bb b')
            (node ba a) (node bb b)

        Node3 ba a bb b bc c      ->
          liftA2 (\a' b' c' -> Node3 ba a' bb b' bc c')
            (node ba a) (node bb b) <*> node bc c

        Node4 ba a bb b bc c bd d ->
          liftA2 (\a' b' c' d' -> Node4 ba a' bb b' bc c' bd d')
            (node ba a) (node bb b) <*> node bc c <*> node bd d

        Leaf2 ba a bb b           ->
          liftA2 (\a' b' -> Leaf2 ba a' bb b')
            (leaf ba a) (leaf bb b)

        Leaf3 ba a bb b bc c      ->
          liftA2 (\a' b' c' -> Leaf3 ba a' bb b' bc c')
            (leaf ba a) (leaf bb b) <*> leaf bc c

        Leaf4 ba a bb b bc c bd d ->
          liftA2 (\a' b' c' d' -> Leaf4 ba a' bb b' bc c' bd d')
            (leaf ba a) (leaf bb b) <*> leaf bc c <*> leaf bd d

        Leaf1 ba a                ->
          Leaf1 ba <$> leaf ba a

        Empty                     -> pure Empty



{-# INLINE union3MBR #-}
union3MBR :: MBR -> MBR -> MBR -> MBR
union3MBR ba bb bc = unionMBR (unionMBR ba bb) bc

{-# INLINE union4MBR #-}
union4MBR :: MBR -> MBR -> MBR -> MBR -> MBR
union4MBR ba bb bc bd = unionMBR (unionMBR ba bb) (unionMBR bc bd)



data Gut a = GutOne MBR (RTree a)
           | GutTwo MBR (RTree a) MBR (RTree a)

-- | \(\mathcal{O}(\log n)\). Insert a value into the tree.
--
--   'insertGut' uses the R-tree insertion algorithm with quadratic-cost splits.
--   Compared to 'insert' the resulting trees are of lower quality (see the
--   [Wikipedia article](https://en.wikipedia.org/w/index.php?title=R*-tree&oldid=1171720351#Performance)
--   for a graphic example).
insertGut :: MBR -> a -> RTree a -> RTree a
insertGut bx x t =
  case insertGutRoot bx x t of
    GutOne _ o       -> o
    GutTwo bl l br r -> Node2 bl l br r


insertGutRoot :: MBR -> a -> RTree a -> Gut a
insertGutRoot bx x n =
  case n of
    Node2 ba a bb b           ->
      let (be, e, !bz, !z) = leastEnlargement2 bx ba a bb b
      in case insertGut_ bx x be e of
           GutOne bo o ->
             GutOne (unionMBR bo bz) (Node2 bo o bz z)

           GutTwo bl l br r ->
             GutOne (union3MBR bl br bz) (Node3 bl l br r bz z)

    Node3 ba a bb b bc c      ->
      let (be, e, !by, !y, !bz, !z) = leastEnlargement3 bx ba a bb b bc c
      in case insertGut_ bx x be e of
           GutOne bo o ->
             GutOne (union3MBR bo by bz) (Node3 bo o by y bz z)

           GutTwo bl l br r  ->
             GutOne (union4MBR bl br by bz) (Node4 bl l br r by y bz z)

    Node4 ba a bb b bc c bd d ->
      let (be, e, !bw, !w, !by, !y, !bz, !z) = leastEnlargement4 bx ba a bb b bc c bd d
      in case insertGut_ bx x be e of
           GutOne bo o ->
             GutOne (union4MBR bo bw by bz) (Node4 bo o bw w by y bz z)

           GutTwo bl l br r ->
             case quadSplit bl l br r bw w by y bz z of
               Q3L (L3 bl' bm m bo o bp p) (L2 br' bq q bs s) ->
                 GutTwo bl' (Node3 bm m bo o bp p) br' (Node2 bq q bs s)

               Q3R (L2 bl' bm m bo o) (L3 br' bp p bq q bs s) ->
                 GutTwo bl' (Node2 bm m bo o) br' (Node3 bp p bq q bs s)

    Leaf2 ba a bb b           ->
      GutOne (union3MBR ba bb bx) (Leaf3 ba a bb b bx x)

    Leaf3 ba a bb b bc c      ->
      GutOne (union4MBR ba bb bc bx) (Leaf4 ba a bb b bc c bx x)

    Leaf4 ba a bb b bc c bd d ->
      case quadSplit ba a bb b bc c bd d bx x of
        Q3L (L3 bl' bm m bo o bp p) (L2 br' bq q bs s) ->
          GutTwo bl' (Leaf3 bm m bo o bp p) br' (Leaf2 bq q bs s)

        Q3R (L2 bl' bm m bo o) (L3 br' bp p bq q bs s) ->
          GutTwo bl' (Leaf2 bm m bo o) br' (Leaf3 bp p bq q bs s)

    Leaf1 ba a                ->
      GutOne (unionMBR ba bx) (Leaf2 ba a bx x)

    Empty                     ->
      GutOne bx (Leaf1 bx x)


insertGut_ :: MBR -> a -> MBR -> RTree a -> Gut a
insertGut_ bx x = go
  where
    go bn n =
     case n of
       Node2 ba a bb b           ->
         let (be, e, !bz, !z) = leastEnlargement2 bx ba a bb b
         in case go be e of
              GutOne bo o ->
                GutOne (unionMBR bo bz) (Node2 bo o bz z)

              GutTwo bl l br r ->
                GutOne (union3MBR bl br bz) (Node3 bl l br r bz z)

       Node3 ba a bb b bc c      ->
         let (be, e, !by, !y, !bz, !z) = leastEnlargement3 bx ba a bb b bc c
         in case go be e of
              GutOne bo o ->
                GutOne (union3MBR bo by bz) (Node3 bo o by y bz z)

              GutTwo bl l br r  ->
                GutOne (union4MBR bl br by bz) (Node4 bl l br r by y bz z)

       Node4 ba a bb b bc c bd d ->
         let (be, e, !bw, !w, !by, !y, !bz, !z) = leastEnlargement4 bx ba a bb b bc c bd d
         in case go be e of
              GutOne bo o ->
                GutOne (union4MBR bo bw by bz) (Node4 bo o bw w by y bz z)

              GutTwo bl l br r ->
                case quadSplit bl l br r bw w by y bz z of
                  Q3L (L3 bl' bm m bo o bp p) (L2 br' bq q bs s) ->
                    GutTwo bl' (Node3 bm m bo o bp p) br' (Node2 bq q bs s)

                  Q3R (L2 bl' bm m bo o) (L3 br' bp p bq q bs s) ->
                    GutTwo bl' (Node2 bm m bo o) br' (Node3 bp p bq q bs s)

       Leaf2 ba a bb b           ->
         GutOne (unionMBR bn bx) (Leaf3 ba a bb b bx x)

       Leaf3 ba a bb b bc c      ->
         GutOne (unionMBR bn bx) (Leaf4 ba a bb b bc c bx x)

       Leaf4 ba a bb b bc c bd d ->
         case quadSplit ba a bb b bc c bd d bx x of
           Q3L (L3 bl' bm m bo o bp p) (L2 br' bq q bs s) ->
             GutTwo bl' (Leaf3 bm m bo o bp p) br' (Leaf2 bq q bs s)

           Q3R (L2 bl' bm m bo o) (L3 br' bp p bq q bs s) ->
             GutTwo bl' (Leaf2 bm m bo o) br' (Leaf3 bp p bq q bs s)

       Leaf1 ba a                ->
         GutOne (unionMBR ba bn) (Leaf2 ba a bx x)

       Empty                     ->
         GutOne bn (Leaf1 bx x)



insertGutRootNode :: MBR -> RTree a -> Int -> RTree a -> Gut a
insertGutRootNode bx x depth n =
  case n of
    Node2 ba a bb b
      | depth <= 0 ->
          GutOne (union3MBR ba bb bx) (Node3 ba a bb b bx x)

      | otherwise ->
          let (be, e, !bz, !z) = leastEnlargement2 bx ba a bb b
          in case insertGutNode bx x (depth - 1) be e of
               GutOne bo o ->
                 GutOne (unionMBR bo bz) (Node2 bo o bz z)

               GutTwo bl l br r ->
                 GutOne (union3MBR bl br bz) (Node3 bl l br r bz z)

    Node3 ba a bb b bc c
      | depth <= 0 ->
          GutOne (union4MBR ba bb bc bx) (Node4 ba a bb b bc c bx x)

      | otherwise ->
          let (be, e, !by, !y, !bz, !z) = leastEnlargement3 bx ba a bb b bc c
          in case insertGutNode bx x (depth - 1) be e of
               GutOne bo o ->
                 GutOne (union3MBR bo by bz) (Node3 bo o by y bz z)

               GutTwo bl l br r  ->
                 GutOne (union4MBR bl br by bz) (Node4 bl l br r by y bz z)

    Node4 ba a bb b bc c bd d
      | depth <= 0 ->
          case quadSplit ba a bb b bc c bd d bx x of
            Q3L (L3 bl' bm m bo o bp p) (L2 br' bq q bs s) ->
              GutTwo bl' (Node3 bm m bo o bp p) br' (Node2 bq q bs s)

            Q3R (L2 bl' bm m bo o) (L3 br' bp p bq q bs s) ->
              GutTwo bl' (Node2 bm m bo o) br' (Node3 bp p bq q bs s)

      | otherwise ->
          let (be, e, !bw, !w, !by, !y, !bz, !z) = leastEnlargement4 bx ba a bb b bc c bd d
          in case insertGutNode bx x (depth - 1) be e of
               GutOne bo o ->
                 GutOne (union4MBR bo bw by bz) (Node4 bo o bw w by y bz z)

               GutTwo bl l br r ->
                 case quadSplit bl l br r bw w by y bz z of
                   Q3L (L3 bl' bm m bo o bp p) (L2 br' bq q bs s) ->
                     GutTwo bl' (Node3 bm m bo o bp p) br' (Node2 bq q bs s)

                   Q3R (L2 bl' bm m bo o) (L3 br' bp p bq q bs s) ->
                     GutTwo bl' (Node2 bm m bo o) br' (Node3 bp p bq q bs s)

    _ -> assert False
           (errorWithoutStackTrace "Data.RTree.D2.Float.Internal.insertGutRootNode: reached a leaf")
                 n

{-# INLINE insertGutNode #-}
insertGutNode :: MBR -> RTree a -> Int -> MBR -> RTree a -> Gut a
insertGutNode bx x = go
  where
    go depth bn n =
      case n of
        Node2 ba a bb b
          | depth <= 0 ->
              GutOne (unionMBR bn bx) (Node3 ba a bb b bx x)

          | otherwise ->
              let (be, e, !bz, !z) = leastEnlargement2 bx ba a bb b
              in case go (depth - 1) be e of
                   GutOne bo o ->
                     GutOne (unionMBR bo bz) (Node2 bo o bz z)

                   GutTwo bl l br r ->
                     GutOne (union3MBR bl br bz) (Node3 bl l br r bz z)

        Node3 ba a bb b bc c
          | depth <= 0 ->
              GutOne (unionMBR bn bx) (Node4 ba a bb b bc c bx x)

          | otherwise ->
              let (be, e, !by, !y, !bz, !z) = leastEnlargement3 bx ba a bb b bc c
              in case go (depth - 1) be e of
                   GutOne bo o ->
                     GutOne (union3MBR bo by bz) (Node3 bo o by y bz z)

                   GutTwo bl l br r  ->
                     GutOne (union4MBR bl br by bz) (Node4 bl l br r by y bz z)

        Node4 ba a bb b bc c bd d
          | depth <= 0 ->
              case quadSplit ba a bb b bc c bd d bx x of
                Q3L (L3 bl' bm m bo o bp p) (L2 br' bq q bs s) ->
                  GutTwo bl' (Node3 bm m bo o bp p) br' (Node2 bq q bs s)

                Q3R (L2 bl' bm m bo o) (L3 br' bp p bq q bs s) ->
                  GutTwo bl' (Node2 bm m bo o) br' (Node3 bp p bq q bs s)

          | otherwise ->
              let (be, e, !bw, !w, !by, !y, !bz, !z) = leastEnlargement4 bx ba a bb b bc c bd d
              in case go (depth - 1) be e of
                   GutOne bo o ->
                     GutOne (union4MBR bo bw by bz) (Node4 bo o bw w by y bz z)

                   GutTwo bl l br r ->
                     case quadSplit bl l br r bw w by y bz z of
                       Q3L (L3 bl' bm m bo o bp p) (L2 br' bq q bs s) ->
                         GutTwo bl' (Node3 bm m bo o bp p) br' (Node2 bq q bs s)

                       Q3R (L2 bl' bm m bo o) (L3 br' bp p bq q bs s) ->
                         GutTwo bl' (Node2 bm m bo o) br' (Node3 bp p bq q bs s)

        _ -> assert False
               (errorWithoutStackTrace "Data.RTree.D2.Float.Internal.insertGutNode: reached a leaf")
               n



{-# INLINE enlargement #-}
-- as in (adding A to B)
enlargement :: MBR -> MBR -> Float
enlargement bx ba = areaMBR (unionMBR ba bx) - areaMBR ba

{-# INLINE leastEnlargement2 #-}
leastEnlargement2 :: MBR -> MBR -> a -> MBR -> a -> (MBR, a, MBR, a)
leastEnlargement2 bx ba a bb b =
  let aw = (ba, a, bb, b)
      bw = (bb, b, ba, a)

  in case enlargement bx ba `compare` enlargement bx bb of
       GT -> bw
       LT -> aw
       EQ | areaMBR ba <= areaMBR bb -> aw
          | otherwise                -> bw

{-# INLINE leastEnlargement3 #-}
leastEnlargement3 :: MBR -> MBR -> a -> MBR -> a -> MBR -> a -> (MBR, a, MBR, a, MBR, a)
leastEnlargement3 bx ba a bb b bc c =
  let aw = let (be, e, by, y) = leastEnlargement2 bx ba a bc c
           in (be, e, by, y, bb, b)

      bw = let (be, e, by, y) = leastEnlargement2 bx bb b bc c
           in (be, e, by, y, ba, a)

  in case enlargement bx ba `compare` enlargement bx bb of
       GT -> bw
       LT -> aw
       EQ | areaMBR ba <= areaMBR bb -> aw
          | otherwise                -> bw

{-# INLINE leastEnlargement4 #-}
leastEnlargement4
  :: MBR -> MBR -> a -> MBR -> a -> MBR -> a -> MBR -> a
  -> (MBR, a, MBR, a, MBR, a, MBR, a)
leastEnlargement4 bx ba a bb b bc c bd d =
  let (be, e, bn, n) = leastEnlargement2 bx ba a bb b
      (bf, f, bo, o) = leastEnlargement2 bx bc c bd d
      (bg, g, bp, p) = leastEnlargement2 bx be e bf f

  in (bg, g, bn, n, bo, o, bp, p)



data L2 a = L2 MBR MBR a MBR a

data L3 a = L3 MBR MBR a MBR a MBR a

data Q1 a = Q1L (L2 a) MBR a
          | Q1R MBR a (L2 a)

data Q2 a = Q2L (L3 a) MBR a
          | Q2M (L2 a) (L2 a)
          | Q2R MBR a (L3 a)

data Q3 a = Q3L (L3 a) (L2 a)
          | Q3R (L2 a) (L3 a)



{-# NOINLINE quadSplit #-}
quadSplit :: MBR -> a -> MBR -> a -> MBR -> a -> MBR -> a -> MBR -> a -> Q3 a
quadSplit ba a bb b bc c bd d be e =
  let (bl, l, br, r, bx, x, by, y, bz, z) = pickSeeds ba a bb b bc c bd d be e
      (q1, bv, v, bw, w) = distribute3 bl l br r bx x by y bz z
      (q2, bu, u) = distribute2 q1 bv v bw w

  in distribute1 q2 bu u



{-# INLINE pickSeeds #-}
pickSeeds
  :: MBR -> a -> MBR -> a -> MBR -> a -> MBR -> a -> MBR -> a
  -> (MBR, a, MBR, a, MBR, a, MBR, a, MBR, a)
pickSeeds ba a bb b bc c bd d be e =
  let waste bx by = areaMBR (unionMBR bx by) - areaMBR bx - areaMBR by

      align x@( bw, _, bx, _, _, _, _, _, _, _ )
            y@( by, _, bz, _, _, _, _, _, _, _ )
        | waste bw bx > waste by bz = x
        | otherwise                 = y

  in align ( ba, a, bb, b, bc, c, bd, d, be, e )
   . align ( ba, a, bc, c, bb, b, bd, d, be, e )
   . align ( ba, a, bd, d, bb, b, bc, c, be, e )
   . align ( ba, a, be, e, bb, b, bc, c, bd, d )
   . align ( bb, b, bc, c, ba, a, bd, d, be, e )
   . align ( bb, b, bd, d, ba, a, bc, c, be, e )
   . align ( bb, b, be, e, ba, a, bc, c, bd, d )
   . align ( bc, c, bd, d, ba, a, bb, b, be, e )
   $ align ( bc, c, be, e, ba, a, bb, b, bd, d )
           ( bd, d, be, e, ba, a, bb, b, bc, c )



{-# INLINE distribute3 #-}
distribute3
  :: MBR -> a -> MBR -> a -> MBR -> a -> MBR -> a -> MBR -> a -> (Q1 a, MBR, a, MBR, a)
distribute3 bl l br r bx x by y bz z =
  let delta ba = abs (enlargement ba bl - enlargement ba br)

      (be, !e, !bu, !u, !bv, !v) = if delta bx >= delta by
                                     then if delta bx >= delta bz
                                            then (bx, x, by, y, bz, z)
                                            else (bz, z, bx, x, by, y)

                                     else if delta by >= delta bz
                                            then (by, y, bx, x, bz, z)
                                            else (bz, z, bx, x, by, y)

      lw = Q1L (L2 (unionMBR bl be) bl l be e) br r

      rw = Q1R bl l (L2 (unionMBR br be) br r be e)

  in ( case enlargement be bl `compare` enlargement be br of
          GT -> rw
          LT -> lw
          EQ | areaMBR bl < areaMBR br -> lw
             | otherwise               -> rw
     , bu
     , u
     , bv
     , v
     )



{-# INLINE distribute2 #-}
distribute2 :: Q1 a -> MBR -> a -> MBR -> a -> (Q2 a, MBR, a)
distribute2 q bx x by y =
  let delta bl br bd = abs (enlargement bd bl - enlargement bd br)
  in case q of
       Q1L l@(L2 bl ba a bb b) br r ->
         let (be, !e, !bz, !z) | delta bl br bx >= delta bl br by = (bx, x, by, y)
                               | otherwise                        = (by, y, bx, x)

             lw = Q2L (L3 (unionMBR bl be) ba a bb b be e) br r

             rw = Q2M l (L2 (unionMBR br be) br r be e)

         in ( case enlargement be bl `compare` enlargement be br of
                 GT -> rw
                 LT -> lw
                 EQ | areaMBR bl <= areaMBR br -> lw
                    | otherwise                -> rw
             , bz
             , z
             )

       Q1R bl l r@(L2 br ba a bb b) ->
         let (be, !e, !bz, !z) | delta bl br bx >= delta bl br by = (bx, x, by, y)
                               | otherwise                        = (by, y, bx, x)

             lw = Q2M (L2 (unionMBR bl be) bl l be e) r

             rw = Q2R bl l (L3 (unionMBR br be) ba a bb b be e)

         in ( case enlargement be bl `compare` enlargement be br of
                 GT -> rw
                 LT -> lw
                 EQ | areaMBR bl <= areaMBR br -> lw
                    | otherwise                -> rw
             , bz
             , z
             )


{-# INLINE distribute1 #-}
distribute1 :: Q2 a -> MBR -> a -> Q3 a
distribute1 q bx x =
  case q of
    Q2M l@(L2 bl ba a bb b) r@(L2 br bc c bd d) ->
      let lw = Q3L (L3 (unionMBR bl bx) ba a bb b bx x) r

          rw = Q3R l (L3 (unionMBR br bx) bc c bd d bx x)

      in case enlargement bx bl `compare` enlargement bx br of
           GT -> rw
           LT -> lw
           EQ | areaMBR bl <= areaMBR br -> lw
              | otherwise                -> rw

    Q2L l br r -> Q3L l (L2 (unionMBR br bx) br r bx x)

    Q2R bl l r -> Q3R (L2 (unionMBR bl bx) bl l bx x) r



data Carry a = CarryLeaf MBR a
             | CarryNode Int MBR (RTree a)

data Ins a = InsOne MBR (RTree a)
           | InsCarry Word (Carry a) MBR (RTree a)
           | InsTwo Word MBR (RTree a) MBR (RTree a)

-- | \(\mathcal{O}(\log n)\). Insert a value into the tree.
--
--   'insert' uses the R*-tree insertion algorithm.
insert :: MBR -> a -> RTree a -> RTree a
insert bx x n =
  case n of
    Node2 ba a bb b           ->
      let add f bg g bh h =
            let (be, e, !bz, !z) = leastEnlargement2 bx bg g bh h
            in case f be e of
                 InsOne bo o              -> Node2 bo o bz z
                 InsCarry mask carry bo o ->
                   case carry of
                     CarryLeaf bu u       ->
                       add (insert_ mask bu u 0) bo o bz z

                     CarryNode depth bu u ->
                       add (insertNode mask depth bu u 0) bo o bz z

                 InsTwo _ bl l br r               -> Node3 bl l br r bz z

      in add (insert_ 0 bx x 0) ba a bb b

    Node3 ba a bb b bc c      ->
      let add f bg g bh h bi i =
            let (be, e, !by, !y, !bz, !z) = leastEnlargement3 bx bg g bh h bi i
            in case f be e of
                 InsOne bo o              -> Node3 bo o by y bz z
                 InsCarry mask carry bo o ->
                   case carry of
                     CarryLeaf bu u       ->
                       add (insert_ mask bu u 0) bo o by y bz z

                     CarryNode depth bu u ->
                       add (insertNode mask depth bu u 0) bo o by y bz z

                 InsTwo _ bl l br r               -> Node4 bl l br r by y bz z

      in add (insert_ 0 bx x 0) ba a bb b bc c

    Node4 ba a bb b bc c bd d ->
      let add f bg g bh h bi i bj j =
            let (be, e, !bw, !w, !by, !y, !bz, !z) = leastEnlargement4 bx bg g bh h bi i bj j
            in case f be e of
                 InsOne bo o              -> Node4 bo o bw w by y bz z
                 InsCarry mask carry bo o ->
                   case carry of
                     CarryLeaf bu u       ->
                       add (insert_ mask bu u 0) bo o bw w by y bz z

                     CarryNode depth bu u ->
                       add (insertNode mask depth bu u 0) bo o bw w by y bz z

                 InsTwo _ bl l br r               ->
                   case sortSplit bl l br r bw w by y bz z of
                     Q3L (L3 bl' bm m bo o bp p) (L2 br' bs s bt t) ->
                       Node2 bl' (Node3 bm m bo o bp p) br' (Node2 bs s bt t)

                     Q3R (L2 bl' bm m bo o) (L3 br' bp p bs s bt t) ->
                       Node2 bl' (Node2 bm m bo o) br' (Node3 bp p bs s bt t)

      in add (insert_ 0 bx x 0) ba a bb b bc c bd d

    Leaf2 ba a bb b           -> Leaf3 ba a bb b bx x
    Leaf3 ba a bb b bc c      -> Leaf4 ba a bb b bc c bx x
    Leaf4 ba a bb b bc c bd d ->
      case sortSplit ba a bb b bc c bd d bx x of
        Q3L (L3 bl bu u bv v bw w) (L2 br by y bz z) ->
          Node2 bl (Leaf3 bu u bv v bw w) br (Leaf2 by y bz z)

        Q3R (L2 bl bu u bv v) (L3 br bw w by y bz z) ->
          Node2 bl (Leaf2 bu u bv v) br (Leaf3 bw w by y bz z)

    Leaf1 ba a                -> Leaf2 ba a bx x
    Empty                     -> Leaf1 bx x



insert_ :: Word -> MBR -> a -> Int -> MBR -> RTree a -> Ins a
insert_ mask bx x = go
  where
    go height bn n =
      case n of
        Node2 ba a bb b           ->
          let (be, e, !bz, !z) = leastEnlargement2 bx ba a bb b
          in case go (height + 1) be e of
               InsOne bo o               -> InsOne (unionMBR bo bz) (Node2 bo o bz z)
               InsCarry mask' carry bo o ->
                 InsCarry mask' carry (unionMBR bo bz) (Node2 bo o bz z)

               InsTwo _ bl l br r        ->
                 InsOne (union3MBR bl br bz) (Node3 bl l br r bz z)

        Node3 ba a bb b bc c      ->
          let (be, e, !by, !y, !bz, !z) = leastEnlargement3 bx ba a bb b bc c
          in case go (height + 1) be e of
               InsOne bo o               ->
                 InsOne (union3MBR bo by bz) (Node3 bo o by y bz z)

               InsCarry mask' carry bo o ->
                 InsCarry mask' carry (union3MBR bo by bz) (Node3 bo o by y bz z)

               InsTwo _ bl l br r        ->
                 InsOne (union4MBR bl br by bz) (Node4 bl l br r by y bz z)

        Node4 ba a bb b bc c bd d ->
          let (be, e, !bw, !w, !by, !y, !bz, !z) = leastEnlargement4 bx ba a bb b bc c bd d
          in case go (height + 1) be e of
               InsOne bo o               ->
                 InsOne (union4MBR bo bw by bz) (Node4 bo o bw w by y bz z)

               InsCarry mask' carry bo o ->
                 InsCarry mask' carry (union4MBR bo bw by bz) (Node4 bo o bw w by y bz z)

               InsTwo _ bl l br r        ->
                 let bit_ = 1 `unsafeShiftL` height
                 in case mask .&. bit_ of
                      0 ->
                        case sortSplit bl l br r bw w by y bz z of
                          Q3L (L3 bl' bm m bo o bp p) (L2 br' bs s bt t) ->
                            InsTwo mask bl' (Node3 bm m bo o bp p) br' (Node2 bs s bt t)

                          Q3R (L2 bl' bm m bo o) (L3 br' bp p bs s bt t) ->
                            InsTwo mask bl' (Node2 bm m bo o) br' (Node3 bp p bs s bt t)

                      _ ->
                        let (bm, m, bo, o, bp, p, bs, s, bt, t ) =
                               sort5 (distance (unionMBR bn bx)) bl l br r bw w by y bz z

                        in InsCarry (mask .|. bit_) (CarryNode height bt t)
                             (union4MBR bm bo bp bs) (Node4 bm m bo o bp p bs s)

        Leaf2 ba a bb b           ->
          InsOne (union3MBR ba bb bx) (Leaf3 ba a bb b bx x)

        Leaf3 ba a bb b bc c      ->
          InsOne (union4MBR ba bb bc bx) (Leaf4 ba a bb b bc c bx x)

        Leaf4 ba a bb b bc c bd d ->
          let bit_ = 1 `unsafeShiftL` height
          in case mask .&. bit_ of
               0 ->
                 case sortSplit ba a bb b bc c bd d bx x of
                   Q3L (L3 bl bu u bv v bw w) (L2 br by y bz z) ->
                     InsTwo mask bl (Leaf3 bu u bv v bw w) br (Leaf2 by y bz z)

                   Q3R (L2 bl bu u bv v) (L3 br bw w by y bz z) ->
                     InsTwo mask bl (Leaf2 bu u bv v) br (Leaf3 bw w by y bz z)

               _ ->
                 let (bu, u, bv, v, bw, w, by, y, bz, z) =
                        sort5 (distance (unionMBR bn bx)) ba a bb b bc c bd d bx x

                 in InsCarry (mask .|. bit_) (CarryLeaf bz z)
                      (union4MBR bu bv bw by) (Leaf4 bu u bv v bw w by y)

        Leaf1 ba a               ->
          InsOne (unionMBR ba bx) (Leaf2 ba a bx x)

        Empty                    ->
          InsOne bx (Leaf1 bx x)


insertNode :: Word -> Int -> MBR -> RTree a -> Int -> MBR -> RTree a -> Ins a
insertNode mask depth bx x = go
  where
    go height bn n =
      case n of
        Node2 ba a bb b
          | height >= depth ->
              let (be, e, !bz, !z) = leastEnlargement2 bx ba a bb b
              in case go (height + 1) be e of
                   InsOne bo o               -> InsOne (unionMBR bo bz) (Node2 bo o bz z)
                   InsCarry mask' carry bo o ->
                     InsCarry mask' carry (unionMBR bo bz) (Node2 bo o bz z)

                   InsTwo _ bl l br r        ->
                     InsOne (union3MBR bl br bz) (Node3 bl l br r bz z)

          | otherwise       ->
              InsOne (unionMBR bn bx) (Node3 ba a bb b bx x)

        Node3 ba a bb b bc c
          | height >= depth ->
              let (be, e, !by, !y, !bz, !z) = leastEnlargement3 bx ba a bb b bc c
              in case go (height + 1) be e of
                   InsOne bo o               ->
                     InsOne (union3MBR bo by bz) (Node3 bo o by y bz z)

                   InsCarry mask' carry bo o ->
                     InsCarry mask' carry (union3MBR bo by bz) (Node3 bo o by y bz z)

                   InsTwo _ bl l br r        ->
                     InsOne (union4MBR bl br by bz) (Node4 bl l br r by y bz z)

          | otherwise       ->
              InsOne (unionMBR bn bx) (Node4 ba a bb b bc c bx x)

        Node4 ba a bb b bc c bd d
          | height >= depth ->
              let (be, e, !bw, !w, !by, !y, !bz, !z) = leastEnlargement4 bx ba a bb b bc c bd d
              in case go (height + 1) be e of
                   InsOne bo o               ->
                     InsOne (union4MBR bo bw by bz) (Node4 bo o bw w by y bz z)

                   InsCarry mask' carry bo o ->
                     InsCarry mask' carry (union4MBR bo bw by bz) (Node4 bo o bw w by y bz z)

                   InsTwo _ bl l br r        ->
                     let bit_ = 1 `unsafeShiftL` height
                     in case mask .&. bit_ of
                          0 ->
                            case sortSplit bl l br r bw w by y bz z of
                              Q3L (L3 bl' bm m bo o bp p) (L2 br' bs s bt t) ->
                                InsTwo mask bl' (Node3 bm m bo o bp p) br' (Node2 bs s bt t)

                              Q3R (L2 bl' bm m bo o) (L3 br' bp p bs s bt t) ->
                                InsTwo mask bl' (Node2 bm m bo o) br' (Node3 bp p bs s bt t)

                          _ ->
                            let (bm, m, bo, o, bp, p, bs, s, bt, t) =
                                  sort5 (distance (unionMBR bn bx)) bl l br r bw w by y bz z

                            in InsCarry (mask .|. bit_) (CarryNode height bt t)
                                 (union4MBR bm bo bp bs) (Node4 bm m bo o bp p bs s)

          | otherwise       ->
              let bit_ = 1 `unsafeShiftL` height
              in case mask .&. bit_ of
                   0 ->
                     case sortSplit ba a bb b bc c bd d bx x of
                       Q3L (L3 bl' bm m bo o bp p) (L2 br' bs s bt t) ->
                         InsTwo mask bl' (Node3 bm m bo o bp p) br' (Node2 bs s bt t)

                       Q3R (L2 bl' bm m bo o) (L3 br' bp p bs s bt t) ->
                         InsTwo mask bl' (Node2 bm m bo o) br' (Node3 bp p bs s bt t)

                   _ ->
                     let (bm, m, bo, o, bp, p, bs, s, bt, t) =
                           sort5 (distance (unionMBR bn bx)) ba a bb b bc c bd d bx x

                     in InsCarry (mask .|. bit_) (CarryNode height bt t)
                          (union4MBR bm bo bp bs) (Node4 bm m bo o bp p bs s)



        _ -> assert False
               (errorWithoutStackTrace "Data.RTree.D2.Float.Internal.insertNode: reached a leaf")
               n



{-# NOINLINE sortSplit #-}
sortSplit :: MBR -> a -> MBR -> a -> MBR -> a -> MBR -> a -> MBR -> a -> Q3 a
sortSplit ba a bb b bc c bd d be e =
  let v = sort5 vertical   ba a bb b bc c bd d be e
      h = sort5 horizontal ba a bb b bc c bd d be e

      vg = group v
      hg = group h

      ( al@(L3 bu _ _ _ _ _ _), ar@(L2 bv _ _ _ _)
       , bl@(L2 bx _ _ _ _), br@(L3 by _ _ _ _ _ _) )
          | margins vg <= margins hg = vg
          | otherwise                = hg

      aw = Q3L al ar
      bw = Q3R bl br

  in case overlapMBR bu bv `compare` overlapMBR bx by of
       GT -> bw
       LT -> aw
       EQ | areaMBR bu + areaMBR bv <= areaMBR bx + areaMBR by -> aw
          | otherwise                                          -> bw



{-# INLINE horizontal #-}
horizontal :: MBR -> MBR -> Bool
horizontal (UnsafeMBR xmin _ xmax _) (UnsafeMBR xmin' _ xmax' _) =
  case xmin `compare` xmin' of
    GT -> False
    LT -> True
    EQ -> xmax <= xmax'

{-# INLINE vertical #-}
vertical :: MBR -> MBR -> Bool
vertical (UnsafeMBR _ ymin _ ymax) (UnsafeMBR _ ymin' _ ymax') =
  case ymin `compare` ymin' of
    GT -> False
    LT -> True
    EQ -> ymax <= ymax'

{-# INLINE distance #-}
distance :: MBR -> MBR -> MBR -> Bool
distance bx ba bb = distanceMBR bx ba <= distanceMBR bx bb

{-# INLINE sort5 #-}
sort5
  :: (k -> k -> Bool) -- as in (A is smaller than B)
  -> k -> a -> k -> a -> k -> a -> k -> a -> k -> a
  -> (k, a, k, a, k, a, k, a, k, a)
sort5 f ka a kb b kc c kd d ke e =
  let swap kx x ky y
        | f kx ky  = (kx, x, ky, y)
        | otherwise = (ky, y, kx, x)

      sort3 kw w kx x ky y kz z
        | f kw ky  =
            if f kw kx
              then (kw, w, kx, x, ky, y, kz, z)
              else (kx, x, kw, w, ky, y, kz, z)

        | otherwise =
            if f kw kz
              then (kx, x, ky, y, kw, w, kz, z)
              else (kx, x, ky, y, kz, z, kw, w)

      (ka1, a1, kb1, b1) = swap ka a kb b
      (kc1, c1, kd1, d1) = swap kc c kd d

      (ka2, (a2, kb2, b2), kc2, (c2, kd2, d2)) = swap ka1 (a1, kb1, b1) kc1 (c1, kd1, d1)

      (ka3, a3, kc3, c3, kd3, d3, ke3, e3) = sort3 ke e ka2 a2 kc2 c2 kd2 d2

      (kb4, b4, kc4, c4, kd4, d4, ke4, e4) = sort3 kb2 b2 kc3 c3 kd3 d3 ke3 e3

  in (ka3, a3, kb4, b4, kc4, c4, kd4, d4, ke4, e4)

{-# INLINE group #-}
group
  :: (MBR, a, MBR, a, MBR, a, MBR, a, MBR, a) -> (L3 a, L2 a, L2 a, L3 a)
group (ba, a, bb, b, bc, c, bd, d, be, e) =
  ( L3 (union3MBR ba bb bc) ba a bb b bc c, L2 (unionMBR bd be) bd d be e
   , L2 (unionMBR ba bb) ba a bb b, L3 (union3MBR bd be bc) bd d be e bc c )

{-# INLINE margins #-}
margins :: (L3 a, L2 a, L2 a, L3 a) -> Float
margins (L3 bw _ _ _ _ _ _, L2 bx _ _ _ _, L2 by _ _ _ _, L3 bz _ _ _ _ _ _) =
  marginMBR bw + marginMBR bx + marginMBR by + marginMBR bz



-- | \(\mathcal{O}(\log n)\).
--   Remove an entry stored under a given t'MBR', if one exists.
--   If multiple entries qualify, the leftmost one is removed.
--
--   'delete' uses the R-tree deletion algorithm with quadratic-cost splits.
delete :: MBR -> RTree a -> RTree a
delete bx s =
  case delete_ bx 0 s of
    DelOne _ o     -> o
    DelNone        -> s
    DelSome re _ o -> reintegrate 0 o re
    DelRe re       -> reconstruct re
  where
    reintegrate height n re =
      case re of
        ReCons depth ba a re' ->
          case insertGutRootNode ba a (depth + height) n of
            GutOne _ o       -> reintegrate height o re'
            GutTwo bl l br r -> reintegrate (height + 1) (Node2 bl l br r) re'

        ReLeaf ba a          ->
          case insertGutRoot ba a n of
            GutOne _ o       -> o
            GutTwo bl l br r -> Node2 bl l br r

    {-# INLINE reconstruct #-}
    reconstruct re =
      case re of
        ReCons _ _ n re' -> reintegrate (-1) n re'
        ReLeaf ba a      -> Leaf1 ba a

data Re a = ReCons Int MBR (RTree a) (Re a)
          | ReLeaf MBR a

data Del a = DelNone
           | DelOne MBR (RTree a)
           | DelSome (Re a) MBR (RTree a)
           | DelRe (Re a)

{-# INLINE delete_ #-}
delete_ :: MBR -> Int -> RTree a -> Del a
delete_ bx = go
  where
    {-# INLINE cut2 #-}
    cut2 depth next ba a bb b
      | containsMBR ba bx =
          case go (depth + 1) a of
            DelNone         -> next
            DelOne bo o     -> DelOne (unionMBR bo bb) (Node2 bo o bb b)
            DelSome re bo o -> DelSome re (unionMBR bo bb) (Node2 bo o bb b)
            DelRe re        -> DelRe (ReCons depth bb b re)

      | otherwise         = next

    {-# INLINE cut3 #-}
    cut3 depth next ba a bb b bc c
      | containsMBR ba bx =
          case go (depth + 1) a of
            DelNone         -> next
            DelOne bo o     -> DelOne (union3MBR bo bb bc) (Node3 bo o bb b bc c)
            DelSome re bo o -> DelSome re (union3MBR bo bb bc) (Node3 bo o bb b bc c)
            DelRe re        -> DelSome re (unionMBR bb bc) (Node2 bb b bc c)

      | otherwise         = next

    {-# INLINE cut4 #-}
    cut4 depth next ba a bb b bc c bd d
      | containsMBR ba bx =
          case go (depth + 1) a of
            DelNone         -> next
            DelOne bo o     -> DelOne (union4MBR bo bb bc bd) (Node4 bo o bb b bc c bd d)
            DelSome re bo o -> DelSome re (union4MBR bo bb bc bd) (Node4 bo o bb b bc c bd d)
            DelRe re        -> DelSome re (union3MBR bb bc bd) (Node3 bb b bc c bd d)

      | otherwise         = next

    {-# INLINE edge2 #-}
    edge2 next ba bb b
      | eqMBR ba bx = DelRe (ReLeaf bb b)
      | otherwise   = next

    {-# INLINE edge3 #-}
    edge3 next ba bb b bc c
      | eqMBR ba bx = DelOne (unionMBR bb bc) (Leaf2 bb b bc c)
      | otherwise   = next

    {-# INLINE edge4 #-}
    edge4 next ba bb b bc c bd d
      | eqMBR ba bx = DelOne (union3MBR bb bc bd) (Leaf3 bb b bc c bd d)
      | otherwise   = next

    go depth n =
      case n of
        Node2 ba a bb b ->
          let dela = cut2 depth delb    ba a bb b
              delb = cut2 depth DelNone bb b ba a

          in dela

        Node3 ba a bb b bc c ->
          let dela = cut3 depth delb    ba a bb b bc c
              delb = cut3 depth delc    bb b ba a bc c
              delc = cut3 depth DelNone bc c ba a bb b

          in dela

        Node4 ba a bb b bc c bd d ->
          let dela = cut4 depth delb    ba a bb b bc c bd d
              delb = cut4 depth delc    bb b ba a bc c bd d
              delc = cut4 depth deld    bc c ba a bb b bd d
              deld = cut4 depth DelNone bd d ba a bb b bc c

          in dela

        Leaf2 ba a bb b ->
          let dela = edge2 delb    ba bb b
              delb = edge2 DelNone bb ba a

          in dela

        Leaf3 ba a bb b bc c ->
          let dela = edge3 delb    ba bb b bc c
              delb = edge3 delc    bb ba a bc c
              delc = edge3 DelNone bc ba a bb b

          in dela

        Leaf4 ba a bb b bc c bd d ->
          let dela = edge4 delb    ba bb b bc c bd d
              delb = edge4 delc    bb ba a bc c bd d
              delc = edge4 deld    bc ba a bb b bd d
              deld = edge4 DelNone bd ba a bb b bc c

          in dela

        Leaf1 ba _ | eqMBR bx ba -> DelOne ba Empty
                   | otherwise   -> DelNone

        Empty      -> DelNone




quotCeil :: Int -> Int -> Int
quotCeil i d = let ~(p, q) = quotRem i d
               in p + case q of
                        0 -> 0
                        _ -> 1

slices :: Int -> Int
slices r = ceiling (sqrt (fromIntegral (quotCeil r 4)) :: Float)

partition1 :: Int -> [a] -> [(Int, [a])]
partition1 n_ = go
  where
    go xs =
      let ~(n, before, after) = splitAt1 0 xs
      in (n, before) : case after of
                         _:_ -> go after
                         []  -> []

    splitAt1 n xs =
      case xs of
        []   -> (n, [], [])
        x:ys
          | n < n_    -> let ~(m, as, bs) = splitAt1 (n + 1) ys
                         in (m, x:as, bs)

          | [] <- ys  -> (n + 1, xs, [])
          | otherwise -> (n    , [], xs)



-- | \(\mathcal{O}(n \log n)\). Bulk-load a tree.
--
--   'bulkSTR' uses the Sort-Tile-Recursive algorithm.
bulkSTR :: [(MBR, a)] -> RTree a
bulkSTR xs =
  case xs of
    _:_:_     -> snd $ vertically (length xs) xs
    [(ba, a)] -> Leaf1 ba a
    []        -> Empty
  where
    horiCenter (UnsafeMBR xmin _ xmax _, _) = xmin + xmax

    vertCenter (UnsafeMBR _ ymin _ ymax, _) = ymin + ymax

    horizontally r as =
      let s = slices r
      in if s <= 1
           then base as
           else compress .
                  fmap (uncurry vertically) $
                    partition1 (r `quotCeil` s) (List.sortBy (compare `on` vertCenter) as)


    vertically r as =
      let s = slices r
      in if s <= 1
           then base as
           else compress .
                  fmap (uncurry horizontally) $
                    partition1 (r `quotCeil` s) (List.sortBy (compare `on` horiCenter) as)

    compress (x : ys) = go (x :| ys)
      where
        go (a :| bs) =
          case bs of
            []   -> a
            b:cs -> go (mend a b cs)

    compress [] =
      errorWithoutStackTrace
        "Data.RTree.D2.Float.Internal.bulkSTR: zero-sized partition"

    mend (ba, a) (bb, b) cs =
      case cs of
        (bc, c) : (bd, d) : e : f : gs ->
          (union4MBR ba bb bc bd, Node4 ba a bb b bc c bd d) <| mend e f gs

        (bc, c) : (bd, d) : (be, e) : [] ->
          (union3MBR ba bb bc, Node3 ba a bb b bc c) :|
            (unionMBR bd be, Node2 bd d be e) : []

        (bc, c) : (bd, d) : [] ->
          (union4MBR ba bb bc bd, Node4 ba a bb b bc c bd d) :| []

        (bc, c) : [] ->
          (union3MBR ba bb bc, Node3 ba a bb b bc c) :| []

        [] ->
          (unionMBR ba bb, Node2 ba a bb b) :| []

    base as =
      case as of
        (ba, a) : (bb, b) : (bc, c) : (bd, d) : [] ->
          (union4MBR ba bb bc bd, Leaf4 ba a bb b bc c bd d)

        (ba, a) : (bb, b) : (bc, c) : [] ->
          (union3MBR ba bb bc, Leaf3 ba a bb b bc c)

        (ba, a) : (bb, b) : [] ->
          (unionMBR ba bb, Leaf2 ba a bb b)

        _ -> errorWithoutStackTrace
               "Data.RTree.D2.Float.Internal.bulkSTR: malformed leaf"
