{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
     Reference spatial tree implemented using a naive list of elements.

     Every fold/map is \(O (n)\).
-}

module Data.NoTree where

import           Data.Primitive.Types
import           Data.RTree.Internal (Predicate (..), onElement, MBR (..))

import           Control.DeepSeq
import qualified Data.Foldable as Fold
import qualified Data.List as List
import           Prelude hiding (Foldable (..))



newtype NoTree r a = NoTree { toList :: [(MBR r, a)] }
                     deriving NFData

instance (Prim r, Show r, Show a) => Show (NoTree r a) where
  show = showString "fromList " . flip showList "" . toList

instance Functor (NoTree r) where
  fmap f = NoTree . fmap (fmap f) . toList

instance Fold.Foldable (NoTree r) where
  foldMap  f = Fold.foldMap  (f . snd) . toList
  foldMap' f = Fold.foldMap' (f . snd) . toList

  foldr  f z = Fold.foldr  (f . snd) z . toList
  foldr' f z = Fold.foldr' (f . snd) z . toList

  foldl  f z = Fold.foldl  (\acc -> f acc . snd) z . toList
  foldl' f z = Fold.foldl' (\acc -> f acc . snd) z . toList

instance Traversable (NoTree r) where
  traverse f = fmap NoTree . Prelude.traverse (Prelude.traverse f) . toList



empty :: NoTree r a
empty = NoTree []

singleton :: MBR r -> a -> NoTree r a
singleton = curry $ NoTree . (:[])



null :: NoTree r a -> Bool
null = List.null . toList

length :: NoTree r a -> Int
length = List.length . toList



insert :: (Eq r, Prim r) => MBR r -> a -> NoTree r a -> NoTree r a
insert ba a = NoTree . (:) (ba, a) . toList

delete :: (Eq r, Prim r) => MBR r -> NoTree r a -> NoTree r a
delete ba no = let (xs, ys) = span ((/= ba) . fst) $ toList no
               in NoTree $ xs <> drop 1 ys



map :: Predicate r -> (a -> a) -> NoTree r a -> NoTree r a
map pre = Data.NoTree.mapWithKey pre . const

mapWithKey :: Predicate r -> (MBR r -> a -> a) -> NoTree r a -> NoTree r a
mapWithKey pre f = NoTree . fmap opt . toList
  where
    opt (ba, a) = (,) ba $ if onElement pre ba
                             then f ba a
                             else a



foldMap :: Monoid m => Predicate r -> (a -> m) -> NoTree r a -> m
foldMap pre = foldMapWithKey pre . const

foldMapWithKey :: Monoid m => Predicate r -> (MBR r -> a -> m) -> NoTree r a -> m
foldMapWithKey pre f = Fold.foldMap opt . toList
  where
    opt (ba, a) | onElement pre ba = f ba a
                | otherwise         = mempty

foldMap' :: Monoid m => Predicate r -> (a -> m) -> NoTree r a -> m
foldMap' pre = foldMapWithKey' pre . const

foldMapWithKey' :: Monoid m => Predicate r -> (MBR r -> a -> m) -> NoTree r a -> m
foldMapWithKey' pre f = Fold.foldMap' opt . toList
  where
    opt (ba, a) | onElement pre ba = f ba a
                | otherwise         = mempty



foldr :: Predicate r -> (a -> b -> b) -> b -> NoTree r a -> b
foldr pre = foldrWithKey pre . const

foldrWithKey :: Predicate r -> (MBR r -> a -> b -> b) -> b -> NoTree r a -> b
foldrWithKey pre f z = Fold.foldr opt z . toList
  where
    opt (ba, a) acc | onElement pre ba = f ba a acc
                    | otherwise         = acc

foldr' :: Predicate r -> (a -> b -> b) -> b -> NoTree r a -> b
foldr' pre = foldrWithKey' pre . const

foldrWithKey' :: Predicate r -> (MBR r -> a -> b -> b) -> b -> NoTree r a -> b
foldrWithKey' pre f z = Fold.foldr' opt z . toList
  where
    opt (ba, a) acc | onElement pre ba = f ba a acc
                    | otherwise         = acc



foldl :: Predicate r -> (b -> a -> b) -> b -> NoTree r a -> b
foldl pre = foldlWithKey pre . (.) const

foldlWithKey :: Predicate r -> (b -> MBR r -> a -> b) -> b -> NoTree r a -> b
foldlWithKey pre f z = Fold.foldl opt z . toList
  where
    opt acc (ba, a) | onElement pre ba = f acc ba a
                    | otherwise         = acc

foldl' :: Predicate r -> (b -> a -> b) -> b -> NoTree r a -> b
foldl' pre = foldlWithKey' pre . (.) const

foldlWithKey' :: Predicate r -> (b -> MBR r -> a -> b) -> b -> NoTree r a -> b
foldlWithKey' pre f z = Fold.foldl' opt z . toList
  where
    opt acc (ba, a) | onElement pre ba = f acc ba a
                    | otherwise         = acc



traverse :: Applicative f => Predicate r -> (a -> f a) -> NoTree r a -> f (NoTree r a)
traverse pre = Data.NoTree.traverseWithKey pre . const

traverseWithKey
  :: Applicative f => Predicate r -> (MBR r -> a -> f a) -> NoTree r a -> f (NoTree r a)
traverseWithKey pre f = fmap NoTree . Prelude.traverse opt . toList
  where
    opt (ba, a) | onElement pre ba = (,) ba <$> f ba a
                | otherwise        = pure (ba, a)



fromList :: [(MBR r, a)] -> NoTree r a
fromList = NoTree
