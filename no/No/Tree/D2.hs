{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
     Reference spatial tree implemented using a naive list of elements.

     Every fold/map is \(O (n)\).
-}

module No.Tree.D2 where

import           Data.RTree.D2.Double.Unsafe (MBR (..), Predicate (..))

import           Control.DeepSeq
import qualified Data.Foldable as Fold
import qualified Data.List as List
import           Prelude hiding (Foldable (..))



newtype NoTree a = NoTree { toList :: [(MBR, a)] }

instance Show a => Show (NoTree a) where
  show = showString "fromList " . flip showList "" . toList

instance NFData a => NFData (NoTree a) where
  rnf = liftRnf (\(ba, a) -> ba `seq` rnf a) . toList

instance Functor NoTree where
  fmap f = NoTree . fmap (fmap f) . toList

instance Fold.Foldable NoTree where
  foldMap  f = Fold.foldMap  (f . snd) . toList

  foldr  f z = Fold.foldr  (f . snd) z . toList
  foldr' f z = Fold.foldr' (f . snd) z . toList

  foldl  f z = Fold.foldl  (\acc -> f acc . snd) z . toList
  foldl' f z = Fold.foldl' (\acc -> f acc . snd) z . toList

instance Traversable NoTree where
  traverse f = fmap NoTree . Prelude.traverse (Prelude.traverse f) . toList



empty :: NoTree a
empty = NoTree []

singleton :: MBR -> a -> NoTree a
singleton bx x = NoTree [(bx, x)]



null :: NoTree a -> Bool
null = List.null . toList

length :: NoTree a -> Int
length = List.length . toList



insert :: MBR -> a -> NoTree a -> NoTree a
insert ba a = NoTree . (:) (ba, a) . toList

delete :: MBR -> NoTree a -> NoTree a
delete ba no = let (xs, ys) = break ((== ba) . fst) $ toList no
               in NoTree $ xs <> drop 1 ys



mapWithKey :: (MBR -> a -> b) -> NoTree a -> NoTree b
mapWithKey f = NoTree . fmap (\ ~(ba, a) -> (ba, f ba a) ) . toList

adjustRangeWithKey :: Predicate -> (MBR -> a -> a) -> NoTree a -> NoTree a
adjustRangeWithKey (Predicate _ checkLeaf) f =
  NoTree . fmap (\(ba, a) -> (ba, opt ba a)) . toList
  where
    opt ba a | checkLeaf ba = f ba a
             | otherwise    = a



foldMapRangeWithKey :: Monoid m => Predicate -> (MBR -> a -> m) -> NoTree a -> m
foldMapRangeWithKey (Predicate _ checkLeaf) f = Fold.foldMap opt . toList
  where
    opt (ba, a) | checkLeaf ba = f ba a
                | otherwise    = mempty


foldrRangeWithKey :: Predicate -> (MBR -> a -> b -> b) -> b -> NoTree a -> b
foldrRangeWithKey (Predicate _ checkLeaf) f z = Fold.foldr opt z . toList
  where
    opt (ba, a) acc | checkLeaf ba = f ba a acc
                    | otherwise    = acc

foldrRangeWithKey' :: Predicate -> (MBR -> a -> b -> b) -> b -> NoTree a -> b
foldrRangeWithKey' (Predicate _ checkLeaf) f z = Fold.foldr' opt z . toList
  where
    opt (ba, a) acc | checkLeaf ba = f ba a acc
                    | otherwise    = acc


foldlRangeWithKey :: Predicate -> (b -> MBR -> a -> b) -> b -> NoTree a -> b
foldlRangeWithKey (Predicate _ checkLeaf) f z = Fold.foldl opt z . toList
  where
    opt acc (ba, a) | checkLeaf ba = f acc ba a
                    | otherwise    = acc

foldlRangeWithKey' :: Predicate -> (b -> MBR -> a -> b) -> b -> NoTree a -> b
foldlRangeWithKey' (Predicate _ checkLeaf) f z = Fold.foldl' opt z . toList
  where
    opt acc (ba, a) | checkLeaf ba = f acc ba a
                    | otherwise    = acc



traverseWithKey
  :: Applicative f => (MBR -> a -> f b) -> NoTree a -> f (NoTree b)
traverseWithKey f =
  fmap NoTree . Prelude.traverse ( \(ba, a) -> (,) ba <$> f ba a) . toList

traverseRangeWithKey
  :: Applicative f
  => Predicate -> (MBR -> a -> f a) -> NoTree a -> f (NoTree a)
traverseRangeWithKey (Predicate _ checkLeaf) f =
  fmap NoTree . Prelude.traverse ( \(ba, a) -> (,) ba <$> opt ba a) . toList
  where
    opt ba a | checkLeaf ba = f ba a
             | otherwise    = pure a



fromList :: [(MBR, a)] -> NoTree a
fromList = NoTree
