{-# LANGUAGE MagicHash
           , Rank2Types
           , UnboxedTuples #-}

module Data.Primitive.Array.Extra where

import           Control.Applicative
import           Control.Monad.ST
import qualified Data.Foldable as Fold
import           Data.Primitive.Array
import           Prelude hiding (Foldable (..))



{-# INLINEABLE fmap #-}
fmap :: (a -> b) -> Int -> Array a -> Array b
fmap f n as =
  let err = errorWithoutStackTrace "Data.Primitive.Array.Indexed.fmap: uninitialized"
  in createArray n err $ \m ->
       Fold.for_ [0 .. n - 1] $ \i -> do
         a <- indexArrayM as i
         writeArray m i $ f a

{-# INLINEABLE foldMap #-}
foldMap :: Monoid m => (a -> m) -> Int -> Array a -> m
foldMap f n as = flip Fold.foldMap [0 .. n - 1] $ \i ->
                        let (# a #) = indexArray## as i
                        in f a

{-# INLINEABLE ifoldMap #-}
ifoldMap :: Monoid m => (Int -> a -> m) -> Int -> Array a -> m
ifoldMap f n as = flip Fold.foldMap [0 .. n - 1] $ \i ->
                         let (# a #) = indexArray## as i
                         in f i a

{-# INLINEABLE foldMap' #-}
foldMap' :: Monoid m => (a -> m) -> Int -> Array a -> m
foldMap' f n as = flip Fold.foldMap' [0 .. n - 1] $ \i ->
                         let (# a #) = indexArray## as i
                         in f $! a

{-# INLINEABLE foldr #-}
foldr :: (a -> b -> b) -> b -> Int -> Array a -> b
foldr f z n as = let go i acc = let (# a #) = indexArray## as i
                                in f a acc
                 in Fold.foldr go z [0 .. n - 1]

{-# INLINEABLE foldr1 #-}
foldr1 :: (a -> a -> a) -> Int -> Array a -> a
foldr1 f n as
  | n <= 0    = errorWithoutStackTrace
                  "Data.Primitive.Array.Extra.foldr1: array length below one"
  | otherwise = let (# z #) = indexArray## as (n - 1)
                in foldr f z (n - 1) as

{-# INLINEABLE foldr' #-}
foldr' :: (a -> b -> b) -> b -> Int -> Array a -> b
foldr' f z n as = let go i acc = let (# a #) = indexArray## as i
                                 in a `seq` f a acc
                  in Fold.foldr' go z [0 .. n - 1]

{-# INLINEABLE foldl #-}
foldl :: (b -> a -> b) -> b -> Int -> Array a -> b
foldl f z n as = let go acc i = let (# a #) = indexArray## as i
                                in f acc a
                 in Fold.foldl go z [0 .. n - 1]

{-# INLINEABLE foldl' #-}
foldl' :: (b -> a -> b) -> b -> Int -> Array a -> b
foldl' f z n as = let go acc i = let (# a #) = indexArray## as i
                                 in f acc $! a
                  in Fold.foldl' go z [0 .. n - 1]

newtype Fused a = Fused { runFused :: forall s. MutableArray s a -> ST s (Array a) }

{-# INLINEABLE traverse #-}
traverse :: Applicative f => (a -> f b) -> Int -> Array a -> f (Array b)
traverse f n as
  | n <= 0    = pure emptyArray
  | otherwise =
      let err = errorWithoutStackTrace
                  "Data.Primitive.Array.Indexed.traverse: uninitialized"

          go i | i == n    = pure $ Fused unsafeFreezeArray
               | otherwise =
                   let (# a #) = indexArray## as i
                   in liftA2 (\b (Fused g) -> Fused $ \m -> do writeArray m i b
                                                               g m
                             ) (f a) $ go (i + 1)

      in (\g -> runST $ runFused g =<< newArray n err) <$> go 0


{-# INLINEABLE zip #-}
zip :: (a -> b -> c) -> Int -> Array a -> Array b -> Array c
zip f n as bs =
  let err = errorWithoutStackTrace "Data.Primitive.Array.Indexed.zip: uninitialized"
  in createArray n err $ \m ->
       Fold.for_ [0 .. n - 1] $ \i -> do
         a <- indexArrayM as i
         b <- indexArrayM bs i
         writeArray m i $ f a b

{-# INLINEABLE zipMap #-}
zipMap :: Monoid m => (a -> b -> m) -> Int -> Array a -> Array b -> m
zipMap f n as bs = flip Fold.foldMap [0 .. n - 1] $ \i ->
                          let (# a #) = indexArray## as i
                              (# b #) = indexArray## bs i
                          in f a b

{-# INLINEABLE izipMap #-}
izipMap :: Monoid m => (Int -> a -> b -> m) -> Int -> Array a -> Array b -> m
izipMap f n as bs = flip Fold.foldMap [0 .. n - 1] $ \i ->
                           let (# a #) = indexArray## as i
                               (# b #) = indexArray## bs i
                           in f i a b

{-# INLINEABLE zipMap' #-}
zipMap' :: Monoid m => (a -> b -> m) -> Int -> Array a -> Array b -> m
zipMap' f n as bs = flip Fold.foldMap' [0 .. n - 1] $ \i ->
                           let (# a #) = indexArray## as i
                               (# b #) = indexArray## bs i
                           in a `seq` b `seq` f a b

{-# INLINEABLE zipr #-}
zipr :: (a -> b -> c -> c) -> c -> Int -> Array a -> Array b -> c
zipr f z n as bs = let go i acc = let (# a #) = indexArray## as i
                                      (# b #) = indexArray## bs i
                                  in f a b acc
                   in Fold.foldr go z [0 .. n - 1]

{-# INLINEABLE izipr #-}
izipr :: (a -> b -> c -> c) -> c -> Int -> Array a -> Array b -> c
izipr f z n as bs = let go i acc = let (# a #) = indexArray## as i
                                       (# b #) = indexArray## bs i
                                   in f a b acc
                    in Fold.foldr go z [0 .. n - 1]

{-# INLINEABLE zipr' #-}
zipr' :: (a -> b -> c -> c) -> c -> Int -> Array a -> Array b -> c
zipr' f z n as bs = let go i acc = let (# a #) = indexArray## as i
                                       (# b #) = indexArray## bs i
                                   in a `seq` b `seq` f a b acc
                    in Fold.foldr' go z [0 .. n - 1]

{-# INLINEABLE zipl #-}
zipl :: (c -> a -> b -> c) -> c -> Int -> Array a -> Array b -> c
zipl f z n as bs = let go acc i = let (# a #) = indexArray## as i
                                      (# b #) = indexArray## bs i
                                  in f acc a b
                   in Fold.foldl go z [0 .. n - 1]

{-# INLINEABLE zipl' #-}
zipl' :: (c -> a -> b -> c) -> c -> Int -> Array a -> Array b -> c
zipl' f z n as bs = let go acc i = let (# a #) = indexArray## as i
                                       (# b #) = indexArray## bs i
                                   in a `seq` b `seq` f acc a b
                    in Fold.foldl' go z [0 .. n - 1]

{-# INLINEABLE zipA #-}
zipA :: Applicative f => (a -> b -> f c) -> Int -> Array a -> Array b -> f (Array c)
zipA f n as bs
  | n <= 0    = pure emptyArray
  | otherwise =
      let err = errorWithoutStackTrace
                  "Data.Primitive.Array.Indexed.zipA: uninitialized"

          go i | i == n    = pure $ Fused unsafeFreezeArray
               | otherwise =
                   let (# a #) = indexArray## as i
                       (# b #) = indexArray## bs i
                   in liftA2 (\c (Fused g) -> Fused $ \m -> do writeArray m i c
                                                               g m
                             ) (f a b) $ go (i + 1)

      in (\g -> runST $ runFused g =<< newArray n err) <$> go 0



{-# INLINEABLE snoc #-}
-- | Appends an element.
snoc :: Int -> Array a -> a -> Array a
snoc n as a =
  runST $ do
    bs <- newArray (n + 1) $
            errorWithoutStackTrace "Data.Primitive.Array.Indexed.snoc: empty"
    copyArray bs 0 as 0 n
    writeArray bs n a
    unsafeFreezeArray bs



{-# INLINEABLE replace #-}
-- | Replaces the element at index.
replace :: Int -> Array a -> Int -> a -> Array a
replace n as i a = update n as i $ \_ -> a



{-# INLINEABLE update #-}
-- | Updates the element at index.
update :: Int -> Array a -> Int -> (a -> a) -> Array a
update n as i f =
  runST $ do
    bs <- thawArray as 0 n
    a <- indexArrayM as i
    writeArray bs i $ f a
    unsafeFreezeArray bs



{-# INLINEABLE replaceSnoc #-}
-- | Replaces an element at index and appends another one.
replaceSnoc :: Int -> Array a -> Int -> a -> a -> Array a
replaceSnoc n as i a b =
  runST $ do
    bs <- newArray (n + 1) $
            errorWithoutStackTrace "Data.Primitive.Array.Indexed.replaceSnoc: empty"
    copyArray bs 0 as 0 n
    writeArray bs i a
    writeArray bs n b
    unsafeFreezeArray bs



{-# INLINEABLE lose #-}
-- | Drops an element at index.
lose :: Int -> Array a -> Int -> Array a
lose n as i =
  runST $ do
    bs <- newArray (n - 1) $
            errorWithoutStackTrace "Data.Primitive.Array.Indexed.lose: empty"
    copyArray bs 0 as 0       i
    copyArray bs i as (i + 1) ((n - 1) - i)
    unsafeFreezeArray bs
