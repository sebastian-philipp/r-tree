{-# LANGUAGE RoleAnnotations #-}

{- |
     Module     : Data.RTree.MBR
     Copyright  : Copyright (c) 2015, Birte Wagner, Sebastian Philipp
                  Copyright (c) 2022, Oleksii Divak
     License    : MIT

     Maintainer : Oleksii Divak
     Stability  : experimental
     Portability: not portable
-}

module Data.RTree.MBR
  ( MBR (..)

  , valid

  , area
  , margin
  , overlap
  , distance

  , union

  , contains
  , contains'

  , intersection
  , intersection'
  , intersects
  , intersects'
  ) where

import           Control.DeepSeq
import           Control.Monad



type role MBR nominal
-- | Minimum bounding rectangle.
data MBR r = MBR
               !r -- ^ xmin
               !r -- ^ ymin
               !r -- ^ xmax
               !r -- ^ ymax

instance Eq r => Eq (MBR r) where
  MBR xmin ymin xmax ymax == MBR xmin' ymin' xmax' ymax' =
    xmin == xmin' && ymin == ymin' && xmax == xmax' && ymax == ymax'

instance Show r => Show (MBR r) where
  showsPrec n (MBR xmin ymin xmax ymax) =
    showParen (n > 10) $
        showString "MBR "
      . showsPrec 11 xmin . showChar ' '
      . showsPrec 11 ymin . showChar ' '
      . showsPrec 11 xmax . showChar ' '
      . showsPrec 11 ymax

instance NFData r => NFData (MBR r) where
  rnf (MBR a b c d) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d

instance NFData1 MBR where
  liftRnf f (MBR a b c d) = f a `seq` f b `seq` f c `seq` f d



{-# INLINABLE valid #-}
-- | Checks whether @xmin@ and @ymin@ are smaller than or equal to
--   @xmax@ and @ymax@ respectively.
valid :: Ord r => MBR r -> Bool
valid (MBR xmin ymin xmax ymax) = xmin <= xmax && ymin <= ymax



{-# INLINABLE union #-}
-- | Combines two 'MBR's.
union :: Ord r => MBR r -> MBR r -> MBR r
union (MBR xmin ymin xmax ymax) (MBR xmin' ymin' xmax' ymax') =
  MBR (xmin `min` xmin') (ymin `min` ymin') (xmax `max` xmax') (ymax `max` ymax')



{-# INLINABLE area #-}
-- | Area of the bounding rectangle.
area :: Num r => MBR r -> r
area (MBR xmin ymin xmax ymax) = (xmax - xmin) * (ymax - ymin)

{-# INLINABLE margin #-}
-- | Half the perimeter of the bounding rectangle.
margin :: Num r => MBR r -> r
margin (MBR xmin ymin xmax ymax) = (xmax - xmin) + (ymax - ymin)

{-# INLINEABLE overlap #-}
-- | Area of the intersection of two bounding rectangles.
overlap :: (Ord r, Num r) => MBR r -> MBR r -> r
overlap = intersection_ $ \x y x' y' -> if x < x' && y < y'
                                          then area $ MBR x y x' y'
                                          else 0


{-# INLINABLE distance #-}
-- | Square Euclidean distance between double the centers of two bounding rectangles.
--
--   NOTE: the 'distance' is therefore \(4\) times the actual square distance.
--   This may lead to an overflow sooner than expected when using small integer types
--   as coordinates.
distance :: Num r => MBR r -> MBR r -> r
distance (MBR xmin ymin xmax ymax) (MBR xmin' ymin' xmax' ymax') =
  let x = xmax + xmin - xmax' - xmin'
      y = ymax + ymin - ymax' - ymin'
  in x * x + y * y



{-# INLINE contains_ #-}
contains_ :: Ord r => (r -> r -> Bool) -> MBR r -> MBR r -> Bool
contains_ f (MBR xmin ymin xmax ymax) (MBR xmin' ymin' xmax' ymax') =
  f xmin xmin' && f ymin ymin' && f xmax' xmax && f ymax' ymax



{-# INLINABLE contains #-}
-- | Checks whether the first rectangle is fully contained by the second one.
contains :: Ord r => MBR r -> MBR r -> Bool
contains = contains_ (>=)

{-# INLINABLE contains' #-}
-- | Same as 'contains', but the checks are strict
--   (first rectangle touching any sides of the second one yields a 'False').
contains' :: Ord r => MBR r -> MBR r -> Bool
contains' = contains_ (>)



{-# INLINE intersection_ #-}
intersection_ :: Ord r => (r -> r -> r -> r -> a) -> MBR r -> MBR r -> a
intersection_ f (MBR xmin ymin xmax ymax) (MBR xmin' ymin' xmax' ymax') =
  f x y x' y'
  where
    x  = xmin `max` xmin'
    y  = ymin `max` ymin'
    x' = xmax `min` xmax'
    y' = ymax `min` ymax'



{-# INLINABLE intersection #-}
-- | Common area between two bounding rectangles, if any exists.
intersection :: Ord r => MBR r -> MBR r -> Maybe (MBR r)
intersection = intersection_ $ \x y x' y' -> MBR x y x' y' <$ guard (x <= x' && y <= y')

{-# INLINABLE intersection' #-}
-- | Same as 'intersection', but the checks are strict
--   (mere overlap on a side or a point returns 'Nothing').
intersection' :: Ord r => MBR r -> MBR r -> Maybe (MBR r)
intersection' = intersection_ $ \x y x' y' -> MBR x y x' y' <$ guard (x < x' && y < y')

{-# INLINABLE intersects #-}
-- | Checks whether two bounding rectangles have any common area.
intersects :: Ord r => MBR r -> MBR r -> Bool
intersects = intersection_ $ \x y x' y' -> x <= x' && y <= y'

{-# INLINABLE intersects' #-}
-- | Same as 'intersects', but the checks are strict
--   (mere overlap on a side or a point returns 'False').
intersects' :: Ord r => MBR r -> MBR r -> Bool
intersects' = intersection_ $ \x y x' y' -> x < x' && y < y'
