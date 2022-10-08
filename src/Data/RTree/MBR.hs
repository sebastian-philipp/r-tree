{-# LANGUAGE PatternSynonyms
           , ViewPatterns
           , UnboxedSums #-}

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
  ( MBR (MBR)

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

import           Data.RTree.MBR.Internal

import           Control.Monad
import           Data.Primitive.Types



{-# INLINABLE valid #-}
-- | Checks whether @xmin@ and @ymin@ are smaller than or equal to
--   @xmax@ and @ymax@ respectively.
valid :: (Ord r, Prim r) => MBR r -> Bool
valid (MBR xmin ymin xmax ymax) = xmin <= xmax && ymin <= ymax



{-# INLINABLE union #-}
-- | Combines two 'MBR's.
union :: (Ord r, Prim r) => MBR r -> MBR r -> MBR r
union (MBR xmin ymin xmax ymax) (MBR xmin' ymin' xmax' ymax') =
  MBR (xmin `min` xmin') (ymin `min` ymin') (xmax `max` xmax') (ymax `max` ymax')



{-# INLINABLE area #-}
-- | Area of the bounding rectangle.
area :: (Num r, Prim r) => MBR r -> r
area (MBR xmin ymin xmax ymax) = (xmax - xmin) * (ymax - ymin)

{-# INLINABLE margin #-}
-- | Half the perimeter of the bounding rectangle.
margin :: (Num r, Prim r) => MBR r -> r
margin (MBR xmin ymin xmax ymax) = (xmax - xmin) + (ymax - ymin)

{-# INLINEABLE overlap #-}
-- | Area of the intersection of two bounding rectangles.
overlap :: (Ord r, Num r, Prim r) => MBR r -> MBR r -> r
overlap = intersection_ $ \x y x' y' -> if x < x' && y < y'
                                          then area $ MBR x y x' y'
                                          else 0


{-# INLINABLE distance #-}
-- | Square Euclidean distance between double the centers of two bounding rectangles.
--
--   NOTE: the 'distance' is therefore \(4\) times the actual square distance.
--   This may lead to an overflow sooner than expected when using small integer types
--   as coordinates.
distance :: (Num r, Prim r) => MBR r -> MBR r -> r
distance (MBR xmin ymin xmax ymax) (MBR xmin' ymin' xmax' ymax') =
  let x = xmax + xmin - xmax' - xmin'
      y = ymax + ymin - ymax' - ymin'
  in x * x + y * y



{-# INLINE contains_ #-}
contains_ :: Prim r => (r -> r -> Bool) -> MBR r -> MBR r -> Bool
contains_ f (MBR xmin ymin xmax ymax) (MBR xmin' ymin' xmax' ymax') =
  f xmin xmin' && f ymin ymin' && f xmax' xmax && f ymax' ymax



{-# INLINABLE contains #-}
-- | Checks whether the first rectangle is fully contained by the second one.
contains :: (Ord r, Prim r) => MBR r -> MBR r -> Bool
contains = contains_ (>=)

{-# INLINABLE contains' #-}
-- | Same as 'contains', but the checks are strict
--   (first rectangle touching any sides of the second one yields a 'False').
contains' :: (Ord r, Prim r) => MBR r -> MBR r -> Bool
contains' = contains_ (>)



{-# INLINE intersection_ #-}
intersection_ :: (Ord r, Prim r) => (r -> r -> r -> r -> a) -> MBR r -> MBR r -> a
intersection_ f (MBR xmin ymin xmax ymax) (MBR xmin' ymin' xmax' ymax') =
  f x y x' y'
  where
    x  = xmin `max` xmin'
    y  = ymin `max` ymin'
    x' = xmax `min` xmax'
    y' = ymax `min` ymax'



{-# INLINABLE intersection #-}
-- | Common area between two bounding rectangles, if any exists.
intersection :: (Ord r, Prim r) => MBR r -> MBR r -> Maybe (MBR r)
intersection = intersection_ $ \x y x' y' -> MBR x y x' y' <$ guard (x <= x' && y <= y')

{-# INLINABLE intersection' #-}
-- | Same as 'intersection', but the checks are strict
--   (mere overlap on a side or a point returns 'Nothing').
intersection' :: (Ord r, Prim r) => MBR r -> MBR r -> Maybe (MBR r)
intersection' = intersection_ $ \x y x' y' -> MBR x y x' y' <$ guard (x < x' && y < y')

{-# INLINABLE intersects #-}
-- | Checks whether two bounding rectangles have any common area.
intersects :: (Ord r, Prim r) => MBR r -> MBR r -> Bool
intersects = intersection_ $ \x y x' y' -> x <= x' && y <= y'

{-# INLINABLE intersects' #-}
-- | Same as 'intersects', but the checks are strict
--   (mere overlap on a side or a point returns 'False').
intersects' :: (Ord r, Prim r) => MBR r -> MBR r -> Bool
intersects' = intersection_ $ \x y x' y' -> x < x' && y < y'
