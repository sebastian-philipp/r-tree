{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

{- |
  Module     : Data.RTree.MBB
  Copyright  : Copyright (c) 2015, Birte Wagner, Sebastian Philipp
  License    : MIT

  Maintainer : Birte Wagner, Sebastian Philipp (sebastian@spawnhost.de)
  Stability  : experimental
  Portability: not portable

  This module provides a minimal bounding box.

-}


module Data.RTree.MBB
(
    MBB (..),
    mbb,
    area,
    containsMBB,
    touchesMBB,
    unionMBB,
    unionsMBB,
    intersectMBB,
    isValidMBB,
    isPointMBB
)
where

import Data.Binary

import           Data.Maybe (isJust)
import GHC.Generics (Generic)

-- | Minimal bounding box
data MBB = MBB {getUlx :: {-# UNPACK #-} ! Double, getUly :: {-# UNPACK #-} ! Double, getBrx :: {-# UNPACK #-} ! Double, getBry :: {-# UNPACK #-} ! Double}
    deriving (Eq, Generic, Ord)

-- | create a minimal bounding box (or a rectangle)
-- The first point must be smaller, than the second one. This is unchecked. To make sense of the following components, visualize x-axis pointing to the right and y-axis pointing downwards; u=upper, b=bottom, l=left,r=right.
mbb :: Double -- ^ x - coordinate of first  point (ulx)
    -> Double -- ^ y - coordinate of first  point (uly)
    -> Double -- ^ x - coordinate of second point (brx)
    -> Double -- ^ y - coordinate of second point (bry)
    -> MBB
mbb = MBB

-- | the property that a 'MBB' must hold, namely that the first point must be smaller than the second one.
isValidMBB :: MBB -> Bool
isValidMBB (MBB ulx uly brx bry) = (ulx <= brx) && (uly <= bry)

isPointMBB :: MBB -> Bool
isPointMBB (MBB ulx uly brx bry) = (ulx == brx) && (uly == bry)

-- | internal only.
unionsMBB :: [MBB] -> MBB
unionsMBB [] = error "unionsMBB': []"
unionsMBB xs = foldr1 unionMBB xs

-- | unifies two MBBs into one
unionMBB :: MBB -> MBB -> MBB
unionMBB (MBB ulx uly brx bry) (MBB ulx' uly' brx' bry') = MBB (min ulx ulx') (min uly uly') (max brx brx') (max bry bry')

-- | calculates the area of the rect
area :: MBB -> Double
area (MBB ulx uly brx bry) = (brx - ulx) * (bry - uly)

-- | returns True, when the first mbb contains the second
containsMBB :: MBB -> MBB -> Bool
containsMBB (MBB x11 y11 x12 y12) (MBB x21 y21 x22 y22) =  x11 <= x21 && y11 <= y21 && x12 >= x22 && y12 >= y22


-- | returns True, when the two mbbs touch each other
touchesMBB :: MBB -> MBB -> Bool
touchesMBB mbb1 mbb2 = isJust $ mbb1 `intersectMBB` mbb2


-- | returns the intersection of both mbbs. Returns Nothing, if they don't intersect.
intersectMBB :: MBB -> MBB -> Maybe MBB
intersectMBB (MBB ulx uly brx bry) (MBB ulx' uly' brx' bry')
    | ulx'' <= brx'' && uly'' <= bry'' = Just $ MBB ulx'' uly'' brx'' bry''
    | otherwise                        = Nothing
    where
    ulx'' = max ulx ulx'
    uly'' = max uly uly'
    brx'' = min brx brx'
    bry'' = min bry bry'


instance Show MBB where
    show (MBB ulx uly brx bry) = concat ["mbb ", show ulx, " ", show uly, " ", show brx, " ", show bry]


instance Binary MBB where
    put (MBB ulx uly brx bry) = put ulx >> put uly >> put brx >> put bry
    get = MBB <$> get <*> get <*> get <*> get
