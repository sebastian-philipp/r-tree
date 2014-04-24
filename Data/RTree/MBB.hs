{-# LANGUAGE DeriveDataTypeable #-}


{- |
  Module     : Data.RTree.MBB
  Copyright  : Copyright (c) 2014, Birte Wagner, Sebastian Philipp
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
    unionMBB,
    intersectMBB
)
where

data MBB = MBB {getUlx :: {-# UNPACK #-} ! Double, getUly :: {-# UNPACK #-} ! Double, getBrx :: {-# UNPACK #-} ! Double, getBry :: {-# UNPACK #-} ! Double}
    deriving (Show, Eq)

mbb :: Double -> Double -> Double -> Double -> MBB
mbb = MBB

-- | internal only.
unionMBB :: [MBB] -> MBB
unionMBB [] = error "unionMBB': []"
unionMBB xs = foldr1 f xs
    where
    f (MBB ulx uly brx bry) (MBB ulx' uly' brx' bry') = MBB (min ulx ulx') (min uly uly') (max brx brx') (max bry bry')

area :: MBB -> Double
area (MBB ulx uly brx bry) = (brx - ulx) * (bry - uly)

containsMBB :: MBB -> MBB -> Bool
containsMBB (MBB x11 y11 x12 y12) (MBB x21 y21 x22 y22) =  x11 <= x21 && y11 <= y21 && x12 >= x22 && y12 >= y22

intersectMBB :: MBB -> MBB -> Maybe MBB
intersectMBB (MBB ulx uly brx bry) (MBB ulx' uly' brx' bry')
    | ulx'' <= brx'' && uly'' <= bry'' = Just $ MBB ulx'' uly'' brx'' bry''
    | otherwise                        = Nothing
    where
    ulx'' = max ulx ulx'
    uly'' = max uly uly'
    brx'' = min brx brx'
    bry'' = min bry bry'