{-# LANGUAGE PatternSynonyms
           , RoleAnnotations
           , ViewPatterns
           , UnboxedSums #-}

module Data.RTree.MBR.Internal
  ( MBR (.., MBR)
  ) where

import           Control.DeepSeq
import           Control.Monad.ST
import           Data.Primitive.PrimArray
import           Data.Primitive.Types



type role MBR nominal
-- | Two-dimensional minimum bounding rectangle.
--
--   This datatype is unboxed, so evaluating it to weak head normal form
--   evaluates all the coordinates to normal form.
newtype MBR r = MkMBR (PrimArray r)

{-# COMPLETE MBR #-}
pattern MBR
  :: Prim r
  => r      -- ^ xmin
  -> r      -- ^ ymin
  -> r      -- ^ xmax
  -> r      -- ^ ymax
  -> MBR r
pattern MBR xmin ymin xmax ymax <-
          ( (\(MkMBR arr) -> (# indexPrimArray arr 0
                             ,  indexPrimArray arr 1
                             ,  indexPrimArray arr 2
                             ,  indexPrimArray arr 3 #)
            ) -> (# xmin, ymin, xmax, ymax #) )
  where
    MBR xmin ymin xmax ymax = runST $ do
                                arr <- newPrimArray 4
                                writePrimArray arr 0 xmin
                                writePrimArray arr 1 ymin
                                writePrimArray arr 2 xmax
                                writePrimArray arr 3 ymax
                                MkMBR <$> unsafeFreezePrimArray arr

instance (Eq r, Prim r) => Eq (MBR r) where
  MBR xmin ymin xmax ymax == MBR xmin' ymin' xmax' ymax' =
    xmin == xmin' && ymin == ymin' && xmax == xmax' && ymax == ymax'

instance (Show r, Prim r) => Show (MBR r) where
  showsPrec n (MBR xmin ymin xmax ymax) =
    showParen (n > 10) $
        showString "MBR "
      . showsPrec 11 xmin . showChar ' '
      . showsPrec 11 ymin . showChar ' '
      . showsPrec 11 xmax . showChar ' '
      . showsPrec 11 ymax

instance NFData r => NFData (MBR r) where
  rnf (MkMBR _) = ()
