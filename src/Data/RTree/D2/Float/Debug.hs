{-# LANGUAGE ScopedTypeVariables #-}

{- |
     Module     : Data.RTree.D2.Float.Debug
     Copyright  : Copyright (c) 2015, Birte Wagner, Sebastian Philipp
                  Copyright (c) 2022, Oleksii Divak
     License    : MIT

     Maintainer : Oleksii Divak
     Stability  : experimental
     Portability: not portable

     Functions that expose the innerworkings of an 'RTree', but are completely safe
     to use otherwise.
-}

module Data.RTree.D2.Float.Debug
  ( showsTree

  , Validity (..)
  , Reason (..)
  , validate
  ) where

import           Data.RTree.D2.Float.Internal



-- | \(\mathcal{O}(n)\).
--   Shows the internal structure of the R-tree.
showsTree :: (a -> ShowS) -> RTree a -> ShowS
showsTree f = go id 0
  where
    {-# INLINE mbr #-}
    mbr (UnsafeMBR xmin ymin xmax ymax) = shows (xmin, ymin, xmax, ymax)

    {-# INLINE offset #-}
    offset i
      | i <= 0    = id
      | otherwise = showChar ' ' . offset (i - 1)

    go s (i :: Int) n =
      offset i .
        case n of
          Node2 ba a bb b           ->
            showString "Node 2" . s
              . showChar '\n' . go (showChar ' ' . mbr ba) (i + 2) a
              . showChar '\n' . go (showChar ' ' . mbr bb) (i + 2) b

          Node3 ba a bb b bc c      ->
            showString "Node 3" . s
              . showChar '\n' . go (showChar ' ' . mbr ba) (i + 2) a
              . showChar '\n' . go (showChar ' ' . mbr bb) (i + 2) b
              . showChar '\n' . go (showChar ' ' . mbr bc) (i + 2) c

          Node4 ba a bb b bc c bd d ->
            showString "Node 4" . s
              . showChar '\n' . go (showChar ' ' . mbr ba) (i + 2) a
              . showChar '\n' . go (showChar ' ' . mbr bb) (i + 2) b
              . showChar '\n' . go (showChar ' ' . mbr bc) (i + 2) c
              . showChar '\n' . go (showChar ' ' . mbr bd) (i + 2) d

          Leaf2 ba a bb b           ->
            showString "Leaf 2" . s
              . showChar '\n' . offset (i + 2) . mbr ba . showChar ' ' . f a
              . showChar '\n' . offset (i + 2) . mbr bb . showChar ' ' . f b

          Leaf3 ba a bb b bc c      ->
            showString "Leaf 3" . s
              . showChar '\n' . offset (i + 2) . mbr ba . showChar ' ' . f a
              . showChar '\n' . offset (i + 2) . mbr bb . showChar ' ' . f b
              . showChar '\n' . offset (i + 2) . mbr bc . showChar ' ' . f c

          Leaf4 ba a bb b bc c bd d ->
            showString "Leaf 4" . s
              . showChar '\n' . offset (i + 2) . mbr ba . showChar ' ' . f a
              . showChar '\n' . offset (i + 2) . mbr bb . showChar ' ' . f b
              . showChar '\n' . offset (i + 2) . mbr bc . showChar ' ' . f c
              . showChar '\n' . offset (i + 2) . mbr bd . showChar ' ' . f d

          Leaf1 bx x                ->
            showString "Leaf 1" . s
              . showChar '\n' . offset (i + 2) . mbr bx . showChar ' ' . f x

          Empty                    ->
            showString "Empty" . s



-- | Whether the tree is well-formed.
data Validity = Valid
              | Invalid Reason
                deriving Show

-- | Reason for why the tree is considered malformed.
data Reason = -- | Not all nodes are at the same depth.
              UnbalancedTree
              -- | Node does not enclose all inner t'MBR's properly.
            | MalformedNode MBR
              -- | Found a 'Leaf1' node not at root level.
            | FoundLeaf1
              -- | Found an 'Empty' node not at root level.
            | FoundEmpty
              deriving Show



data Carry = Carry Int
           | Broken Reason

carry2 :: Carry -> Carry -> Carry
carry2 (Carry i) (Carry j)
  | i == j    = Carry (i + 1)
  | otherwise = Broken UnbalancedTree

carry2 (Carry _) b         = b
carry2 a         _         = a

carry3 :: Carry -> Carry -> Carry -> Carry
carry3 (Carry i) (Carry j) (Carry k)
  | i == j, i == k = Carry (i + 1)
  | otherwise      = Broken UnbalancedTree

carry3 (Carry _) (Carry _) c         = c
carry3 (Carry _) b         _         = b
carry3 a         _         _         = a

carry4 :: Carry -> Carry -> Carry -> Carry -> Carry
carry4 (Carry i) (Carry j) (Carry k) (Carry l)
  | i == j, i == k, i == l = Carry (i + 1)
  | otherwise              = Broken UnbalancedTree

carry4 (Carry _) (Carry _) (Carry _) d         = d
carry4 (Carry _) (Carry _) c         _         = c
carry4 (Carry _) b         _         _         = b
carry4 a         _         _         _         = a



-- | \(\mathcal{O}(n)\).
--   Checks whether the tree is well-formed.
validate :: RTree a -> Validity
validate t =
  case t of
    Leaf1 _ _ -> Valid
    Empty     -> Valid
    _         ->
      case go Nothing t of
        Carry _  -> Valid
        Broken r -> Invalid r
  where
    go mbx x =
      case x of
        Node2 ba a bb b
          | Just bx <- mbx, bx /= unionMBR ba bb -> Broken $ MalformedNode bx
          | otherwise ->
              carry2 (go (Just ba) a)
                     (go (Just bb) b)

        Node3 ba a bb b bc c
          | Just bx <- mbx, bx /= unionMBR (unionMBR ba bb) bc -> Broken $ MalformedNode bx
          | otherwise ->
              carry3 (go (Just ba) a)
                     (go (Just bb) b)
                     (go (Just bc) c)

        Node4 ba a bb b bc c bd d
          | Just bx <- mbx
          , bx /= unionMBR (unionMBR (unionMBR ba bb) bc) bd -> Broken $ MalformedNode bx

          | otherwise ->
              carry4 (go (Just ba) a)
                     (go (Just bb) b)
                     (go (Just bc) c)
                     (go (Just bd) d)

        Leaf2 ba _ bb _
          | Just bx <- mbx, bx /= unionMBR ba bb -> Broken $ MalformedNode bx
          | otherwise -> Carry 0

        Leaf3 ba _ bb _ bc _
          | Just bx <- mbx, bx /= unionMBR (unionMBR ba bb) bc -> Broken $ MalformedNode bx
          | otherwise -> Carry 0

        Leaf4 ba _ bb _ bc _ bd _
          | Just bx <- mbx
          , bx /= unionMBR (unionMBR (unionMBR ba bb) bc) bd -> Broken $ MalformedNode bx

          | otherwise -> Carry 0

        Leaf1 _  _ -> Broken FoundLeaf1
        Empty      -> Broken FoundEmpty
