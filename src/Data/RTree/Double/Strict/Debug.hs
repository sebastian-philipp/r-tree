{-# LANGUAGE ScopedTypeVariables #-}

{- |
     Module     : Data.RTree.Double.Strict.Debug
     Copyright  : Copyright (c) 2015, Birte Wagner, Sebastian Philipp
                  Copyright (c) 2022, Oleksii Divak
     License    : MIT

     Maintainer : Oleksii Divak
     Stability  : experimental
     Portability: not portable

     Functions that expose the innerworkings of an 'RTree', but are completely safe
     to use otherwise.
-}

module Data.RTree.Double.Strict.Debug
  ( showsTree

  , depth

  , isProper
  ) where

import           Data.RTree.Double.Strict.Internal



-- | \(\mathcal{O}(n)\). Shows the internal structure of the R-tree.
showsTree :: (a -> ShowS) -> RTree a -> ShowS
showsTree f r =
  case r of
    Root n     -> go id 0 n
    Leaf1 bx x -> showString "Leaf 1\n  " . mbr bx . showChar ' ' . f x
    Empty      -> showString "Empty"
  where
    {-# INLINE mbr #-}
    mbr (UnsafeMBR xmin ymin xmax ymax) = shows (xmin, ymin, xmax, ymax)

    {-# INLINE offset #-}
    offset i
      | i <= 0    = id
      | otherwise = showChar ' ' . offset (i - 1)

    {-# NOINLINE go #-}
    go s (i :: Int) n =
      case n of
        Node2 ba a bb b           ->
          offset i . showString "Node 2" . s . showChar '\n'
                            . go (showChar ' ' . mbr ba) (i + 2) a
            . showChar '\n' . go (showChar ' ' . mbr bb) (i + 2) b

        Node3 ba a bb b bc c      ->
          offset i . showString "Node 3" . s . showChar '\n'
                            . go (showChar ' ' . mbr ba) (i + 2) a
            . showChar '\n' . go (showChar ' ' . mbr bb) (i + 2) b
            . showChar '\n' . go (showChar ' ' . mbr bc) (i + 2) c

        Node4 ba a bb b bc c bd d ->
          offset i . showString "Node 4" . s . showChar '\n'
                            . go (showChar ' ' . mbr ba) (i + 2) a
            . showChar '\n' . go (showChar ' ' . mbr bb) (i + 2) b
            . showChar '\n' . go (showChar ' ' . mbr bc) (i + 2) c
            . showChar '\n' . go (showChar ' ' . mbr bd) (i + 2) d

        Leaf2 ba a bb b           ->
          offset i . showString "Leaf 2" . s . showChar '\n'
                            . offset (i + 2) . mbr ba . showChar ' ' . f a
            . showChar '\n' . offset (i + 2) . mbr bb . showChar ' ' . f b

        Leaf3 ba a bb b bc c      ->
          offset i . showString "Leaf 3" . s . showChar '\n'
                            . offset (i + 2) . mbr ba . showChar ' ' . f a
            . showChar '\n' . offset (i + 2) . mbr bb . showChar ' ' . f b
            . showChar '\n' . offset (i + 2) . mbr bc . showChar ' ' . f c

        Leaf4 ba a bb b bc c bd d ->
          offset i . showString "Leaf 4" . s . showChar '\n'
                            . offset (i + 2) . mbr ba . showChar ' ' . f a
            . showChar '\n' . offset (i + 2) . mbr bb . showChar ' ' . f b
            . showChar '\n' . offset (i + 2) . mbr bc . showChar ' ' . f c
            . showChar '\n' . offset (i + 2) . mbr bd . showChar ' ' . f d



-- | \(\mathcal{O}(n)\). If the tree is not malformed, returns leaf node depth.
--
--   A properly constructed R-tree always stores all leaves at the same depth.
depth :: RTree a -> Maybe Int
depth r =
  case r of
    Root n    -> go n
    Leaf1 _ _ -> Just 1
    Empty     -> Just 0
  where
    go n =
      case n of
        Node2 _ a _ b         -> do
          a' <- go a
          b' <- go b
          if a' == b'
            then Just $ a' + 1
            else Nothing

        Node3 _ a _ b _ c     -> do
          a' <- go a
          b' <- go b
          c' <- go c
          if a' == b' && a' == c'
            then Just $ a' + 1
            else Nothing

        Node4 _ a _ b _ c _ d -> do
          a' <- go a
          b' <- go b
          c' <- go c
          d' <- go d
          if a' == b' && a' == c' && a' == d'
            then Just $ a' + 1
            else Nothing

        Leaf2 _ _ _ _         -> Just 1
        Leaf3 _ _ _ _ _ _     -> Just 1
        Leaf4 _ _ _ _ _ _ _ _ -> Just 1



union3MBR :: MBR -> MBR -> MBR -> MBR
union3MBR ba bb bc = unionMBR (unionMBR ba bb) bc

union4MBR :: MBR -> MBR -> MBR -> MBR -> MBR
union4MBR ba bb bc bd = unionMBR (unionMBR ba bb) (unionMBR bc bd)

-- | \(\mathcal{O}(n)\).
--   Check that the 'MBR' of every non-leaf node tightly encloses all contained 'MBR's.
isProper :: RTree a -> Bool
isProper r =
  case r of
    Root n -> go Nothing n
    _      -> True
  where
    go mbn n =
      let check2 bx by =
            case mbn of
              Nothing -> True
              Just bn -> eqMBR bn (unionMBR bx by)

          check3 bx by bz =
            case mbn of
              Nothing -> True
              Just bn -> eqMBR bn (union3MBR bx by bz)

          check4 bw bx by bz =
            case mbn of
              Nothing -> True
              Just bn -> eqMBR bn (union4MBR bw bx by bz)

      in case n of
           Node2 ba a bb b           ->
             check2 ba bb       && go (Just ba) a && go (Just bb) b

           Node3 ba a bb b bc c      ->
             check3 ba bb bc    && go (Just ba) a && go (Just bb) b
                                && go (Just bc) c

           Node4 ba a bb b bc c bd d ->
             check4 ba bb bc bd && go (Just ba) a && go (Just bb) b
                                && go (Just bc) c && go (Just bd) d

           Leaf2 ba _ bb _           -> check2 ba bb
           Leaf3 ba _ bb _ bc _      -> check3 ba bb bc
           Leaf4 ba _ bb _ bc _ bd _ -> check4 ba bb bc bd
