{-# LANGUAGE CPP #-}

{- | 
     Module     : Data.RTree.Base
     Copyright  : Copyright (c) 2015, Birte Wagner, Sebastian Philipp
                  Copyright (c) 2022, Oleksii Divak
     License    : MIT

     Maintainer : Oleksii Divak
     Stability  : experimental
     Portability: not portable

     = R-Tree (lazy interface)

     @'RTree' r a@ represents a two-dimensional spatial tree
     from bounding rectangles with coordinates of type @r@ to values of type @a@.
     An 'RTree' is strict in its bounding rectangles but lazy in its values.

     Insertion functions in this module evaluate bounding rectangles to normal form
     before storing them in the map.
     Insertion functions in "Data.RTree.Strict" additionally evaluate values to
     weak head normal form before storing them in the map.

     This module is intended to be imported qualified, to avoid name clashes with
     Prelude functions:

     > import qualified Data.RTree.Lazy as R

     == Performance

     For all maps, folds and traversals the running time is:
     
     - \(O (\log_M n)\) in the best case that on each level only one node
       matches the 'Predicate';

     - \(O (n)\) in the worst case that all nodes qualify;

     == Using integer coordinates

     Integers are valid coordinate types within this library.
     However when using them care must be taken to ensure an overflow on comparison
     never occurs, otherwise the tree may start operating incorrectly.
     
     Good pointers for the minimum integer size applicable are the
     maximum 'MBR.distance' between two furthermost 'MBR's and the largest
     common 'MBR.area' of any two 'MBR's.

     == Lack of unions

     One-dimensional trees, e.g. @Map@s from the
     [containers](https://hackage.haskell.org/package/containers) package, allow you make
     unions between two trees efficiently. As R-Tree bounding rectangles are
     two-dimensional and both axes are treated separately, this feature does not
     extend to them. If you have a need to compute a merge of two large
     spatial trees within tight time constraints, consider a different data structure.

     == Implementation

     The implementation of 'RTree' is based on R-tree and R*-tree definitions
     as described by:

       * Antonin Guttman (1984),
         \"/R-Trees: A Dynamic Index Structure for Spatial Searching/\",
         <http://www-db.deis.unibo.it/courses/SI-LS/papers/Gut84.pdf>

       * N. Beckmann, H.P. Kriegel, R. Schneider, B. Seeger (1990),
         \"/The R*-tree: an efficient and robust access method for points and rectangles/\",
         <https://infolab.usc.edu/csci599/Fall2001/paper/rstar-tree.pdf>

       * S.T. Leutenegger, J.M. Edgington, M.A. Lopez (1997),
         \"/STR: A Simple and Efficient Algorithm for R-Tree Packing/\",
         <https://ia800900.us.archive.org/27/items/nasa_techdoc_19970016975/19970016975.pdf>

-}

module Data.RTree.Lazy
  ( -- * RTree
    RTree
  , MBR (MBR)
    -- * Construction
  , empty
  , singleton
    -- ** Bulk-loading
  , bulkSTR
    -- ** Naive
  , Data.RTree.Lazy.fromList
    -- * Comparisons
  , Predicate
  , equals
  , intersects
  , intersects'
  , contains
  , contains'
  , within
  , within'
    -- ** Size
  , R.null
  , R.length
  , depth
    -- * Insertion
  , insert
  , insertGut
    -- * Deletion
  , delete
    -- * Traversal
    -- ** Map
  , R.map
  , mapWithKey
  , R.traverse
  , traverseWithKey
    -- * Folds
  , R.foldMap
  , R.foldMapWithKey
  , R.foldr
  , R.foldrWithKey
  , R.foldl
  , R.foldlWithKey
    -- ** Strict
#if __GLASGOW_HASKELL__ >= 808
  , R.foldMap'
  , R.foldMapWithKey'
#endif
  , R.foldr'
  , R.foldrWithKey'
  , R.foldl'
  , R.foldlWithKey'
    -- * Conversion
  , elems
  , boxes
    -- ** Lists
  , R.toList
    -- * Prim
  , Prim
  ) where

import           Data.RTree.Internal as R
import           Data.RTree.Internal.Constants
import           Data.RTree.Lazy.Internal
import qualified Data.RTree.MBR as MBR

import           Control.Monad
import           Data.Foldable as Fold
import           Data.Function (on)
import           Data.List (sortBy)
import qualified Data.List as List
import           Data.Ord
import           Prelude hiding (Foldable (..))
import           Data.Primitive.Array
import           Data.Primitive.Types



-- | \(O (\log_M n)\). Insert a value into the tree.
--
--   No checks for duplicates are performed. 'delete' the relevant bounding rectangle
--   before the insertion if you wish to guarantee the uniqueness.
--
--   'insert' uses the BKSS reinsertion algorithm, but it does not implement the
--   choosing of subtree based on least overlap enlargement, as for the
--   'smallP' of \(1\) the behavior of the algorithm is identical to Guttman's.
--
--   'insert' is slightly slower than 'insertGut', but produces higher quality trees.
insert :: (Num r, Ord r, Prim r) => MBR r -> a -> RTree r a -> RTree r a
insert = insert_ []

insert_ :: (Num r, Ord r, Prim r) => [Int] -> MBR r -> a -> RTree r a -> RTree r a
insert_ lvls bz z t =
  case t of
    Root _ x   ->
      case insertNode 0 lvls bz z x of
        Right (ins, lvls', ba, a) -> Fold.foldr (uncurry $ insert_ lvls') (Root ba a) ins
        Left (bl, l, br, r)       -> Root (MBR.union bl br) $ mk Node [(bl, l), (br, r)]

    Leaf1 ba a -> Root (MBR.union ba bz) $ mk Leaf [(ba, a), (bz, z)]

    Empty      -> Leaf1 bz z



insertNode
  :: (Num r, Ord r, Prim r)
  => Int
  -> [Int]
  -> MBR r
  -> a
  -> Node r a
  -> Either (MBR r, Node r a, MBR r, Node r a) ([(MBR r, a)], [Int], MBR r, Node r a)
insertNode lvl lvls bz z x =
  let withR ins ls a = Right (ins, ls, union a, a)
      withL (l, r)   = Left (union l, l, union r, r)
  in case x of
       Node n brs as ->
         let i = leastEnlargement bz n brs
         in case insertNode (lvl + 1) lvls bz z $ indexArray as i of
              Right (ins, lvls', ba, a) -> withR ins lvls' $ replace Node n brs as i ba a

              Left (bl, l, br, r)
                | n < bigM  -> withR [] lvls $ replaceSnoc Node n brs as i bl l br r
                | otherwise -> withL . split Node sorted $ (bl, l) : (br, r) : discard i (nodes n brs as)

       Leaf n brs as
         | n < bigM  -> withR [] lvls $ snoc Leaf n brs as bz z

         | lvl `elem` lvls -> withL . split Leaf quad $ (bz, z) : nodes n brs as

         | otherwise ->
             let middle = bz `MBR.union` union x

                 (ins, keeps) =
                   splitAt smallP $
                     sortBy (flip on fst $ highestDistance middle) $ (bz, z) : nodes n brs as

             in withR ins (lvl : lvls) $ mk Leaf keeps




-- | \(O(n \cdot \log n)\). Bulk-load a tree using the Sort-Tile-Recursive algorithm.
--
--   Trees generated using this function do not respect 'smallM'.
--   Nonetheless they are valid in regards to all operations within this library.
bulkSTR :: (Num r, Ord r, Prim r) => [(MBR r, a)] -> RTree r a
bulkSTR []        = Empty
bulkSTR [(ba, a)] = Leaf1 ba a
bulkSTR xs
  | List.length xs <= bigM = Fold.foldr (uncurry insert) Empty xs
  | otherwise              = bulk' Leaf xs
  where
    bulk'
      :: (Num r, Ord r, Prim r)
      => (Int -> Array (MBR r) -> Array b -> Node r a) -> [(MBR r, b)] -> RTree r a
    bulk' f as = let ns = (\a -> (union a, a)) <$> pack f as
                 in if List.length ns > bigM
                      then bulk' Node ns
                      else let r = mk Node ns
                           in Root (union r) r
    pack f as =
      let bigP :: Int
          bigP = ceiling (fromIntegral (List.length xs) / fromIntegral bigM :: Double)

          bigS = ceiling $ sqrt (fromIntegral bigP :: Double)

      in Fold.foldMap (fmap (mk f) . part bigM . sortBy (ordY `on` fst))
           . part bigS $ sortBy (ordX `on` fst) as



-- | Traverses the list left-to-right and 'insert's elements one by one.
--
--   This is not a performant function, it's merely a 'R.foldl'' over 'insert'.
--
--   If you're only intending to use the resulting tree for lookups, consider 'bulkSTR' instead.
fromList :: (Num r, Ord r, Prim r) => [(MBR r, a)] -> RTree r a
fromList = Fold.foldl' (flip $ uncurry insert) Empty
