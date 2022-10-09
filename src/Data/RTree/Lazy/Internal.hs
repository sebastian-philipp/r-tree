module Data.RTree.Lazy.Internal
  ( insertGut
  , delete
  ) where

import           Data.RTree.Internal as R
import           Data.RTree.Internal.Constants
import qualified Data.RTree.MBR as MBR

import           Data.Foldable as Fold
import           Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Ord
import           Data.Monoid
import           Prelude hiding (Foldable (..))



-- | \(O (\log_M n)\). Same as 'Data.Map.Lazy.insert',
--   using Guttman's original insertion algorithm.
insertGut :: (Num r, Ord r) => MBR r -> a -> RTree r a -> RTree r a
insertGut bz z t =
  case t of
    Root _ x   ->
      case insertGutNode bz z x of
        Right (ba, a)       -> Root ba a
        Left (bl, l, br, r) -> Root (MBR.union bl br) . Node $ (bl, l) :| [(br, r)]

    Leaf1 ba a -> Root (MBR.union ba bz) . Leaf $ (ba, a) :| [(bz, z)]
    Empty      -> Leaf1 bz z



insertGutNode
  :: (Num r, Ord r)
  => MBR r
  -> a
  -> Node r a
  -> Either (MBR r, Node r a, MBR r, Node r a) (MBR r, Node r a)
insertGutNode bz z x =
  let withR a      = Right (union a, a)
      withL (l, r) = Left (union l, l, union r, r)
  in case x of
       Node as ->
         let (i, least) = leastEnlargement bz as
         in case insertGutNode bz z least of
              Right (ba, a)   -> withR . Node . NonEmpty.fromList $ swap i (ba, a) as
              Left (bl, l, br, r)
                | NonEmpty.length as < bigM  -> withR . Node $ (br, r) :| swap i (bl, l) as
                | otherwise                  ->
                    withL . split Node quad $ (bl, l) :| ((br, r) : discard i as)

       Leaf as
         | NonEmpty.length as < bigM -> withR . Leaf $ (bz, z) <| as
         | otherwise                 -> withL . split Leaf quad $ (bz, z) <| as



insertDepth
  :: (Num r, Ord r) => Int -> MBR r -> Node r a -> RTree r a -> (Bool, RTree r a)
insertDepth d bz z t =
  case t of
    Root _ x  ->
      case insertDepthNode d bz z x of
        Right (ba, a)       -> (,) False $ Root ba a
        Left (bl, l, br, r) ->
          (,) True  . Root (MBR.union bl br) . Node $ (bl, l) :| [(br, r)]

    Empty     -> (,) False $ Root bz z
    Leaf1 _ _ -> errorWithoutStackTrace "Data.RTree.Lazy.insertDepth: leaf on root level"



insertDepthNode
  :: (Num r, Ord r)
  => Int -> MBR r -> Node r a -> Node r a -> Either (MBR r, Node r a, MBR r, Node r a) (MBR r, Node r a)
insertDepthNode d bz z x =
  let withR a = Right (union a, a)
      withL (l, r) = Left (union l, l, union r, r)
  in case x of
       Node as
         | d <= 0    ->
             if NonEmpty.length as < bigM
               then withR . Node $ (bz, z) <| as
               else withL . split Node quad $ (bz, z) <| as

         | otherwise ->
             let (i, least) = leastEnlargement bz as
             in case insertDepthNode (d - 1) bz z least of
                  Right (ba, a)   -> withR . Node . NonEmpty.fromList $ swap i (ba, a) as
                  Left (bl, l, br, r)
                    | NonEmpty.length as < bigM ->
                        withR . Node $ (br, r) :| swap i (bl, l) as

                    | otherwise ->
                        withL . split Node quad $ (br, r) :| swap i (bl, l) as

       Leaf _ -> errorWithoutStackTrace $ "Data.RTree.Lazy.insertDepth: reached a leaf"



-- | \(O (\log_M n)\). Delete the first occurrence of a given bounding rectangle, if one exists.
--
--   'delete' uses Guttman's original deletion and (re)insertion algorithms.
delete :: (Num r, Ord r) => MBR r -> RTree r a -> RTree r a
delete bz r =
  case r of
    Root _ x  ->
      let deep i (bb, b) (d, r') = let (higher, r'') = insertDepth (i - d) bb b r'
                                   in flip (,) r'' $ if higher
                                                       then d + 1
                                                       else d

          f (Left (i, as)) (d, r') = Fold.foldr (deep i) (d, r') as
          f (Right as)     (d, r') = (,) d $ Fold.foldr (uncurry insertGut) r' as

      in case deleteNode 0 bz x of
           Just (Just (bb, b), reins) ->
             snd $ Fold.foldr f (0, Root bb b) reins

           Just (Nothing, reins) ->
             case reins of
               Left (d, as)    : rs -> let r' = Node $ NonEmpty.fromList as
                                       in snd $ Fold.foldr f (d, Root (union r') r') rs
               Right [(ba, a)] : [] -> Leaf1 ba a
               Right as        : [] -> Fold.foldr (uncurry insertGut) R.empty as
               Right _         : _  -> errorWithoutStackTrace "Data.RTree.Lazy.delete: leftovers"
               []                   -> errorWithoutStackTrace "Data.RTree.Lazy.delete: no reinserts"

           Nothing -> r

    Leaf1 ba _ | ba == bz  -> Empty
               | otherwise -> r

    Empty -> Empty



deleteNode
  :: (Num r, Ord r)
  => Int
  -> MBR r
  -> Node r a
  -> Maybe (Maybe (MBR r, Node r a), [Either (Int, [(MBR r, Node r a)]) [(MBR r, a)]])
deleteNode d bz x =
  let withU a = (union a, a)
  in case x of
       Node as ->
         let f (ba, a, i) | MBR.contains bz ba
                          , Just (mayB, ins) <- deleteNode (d + 1) bz a
                              = First $ Just (i, mayB, ins)

                          | otherwise = First Nothing

         in case getFirst . Fold.foldMap f $
                              NonEmpty.zipWith (\(by, y) i -> (by, y, i)) as (0 :| [1..]) of
              Nothing                     -> Nothing
              Just (i, Just (bb, b), ins) ->
                Just (Just . withU . Node . NonEmpty.fromList $ swap i (bb, b) as, ins)
              Just (i, Nothing, ins) ->
                Just $
                  case () of
                    () | _ :| [] <- as                -> (Nothing, ins)
                       | NonEmpty.length as <= smallM -> (Nothing, Left (d, discard i as) : ins)
                       | otherwise   ->
                           (Just . withU . Node . NonEmpty.fromList $ discard i as, ins)

       Leaf as ->
         let f (ba, i) | ba == bz  = First $ Just i
                       | otherwise = First Nothing

         in case getFirst . Fold.foldMap f $
                              NonEmpty.zipWith (\(by, _) i -> (by, i)) as (0 :| [1..]) of
              Nothing -> Nothing
              Just i ->
                Just $ if NonEmpty.length as <= smallM
                         then (Nothing, pure . Right $ discard i as)
                         else (Just . withU . Leaf . NonEmpty.fromList $ discard i as, [])
