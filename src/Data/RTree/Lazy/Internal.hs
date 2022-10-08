module Data.RTree.Lazy.Internal
  ( insertGut
  , delete
  ) where

import qualified Data.Primitive.Array.Extra as Array
import           Data.RTree.Internal as R
import           Data.RTree.Internal.Constants
import qualified Data.RTree.MBR as MBR

import           Control.Monad
import           Data.Foldable as Fold
import           Data.Monoid
import           Data.Ord
import           Prelude hiding (Foldable (..))
import           Data.Primitive.Array
import           Data.Primitive.Types



-- | \(O (\log_M n)\). Same as 'Data.Map.Lazy.insert',
--   using Guttman's original insertion algorithm.
insertGut :: (Num r, Ord r, Prim r) => MBR r -> a -> RTree r a -> RTree r a
insertGut bz z t =
  case t of
    Root _ x   ->
      case insertGutNode bz z x of
        Right (ba, a)       -> Root ba a
        Left (bl, l, br, r) -> Root (MBR.union bl br) $ mk Node [(bl, l), (br, r)]

    Leaf1 ba a -> Root (MBR.union ba bz) $ mk Leaf [(ba, a), (bz, z)]
    Empty      -> Leaf1 bz z



insertGutNode
  :: (Num r, Ord r, Prim r)
  => MBR r
  -> a
  -> Node r a
  -> Either (MBR r, Node r a, MBR r, Node r a) (MBR r, Node r a)
insertGutNode bz z x =
  let withR a      = Right (union a, a)
      withL (l, r) = Left (union l, l, union r, r)
  in case x of
       Node n brs as ->
         let i = leastEnlargement bz n brs
         in case insertGutNode bz z $ indexArray as i of
              Right (ba, a)   -> withR $ replace Node n brs as i ba a

              Left (bl, l, br, r)
                | n < bigM  -> withR $ replaceSnoc Node n brs as i bl l br r
                | otherwise -> withL . split Node quad $ (bl, l) : (br, r) : discard i (nodes n brs as)

       Leaf n brs as
         | n < bigM  -> withR $ snoc Leaf n brs as bz z
         | otherwise -> withL . split Leaf quad $ (bz, z) : nodes n brs as




insertDepth :: (Num r, Ord r, Prim r) => Int -> MBR r -> Node r a -> RTree r a -> (Bool, RTree r a)
insertDepth d bz z t =
  case t of
    Root _ x  ->
      case insertDepthNode d bz z x of
        Right (ba, a)       -> (,) False $ Root ba a
        Left (bl, l, br, r) -> (,) True  . Root (MBR.union bl br) $ mk Node [(bl, l), (br, r)]

    Empty     -> (,) False $ Root bz z
    Leaf1 _ _ -> errorWithoutStackTrace "Data.RTree.Lazy.insertDepth: leaf on root level"



insertDepthNode
  :: (Num r, Ord r, Prim r)
  => Int -> MBR r -> Node r a -> Node r a -> Either (MBR r, Node r a, MBR r, Node r a) (MBR r, Node r a)
insertDepthNode d bz z x =
  let withR a = Right (union a, a)
      withL (l, r) = Left (union l, l, union r, r)
  in case x of
       Node n brs as
         | d <= 0    ->
             if n < bigM
               then withR $ snoc Node n brs as bz z
               else withL . split Node quad $ (bz, z) : nodes n brs as

         | otherwise ->
             let i = leastEnlargement bz n brs
             in case insertDepthNode (d - 1) bz z $ indexArray as i of
                  Right (ba, a)   -> withR $ replace Node n brs as i ba a
                  Left (bl, l, br, r)
                    | n < bigM  -> withR $ replaceSnoc Node n brs as i bl l br r
                    | otherwise -> withL . split Node quad $ (bl, l) : (br, r) : discard i (nodes n brs as)

       Leaf _ _ _ -> errorWithoutStackTrace $ "Data.RTree.Lazy.insertDepth: reached a leaf"



-- | \(O (\log_M n)\). Delete the first occurrence of a given bounding rectangle, if one exists.
--
--   'delete' uses Guttman's original deletion and (re)insertion algorithms.
delete :: (Num r, Ord r, Prim r) => MBR r -> RTree r a -> RTree r a
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
               Left (d, as)    : rs -> let r' = mk Node as
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
  :: (Num r, Ord r, Prim r)
  => Int
  -> MBR r
  -> Node r a
  -> Maybe (Maybe (MBR r, Node r a), [Either (Int, [(MBR r, Node r a)]) [(MBR r, a)]])
deleteNode d bz x =
  let withU a = (union a, a)
  in case x of
       Node n brs as ->
         let f i ba a
               | MBR.contains bz ba
               , Just (mayB, ins) <- deleteNode (d + 1) bz a
                   = First $ Just (i, mayB, ins)

               | otherwise          = First Nothing
         in case getFirst $ Array.izipMap f n brs as of
              Nothing                     -> Nothing
              Just (i, Just (bb, b), ins) ->
                Just (Just . withU $ replace Node n brs as i bb b, ins)
              Just (i, Nothing, ins) ->
                Just $ case () of
                         () | n <= 1      -> (Nothing, ins)
                            | n <= smallM -> (Nothing, Left (d, discard i $ nodes n brs as) : ins)
                            | otherwise   -> (Just . withU $ lose Node n brs as i, ins)

       Leaf n brs as ->
         case getFirst $ Array.ifoldMap (\i ba -> First $ i <$ guard (ba == bz)) n brs of
           Nothing -> Nothing
           Just i  ->
             Just $ if n <= smallM
                      then (Nothing, pure . Right . discard i $ nodes n brs as)
                      else (Just . withU $ lose Leaf n brs as i, [])
