module Data.RTree.Internal.Pretty where

import           Data.RTree.Internal as R

import           Data.Foldable as Fold
import           Data.Primitive.Types



-- | Pretty-print the tree into @stdout@.
pretty :: (Prim r, Show r, Show a) => RTree r a -> IO ()
pretty = Fold.traverse_ (putStrLn . indent) . root

pretty' :: (Prim r, Show r, Show a) => RTree r a -> String
pretty' = Fold.foldMap indent . root

indent :: Line -> String
indent (Line i s) = replicate (2 * i 0) ' ' <> s

data Line = Line
              (Int -> Int) -- ^ Relative indentation of the line
              String

plus :: Line -> Line
plus (Line i s) = Line ((+1) . i) s

root :: (Prim r, Show r, Show a) => RTree r a -> [Line]
root r =
  case r of
    Root bs s  -> node bs s
    Leaf1 bb a -> pure . Line id $ "Leaf1 " <> mbr bb <> " " <> show a
    Empty      -> pure $ Line id "Empty"

node :: (Prim r, Show r, Show a) => MBR r -> Node r a -> [Line]
node bs s =
  case s of
    Node n brs as ->
      (:) (Line id $ "Node" <> show n <> " " <> mbr bs) $
        plus <$> Fold.foldMap (uncurry node) (nodes n brs as)
    Leaf n brs as ->
      (:) (Line id $ "Leaf" <> show n <> " " <> mbr bs) $
        plus . (\(br, a) -> Line id $ mbr br <> " " <> show a) <$> nodes n brs as

mbr :: (Prim r, Show r) => MBR r -> String
mbr (MBR xmin ymin xmax ymax) = mconcat [ "(", show xmin, ",", show ymin
                                        , ",", show xmax, ",", show ymax, ")" ]
