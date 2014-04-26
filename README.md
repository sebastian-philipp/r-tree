R-Trees (and in the future also R*-Trees)
====================

R-Tree is a spartial data structure similar to Quadtrees or B-Trees.

An R-Tree is a balanced tree and optimized for lookups. This implemetation useses an R-Tree to privide
a map to arbitrary values.

Some function names clash with "Prelude" names, therefore this module is usually
imported ```qualified```, e.g.:

    > import           Data.RTree (RTree)
    > import qualified Data.RTree as RT

this implemetation is incomplete at the moment. Feel free to send comments, patches or merge requests.
