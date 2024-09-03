# r-tree [![Hackage](http://img.shields.io/hackage/v/r-tree.svg)](https://hackage.haskell.org/package/r-tree)

A Haskell library for [R-](https://en.wikipedia.org/wiki/R-tree) and [R\*-trees](https://en.wikipedia.org/wiki/R\*-tree).

Featuring:

- `Data.R2Tree.*`: spine-strict two-dimensional R-tree.

  `Double`-based implementation is considered the default one;
  a `Float`-based variant is provided for cases where reduced precision is preferred,
  for example rendering.

Three-dimensional R-trees are not currently available,
but should be trivial to add if needed.
