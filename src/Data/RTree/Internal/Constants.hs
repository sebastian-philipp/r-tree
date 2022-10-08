{- | 
     References to books mentioned here can be found in "Data.RTree.Lazy".
-}

module Data.RTree.Internal.Constants where



{-# INLINEABLE smallM #-}
-- | Constant \(m\) (default \(2\)): minimum number of entries within any node.
-- 
--   Defined within [Gut 84, page 48].
--
--   The following equation __must__ hold: \(m \leq \frac{M}{2}\).
smallM :: Int
smallM = 2

{-# INLINEABLE bigM #-}
-- | Constant \(M\) (default \(4\)): maximum number of entries within any node.
--
--   Defined within [Gut 84, page 48].
bigM :: Int
bigM = 4

{-# INLINEABLE smallP #-}
-- | Constant \(p\) (default \(1\)): number of entries reinserted on overflow.
--
--   Defined within [BKSS 90, page 327], which suggests \(p \approx 30\% \cdot M\).
smallP :: Int
smallP = 1
