module Test.Kit
  ( Case (..)
  , augment

  , Test (..)

  , run
  , dump
  ) where

import           Control.Exception
import           Data.Foldable



data Case s a b = Case s a b

augment :: (s -> t) -> [Case s a b] -> [Case t a b]
augment f xs = fmap (\(Case s a b) -> Case (f s) a b) xs



data Test s a b x y = Test (x -> y -> Bool) (s -> a -> x) (s -> b -> y)



newtype Failure = Failure Int

instance Show Failure where
  showsPrec _ (Failure n) = showString "Test failed on case " . shows n

instance Exception Failure



newtype UnknownIndex = UnknownIndex Int

instance Show UnknownIndex where
  showsPrec _ (UnknownIndex n) = showString "No case under index " . shows n

instance Exception UnknownIndex



enumerate :: [Case s a b] -> [(Int, Case s a b)]
enumerate = zip [0..]

run :: [Case s a b] -> Test s a b x y -> IO ()
run cs (Test cmp f g) = traverse_ go $ enumerate cs
  where
    go (n, Case s a b) =
      if cmp (f s a) (g s b)
        then pure ()
        else throwIO (Failure n)

dump :: [Case s a b] -> Test s a b x y -> Int -> IO (s, a, b, x, y)
dump xs (Test _ f g) n =
  case lookup n (enumerate xs) of
    Just (Case s a b) -> pure (s, a, b, f s a, g s b)
    Nothing           -> throwIO (UnknownIndex n)
