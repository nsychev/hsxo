module Hsxo.Util
  ( intLens
  , maybeTraversal
  , enumerate
  ) where

import Lens.Micro (Lens', lens, Traversal')

-- Utility lens to convert different integrals.
intLens :: Integral a => Integral b => Lens' a b
intLens = lens fromIntegral $ const fromIntegral


-- Makes traversal for Maybe.
maybeTraversal :: Traversal' (Maybe a) a
maybeTraversal f (Just x) = Just <$> f x
maybeTraversal _ Nothing = pure Nothing


-- Enumerates lists.
enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]