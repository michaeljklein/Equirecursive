module Data.Equirecursive.Class where

import Unsafe.Coerce

import Control.Lens.Iso

class Pull s t a b where
  pull :: Iso s t a b
  push :: Iso b a t s

-- type Iso s t a b = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)

-- Profunctor p
-- dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
-- lmap :: (a -> b) -> p b c -> p a c
-- rmap :: (b -> c) -> p a b -> p a c

-- -- | This needs some updating.
-- class Pull a b | a -> b, b -> a where
--   pull :: a -> b
--   {-# INLINE pull #-}
--   pull = unsafeCoerce

--   push :: b -> a
--   {-# INLINE push #-}
--   push = unsafeCoerce

--   pullMap ::

