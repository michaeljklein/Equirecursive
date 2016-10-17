module Data.Equirecursive.Class where

import Unsafe.Coerce
import Control.Lens.Iso

import Data.Recurse
import Data.X

-- | Don't forget pullN, Next
class Pull s t a b | s -> a, t -> b, a -> s, b -> t, s b -> t, t a -> s where
  pull :: Iso s t a b
  {-# INLINE pull #-}
  pull = unsafeCoerce

  push :: Iso b a t s
  {-# INLINE push #-}
  push = unsafeCoerce



-- newtype EList a = EList { getEList :: RecurseL (a, XY) }

-- instance Pull (EList a) (EList b) (a, EList a) (b, EList b) where
--   pull :: forall p f. (Profunctor p, Functor f) => p (a, EList a) (f (b, EList b)) -> p (EList a) (f (EList b))
--   pull = unsafeCoerce


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

