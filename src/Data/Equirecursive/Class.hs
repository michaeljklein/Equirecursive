{-# LANGUAGE TemplateHaskell #-}

module Data.Equirecursive.Class where

import Unsafe.Coerce
import Control.Lens.Iso

import Data.Recurse
import Data.X
import Data.Kind
import Control.Lens

import Data.Equirecursive.Class.TH
import Data.Functor.Identity
import Language.Haskell.TH (litE, stringL)

-- tt1 = $(lookupData ''Identity)
-- tt2 = $(lookupData ''Maybe)
-- tt3 = $(((litE . stringL . show) =<<) $ dataExp [| Either XY |])
-- tt4 = $(((litE . stringL . show) =<<) $ dataExp [| (a, XY) |])

-- tt4 = $(lookupData ''Int)

-- | Don't forget pullN, Next
class Pull s t a b | s -> a, t -> b, a -> s, b -> t, s b -> t, t a -> s where
  pull :: Iso s t a b
  {-# INLINE pull #-}
  pull = unsafeCoerce

  push :: Iso b a t s
  {-# INLINE push #-}
  push = unsafeCoerce

-- instance Pull (RecurseL (a, XY)) (RecurseL (b, XY)) (a, RecurseL (a, XY)) (b, RecurseL (b, XY))
-- instance Pull (RecurseL (XY, a)) (RecurseL (XY, b)) (RecurseL (XY, a), a) (RecurseL (XY, b), b)

-- | Wrap the polymorphic types in (X :: Type -> Type) and
-- it works great!
type family P (a :: Type) (b :: k) :: k where
  P a (X Y) =    a
  P a (X b) =              b
  P a (c b) = (P a c) (P a b)
  P a (  b) =              b

type family UnY (a :: k) :: k where
  UnY (X Y) = X Y
  UnY (X b) = b
  UnY (c b) = (UnY c) (UnY b)
  UnY (  b) = b

-- | I think there's a way to get this working with derive Generic?

class Pull1 f una unb a b | una -> f, unb -> f, una -> a, unb -> b, una b -> unb, unb a -> una where
  pull1 :: Lens (f a) (f b) (P (f a) una) (P (f b) unb)

class Pull2 f una unb a a' b b' | una -> f, unb -> f, una -> a, una -> a', unb -> b, unb -> b', una b -> b, una b b' -> b', unb a a' -> una where
  pull2 :: Lens (f a a') (f b b') (P (f a a') una) (P (f b b') unb)

-- instance Pull2 (,) (a, a') (b, b') a a' b b' where
--   pull2 = undefined

-- instance Pull2 (,) (XY, a) (XY, b) a b where
--   pull2 = undefined


  -- pullN :: X (n :: Nat) -> _
  -- pushN :: X (n :: Nat) -> _

  -- | Pull as many times as it takes
  -- pullInf :: c (.. c (RecurseL a)) -> RecurseL a

  -- | Push as many times as it takes
  -- pushInf

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

