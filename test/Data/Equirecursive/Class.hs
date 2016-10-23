{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

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

