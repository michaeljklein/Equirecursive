{-# LANGUAGE TypeInType #-}

module Data.Recurse (Recurse(..), RecurseL, RecurseU, unlock, lock) where

import Data.Kind
import Data.Type.Equality
import Control.Comonad
import Data.Locking
-- import Unsafe.Coerce

-- | Do not export constructors or destructors
newtype Recurse (l :: Locking) (a :: *) = Recurse { getRecurse :: a }

-- | Convenience alias
type RecurseL a = Recurse 'Locked   a

-- | Convenience alias
type RecurseU a = Recurse 'Unlocked a

-- | Lock a `RecurseU`.
-- Make into a class. Also have @`lock` :: `X` a -> `X` (b :: k -> *)@
lock :: RecurseU a -> RecurseL a
lock (Recurse x) = Recurse x

-- | Do not export!!!
unlock :: RecurseL a -> RecurseU a
unlock (Recurse x) = Recurse x

-- | `Recurse` `Unlocked` is trivially a `Functor`
instance Functor (Recurse 'Unlocked) where
  fmap f (Recurse x) = Recurse (f x)

-- | `Recurse` `Unlocked` is trivially an `Applicative`
instance Applicative (Recurse 'Unlocked) where
  pure = Recurse
  Recurse f <*> Recurse x = Recurse (f x)

-- | `Recurse` `Unlocked` is trivially a `Monad`
instance Monad (Recurse 'Unlocked) where
  return = pure
  Recurse x >>= f = f x

-- | `Recurse` `Unlocked` is trivially a `Comonad`
instance Comonad (Recurse 'Unlocked) where
  extract (Recurse x) = x
  duplicate = Recurse
  extend f = Recurse . f

