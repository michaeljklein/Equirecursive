{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}


module Data.Recurse (Recurse(..), Locking(..), RecurseL, RecurseU, unlock, lock) where

import Data.Kind
import Data.Type.Equality
import Control.Comonad
import Data.X
import Unsafe.Coerce

-- import Data.X.Map (XMap(..))

-- TODO: Should make Recurse 'Locked into a monad? hmmmmm.... would like to give a nice way to return.....


-- | An equirecursive data type.
-- `Rec` should not support unwrapping,
-- but unfolding is possible. E.g.:
-- @
-- `Rec` (c (`XX` k)) -> c (`Rec` (c (`XX` k)))
-- @
-- data Rec (a :: *) = Rec { getRec :: a }

data Locking = Unlocked | Locked deriving (Eq, Ord, Show)

type family EqLocking a b where
  EqLocking 'Unlocked 'Unlocked = 'True
  EqLocking 'Unlocked 'Locked   = 'False
  EqLocking 'Locked   'Unlocked = 'False
  EqLocking 'Locked   'Locked   = 'True

type instance a == b = EqLocking a b


-- | Not sure about the constraints, but should be able to force
-- UnlockedRec to only work when the type doesn't contain any `XX`s.
-- Also, do no export anything that makes a `Locked` `Recurse`.
-- This will allow one to use `Recurse` in `Recursing`, but not
-- within the final equirecursive function.
-- data Recurse (l :: Locking) (a :: *) where
--   RecurseLocked   :: a -> Recurse 'Locked   a
--   RecurseUnlocked :: a -> Recurse 'Unlocked a

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

lockA :: forall a b. (a -> b) -> (RecurseU a -> XY)
lockA _ (Recurse x) = X (unsafeCoerce x)


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


-- | This may be unsafe and should be checked.
-- (It's possible that a recursion endpoint could be killed..)
-- Possible solution is to parametrize, where Rec = Rec Locked, and
-- @`Functor` (`Rec` Unlocked)@
-- instance Functor Rec where
--   fmap f (Rec x) = Rec (f x)



