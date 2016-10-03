{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}


module Data.Recurse (Rec (..), Recurse(..), Locking(..), RecurseL, RecurseU, unlock, lock) where

import Data.Kind
import Data.Type.Equality
import Control.Comonad

-- import Data.X.Map (XMap(..))

-- | An equirecursive data type.
-- `Rec` should not support unwrapping,
-- but unfolding is possible. E.g.:
-- @
-- `Rec` (c (`XX` k)) -> c (`Rec` (c (`XX` k)))
-- @
data Rec (a :: *) = Rec { getRec :: a }

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
data Recurse (l :: Locking) (a :: *) where
  RecurseLocked   :: a -> Recurse 'Locked   a
  RecurseUnlocked :: a -> Recurse 'Unlocked a

type RecurseL a = Recurse 'Locked   a
type RecurseU a = Recurse 'Unlocked a

lock :: RecurseU a -> RecurseL a
lock (RecurseUnlocked x) = RecurseLocked x

unlock :: RecurseL a -> RecurseU a
unlock (RecurseLocked x) = RecurseUnlocked x

instance Functor (Recurse 'Unlocked) where
  fmap f (RecurseUnlocked x) = RecurseUnlocked (f x)

instance Applicative (Recurse 'Unlocked) where
  pure = RecurseUnlocked
  RecurseUnlocked f <*> RecurseUnlocked x = RecurseUnlocked (f x)

instance Monad (Recurse 'Unlocked) where
  return = pure
  RecurseUnlocked x >>= f = f x

instance Comonad (Recurse 'Unlocked) where
  extract (RecurseUnlocked x) = x
  duplicate = RecurseUnlocked
  extend f = RecurseUnlocked . f

-- | This may be unsafe and should be checked.
-- (It's possible that a recursion endpoint could be killed..)
-- Possible solution is to parametrize, where Rec = Rec Locked, and
-- @`Functor` (`Rec` Unlocked)@
instance Functor Rec where
  fmap f (Rec x) = Rec (f x)



