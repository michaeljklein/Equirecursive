{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}

module Data.Rec (Rec (..)) where

-- | An equirecursive data type.
-- `Rec` should not support unwrapping,
-- but unfolding is possible. E.g.:
-- @
-- `Rec` (c (`XX` k)) -> c (`Rec` (c (`XX` k)))
-- @
data Rec (a :: *) = Rec { getRec :: a }

data Locking = Unlocked | Locked deriving (Eq, Ord, Show)

-- | Not sure about the constraints, but should be able to force
-- UnlockedRec to only work when the type doesn't contain any `XX`s.
-- data Rec2 (l :: Locking) (a :: *) where
--   LockedRec   ::                                a -> Rec2 Locked   a
--   UnlockedRec :: XMap a a (XX Type) (Rec ()) => a -> Rec2 Unlocked a


-- | This may be unsafe and should be checked.
-- (It's possible that a recursion endpoint could be killed..)
-- Possible solution is to parametrize, where Rec = Rec Locked, and
-- @`Functor` (`Rec` Unlocked)@
instance Functor Rec where
  fmap f (Rec x) = Rec (f x)



