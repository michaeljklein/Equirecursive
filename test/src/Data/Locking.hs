module Data.Locking where

import Data.Type.Equality

data Locking = Unlocked | Locked deriving (Eq, Ord, Show)

type family EqLocking a b where
  EqLocking 'Unlocked 'Unlocked = 'True
  EqLocking 'Unlocked 'Locked   = 'False
  EqLocking 'Locked   'Unlocked = 'False
  EqLocking 'Locked   'Locked   = 'True

type instance a == b = EqLocking a b


