module Data.Equirecursive.List where

import Data.Equirecursive
import Data.List


newtype EList a = EList { getEList :: RecurseL (a, XY) }

-- What about using an existential type to hide the expansion?
-- What constraints would I want?

instance Functor EList where
  fmap = undefined

instance Applicative EList where
  pure = ecycle . return
  (<*>) = undefined

instance Monad EList where
  return = pure
  (>>=) = undefined

instance Foldable EList where
  foldr = undefined

-- instance Traversable EList

ecycle :: [a] -> EList a
ecycle = undefined

ehead :: EList a -> a
ehead = undefined

-- | I think I might use pullN :: NatWrapper -> a -> PullN Nat a?
eindex :: Int -> EList a -> Maybe a
eindex = undefined

etail :: EList a -> EList a
etail = undefined

euncons :: EList a -> (a, EList a)
euncons = undefined

fromList :: [a] -> EList a
fromList = ecycle

toList :: EList a -> [a]
toList = unfoldr (return . euncons)
