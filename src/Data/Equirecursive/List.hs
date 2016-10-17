{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Equirecursive.List where

import Data.Equirecursive
import Data.List

-- import Data.Recurse.Recursing
import Unsafe.Coerce
import Data.Coerce
import Data.Bifunctor
import Control.Applicative
import Control.Monad.Zip
import Data.Equirecursive.Class
import Control.Lens
import Data.Bitraversable


-- | Does this work for unboxed tuples? Apparently not really.
newtype EList a = EList { getEList :: RecurseL (a, XY) }


instance Pull (EList a) (EList b) (a, EList a) (b, EList b)


instance Functor EList where
  fmap f = pull %~ bimap f (fmap f)

-- | `pure` is just `repeat`. @(`<*>`)@ zips the functions
instance Applicative EList where
  pure = ecycle . return
  (<*>) = mzipWith ($)

instance Monad EList where
  return = pure
  (>>=) :: EList a -> (a -> EList b) -> EList b
  xs >>= f = join (fmap f xs)
    where
      join :: EList (EList a) -> EList a
      join = pull %~ bimap ehead (join . fmap etail)

instance MonadZip EList where
  mzip :: EList a -> EList b -> EList (a, b)
  mzip x y = let ((z, zs), (w, ws)) = (euncons x, euncons y) in ((z,w), mzip zs ws) ^. push

instance Foldable EList where
  {-# INLINE [0] foldr #-}
  foldr f xs = go
    where
      go ys = let (z, zs) = ys ^. pull in z `f` go zs


instance Traversable EList where
  sequenceA :: Applicative f => EList (f a) -> f (EList a)
  sequenceA = pull $ bisequenceA . fmap sequenceA


ecycle :: [a] -> EList a
ecycle xs = go xs
  where
    go (y:ys) = (y, go ys) ^. push
    go  _     =     go xs

ehead :: EList a -> a
ehead = fst . euncons

-- | I think I might use pullN :: NatWrapper -> a -> PullN Nat a?
eindex :: Int -> EList a -> Maybe a
eindex i xs | i < 0     = Nothing
            | i == 0     = Just (ehead xs)
            | otherwise = eindex (i - 1) (etail xs)

etail :: EList a -> EList a
etail = snd . euncons

euncons :: EList a -> (a, EList a)
euncons = (^. pull)

fromList :: [a] -> EList a
fromList = ecycle

toList :: EList a -> [a]
toList = unfoldr (return . euncons)


