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


-- | Does this work for unboxed tuples? Apparently not really.
newtype EList a = EList { getEList :: RecurseL (a, XY) }


instance Pull (EList a) (a, EList a)


instance Functor EList where
  fmap f = push . bimap f (fmap f) . pull


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
      join x = let (xs, xss) = pull x in push (ehead xs, join (fmap etail xss))

instance MonadZip EList where
  mzip :: EList a -> EList b -> EList (a, b)
  mzip x y = go x y
    where
      go x y = let ((z, zs), (w, ws)) = (euncons x, euncons y) in push ((z,w), go zs ws)


instance Foldable EList where
  {-# INLINE [0] foldr #-}
  foldr f xs = go
    where
      go ys = let (z, zs) = pull ys in z `f` go zs


instance Traversable EList where
  sequenceA :: Applicative f => EList (f a) -> f (EList a)
  sequenceA = fmap push . sequenceTup . fmap sequenceA . pull


-- | If bitraversable were a thing... TODO: They are!!!
sequenceTup :: Applicative f => (f a, f b) -> f (a, b)
sequenceTup = uncurry $ liftA2 (,)


ecycle :: [a] -> EList a
ecycle xs = go xs
  where
    go (y:ys) = push (y, go ys)
    go  _     =          go xs

ehead :: EList a -> a
ehead = fst . pull

-- | I think I might use pullN :: NatWrapper -> a -> PullN Nat a?
eindex :: Int -> EList a -> Maybe a
eindex i xs | i < 0     = Nothing
            | i == 0     = Just (ehead xs)
            | otherwise = eindex (i - 1) (etail xs)

etail :: EList a -> EList a
etail = snd . pull

euncons :: EList a -> (a, EList a)
euncons = pull

fromList :: [a] -> EList a
fromList = ecycle

toList :: EList a -> [a]
toList = unfoldr (return . euncons)
