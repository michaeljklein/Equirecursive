{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}


module Data.Rec where

import Control.Arrow
import Data.Kind

-- | Stub for prototyping, remove!
type XX k = k

data Rec (a :: *) = Rec { getRec :: a }

t1 :: forall t. t -> (Integer, t)
t1 = \x -> (0, x)

r1 :: Rec (Int, XX k)
r1 = undefined

t2 :: forall t. (Integer, t) -> (Integer, (Integer, t))
t2 = \x -> (0, first (+1) x)

-- Not sure about this one
t3 :: forall a d. (a -> (Integer, d)) -> Integer -> (Integer, a -> (Integer, d))
t3 = \x -> \y -> (y + 1, (first ((+1)$) . x))

instance Functor ((,,) a b) where
  fmap f (x,y,z) = (x,y,f z)

t4 :: forall t. (t, t, Integer) -> ((t, t, Integer), (t, t, Integer), Integer)
t4 = \x -> ((*2) <$> x, (+1).(*2) <$> x, 0)
-- class Recurse




