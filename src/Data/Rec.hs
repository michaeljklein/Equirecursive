{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}


module Data.Rec (Rec (..)) where

import Control.Arrow
import Data.Kind

-- | Stub for prototyping, remove!
type XX k = k

data Rec (a :: *) = Rec { getRec :: a }

t1 :: forall t. t -> (Integer, t)
t1 = \x -> (0, x)

r1 :: Rec (Integer, XX k)
r1 = undefined

-- | Trivial conversion
r1' :: Rec (Integer, XX k)
r1' = undefined


t2 :: forall t. (Integer, t) -> (Integer, (Integer, t))
t2 = \x -> (0, first (+1) x)

r2 :: Rec (Integer, (Integer, XX k))
r2 = undefined

r2' :: Rec (Integer, XX k)
r2' = undefined


-- Not sure about this one
t3 :: forall a d. (a -> (Integer, d)) -> Integer -> (Integer, a -> (Integer, d))
t3 = \x -> \y -> (y + 1, (first ((+1)$) . x))

instance Functor ((,,) a b) where
  fmap f (x,y,z) = (x,y,f z)

t4 :: forall t. (t, t, Integer) -> ((t, t, Integer), (t, t, Integer), Integer)
t4 = \x -> ((*2) <$> x, (+1).(*2) <$> x, 0)

r4 :: Rec ((XX k, XX k, Integer), (XX k, XX k, Integer), Integer)
r4 = undefined

r4' :: Rec (XX k, XX k, Integer)
r4' = undefined

-- there are two types of recursion:
-- t -> c t, c t -> c (c t)
-- The first type is effectively trivial, and is more
-- general type-wise than the second, but the second is
-- more general functionality wise.
--
-- One method is to apply to the least common denominator and always
-- require the second. The other method is to attempt to double-up on the instances
-- since there's a type-level iso between them. What about a value-level iso?


-- There's really only one way to do this: We need to replace the first argument of
-- the t-types with XX k in the entire t-type.
-- Why only one way? Without depending on the first argument, we can't tell the difference between
-- (1,(2,(1,(2...)))) and (1, (([_,2,1]!!), ...)).
-- The first is static, simple, recursion. the second references previous values.


-- Compare r4 and r4'. The type of r4 tells us that the recursive function
-- references its input (when viewed vs. t4). The type of r4' hides that
-- information, but may be derived with more ease?

-- One clear point is that the non-prime r-types contain more information than is needed once
-- wrapped in Rec.
--
-- How can we convert the t-types into the r'-types??

-- Should this one be an iso?
-- Hmmmm.. Probably not.
-- We do need to 1) know how to get from ti -> ri, but that "might" be as easy as (`asTypeOf` t4 xX)
-- 2) do we need to be able to get back?

-- class Recurse




