{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}


module Data.Rec (Rec (..)) where

import Control.Arrow
import Data.Kind

-- | Stub for prototyping, remove!
type XX k = k

data Rec (a :: *) = Rec { getRec :: a }

instance Functor Rec where
  fmap f (Rec x) = Rec (f x)

t1 :: forall t. t -> (Integer, t)
t1 = \x -> (0, x)

t1' :: forall t. (Integer, Rec t) -> (Integer, Rec (Integer, Rec t))
t1' = \x -> (0, Rec x)

r1 :: Rec (Integer, XX k)
r1 = undefined

-- | Trivial conversion
r1' :: Rec (Integer, XX k)
r1' = undefined


t2 :: forall t. (Integer, t) -> (Integer, (Integer, t))
t2 = \x -> (0, first (+1) x)

t2' :: forall t. (Integer, Rec t) -> (Integer, Rec (Integer, Rec t))
t2' = \x -> (0, Rec $ first (+1) x)

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

t4' :: forall t. (Rec t, Rec t, Integer) -> (Rec (Rec t, Rec t, Integer), Rec (Rec t, Rec t, Integer), Integer)
t4' = undefined -- \x -> (fmap (*1) <$> x, fmap ((+1).(*2)) <$> x, 0)

r4 :: Rec ((XX k, XX k, Integer), (XX k, XX k, Integer), Integer)
r4 = undefined

r4' :: Rec (XX k, XX k, Integer)
r4' = undefined


-- Here is the control flow for deriving the types:
-- Rec (a, XX k) ->         Given in class
--   (a, XX k) ->           getRec
--     (a, Rec t)           over xmap (_ :: XX k -> Rec t)
--     (a, (a, XX k)) ->    over xmap (_ :: XX k -> (a, XX k))
--       (a, (a, Rec t))  over xmap (_ :: XX k -> Rec t)
--
-- By doing a type family application, we get: forall t. (a, Rec t) -> (a, (a, Rec t))
--
-- We then have to generically bind a function of this type into an infinite loop
-- and properly coerce to Rec (a, XX k).
--
-- If we coerce away the Rec's, we get (a, t) -> (a, (a, t)), which can be recursed over.
-- However, hmmmmmmm.
--
-- *: (a, Rec t) -> (a,     (a, Rec t))
-- 1: (a, Rec t) -> (a, Rec (a, Rec t))
-- 2: (a, XX  k) -> (a, Rec (a, XX  k))
-- 3: (a, XX  k) -> (a,         XX  k)
--
-- Here's where the ymap's come in. I believe that the * case
-- makes it impossible to coerce further. However, it's possible
-- that the 2nd case can be skipped.


-- Rec (a, XX k) ->
-- forall t. (a, t) -> (a, (a, t))
--   ||
-- forall t. (a, Rec t) -> (a, (a, Rec t))
--   ||
-- forall t. (a, Rec t) -> (a, Rec (a, t))

-- (a, XX k) -> (a, Rec t)
-- (a, XX k) -> (a, Rec (a, XX k)) -> (a, Rec (a, Rec t))

-- Then, we only need Rec to be a functor, though possibly limited, or even keeping it even more general and
-- giving an iso to Rec's not containing XX. The definitions will have to be modified to account for moving inside
-- Rec, but it should work fine.

-- there are two types of recursion:
-- t -> c t, c t -> c (c t)
-- The first type is effectively trivial, and is more
-- general type-wise than the second, but the second is
-- more general functionality wise.
--
-- One method is to apply to the least common denominator and always
-- require the second. The other method is to attempt to double-up on the instances
-- since there's a type-level iso between them. What about a value-level iso?
--
-- Going from the first to the second is easy, but how do we differentiate between
-- the two? Ahhhhh.... We don't have to!! We have:
--  id = (t -> c t) -> (c t -> c (c t))
-- So we can ignore the first as far as constructors, but we should still simplify the
-- r-types to r'-types.


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




