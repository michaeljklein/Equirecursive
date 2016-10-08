{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}

{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Recurse.Equality where

import Data.Type.Equality
import Data.Kind
import GHC.Generics
-- import Data.Recurse
import Data.Recurse.Recursing
import Data.Lifted
import Data.X


-----------------------------------------------------------------------------------

-- | Likely unfinished, but this is will be the interface to the result of this module
-- class (Req a b ~ eq) => RecurseEq a b eq where
--   req :: a -> b -> Maybe (a :~: b)
-- instance (Req a b ~ 'True) => RecurseEq a b 'True where
--   req _ _ = Just Refl
-- instance (Req a b ~ 'False) => RecurseEq a b 'False where
--   req _ _ = Nothing

-----------------------------------------------------------------------------------

-- | Recurse, checking equality until `XY` is reached.
-- Then, pass the lower depth to the second stage.
type family Req (ar :: *) (a :: *) (br :: *) (b :: *) (d :: Nat) :: Bool

type family ReqExpand (ar :: *) (br :: *) (b :: *) (d :: Nat) where
  ReqExpand ar br XY d = 'True -- Is this really always True? Yup, if a == b, then True.
  ReqExpand ar br b  d = Deq ar ar br b d
type instance Req ar XY br b d = ReqExpand ar br b d

type family ReqV1 (ar :: *) (a :: *) (br :: *) (b :: *) (d :: Nat) where
  ReqV1 ar a br (V1 b) d = Req ar a  br b ('S d)
  ReqV1 ar a br    XY  d = Deq br XY ar a     d
  ReqV1 ar a br     b  d = 'False
type instance Req ar (V1 a) br b d = ReqV1 ar a br b d

type family ReqTuple2 (ar :: *) (a0 :: *) (a1 :: *) (br :: *) (b :: *) (d :: Nat) where
  ReqTuple2 ar a0 a1 br (b0, b1) d = Req ar a0 br b0 ('S d) :&& Req ar a1 br b1 ('S d)
  ReqTuple2 ar a0 a1 br  XY      d = ReqExpand br ar (a0, a1) d
  ReqTuple2 ar a0 a1 br  b       d = 'False
type instance Req ar (a0, a1) br b d = ReqTuple2 ar a0 a1 br b d

type instance Req ar Int br b d = Int == b


-----------------------------------------------------------------------------------

type family Deq (ar :: *) (a :: *) (br :: *) (b :: *) (d :: Nat) :: Bool

type instance Deq ar a br b 'Z = a == b

type family DeqExpand (ar :: *) (br :: *) (b :: *) (d :: Nat) where
  DeqExpand ar br XY d = 'True
  DeqExpand ar br b  d = Deq ar ar br b d
type instance Deq ar XY br b ('S d) = DeqExpand ar br b d

type family DeqV1 (ar :: *) (a0 :: *) (br :: *) (b :: *) (d :: Nat) where
  DeqV1 ar a br (V1 b) d = Deq ar a br b  d
  DeqV1 ar a br  XY    d = Deq ar a br br d
  DeqV1 ar a br     b  d = 'False

-- | Deq template for an n-ary data constructor. Req template is similar
-- type family DeqType (ar :: *) (a0 :: *) .. (an :: *) (br :: *) (b :: *) (d :: Nat) where
--   DeqType ar a0 .. an br (Type b0 .. bn) d = Deq ar a0 br b0 d :&& .. :&& Deq ar an br bn d
--   DeqType ar a0 .. an br      XY          = Deq ar (Type a0 .. an) br br d
--   DeqType ar a0 .. an br       b          = 'False
-- type instance Deq ar (Type a0 .. an) (br :: *) b d = DeqType ar a0 .. an br b d


type family DeqTuple2 (ar :: *) (a0 :: *) (a1 :: *) (br :: *) (b :: *) (d :: Nat) where
  DeqTuple2 ar a0 a1 br (b0, b1) d = Deq ar a0 br b0 d :&& Deq ar a1 br b1 d
  DeqTuple2 ar a0 a1 br  XY      d = Deq ar (a0, a1) br br d
  DeqTuple2 ar a0 a1 br  b       d = 'False
type instance Deq ar (a0, a1) br b ('S d) = DeqTuple2 ar a0 a1 br b d

type instance Deq ar Int br b ('S d) = Int == b



-- Req (RecurseL (Int, XY)) (RecurseL (Int, XY)) (RecurseL (Int, (Int, XY))) (RecurseL (Int, (Int, XY))) Z = 'True

-- Req ar (Int, XY) br (Int, (Int, XY)) Z
--   Req ar Int br Int (S Z) = True
--   Req ar XY  br (Int, XY) (S Z)
--     Deq ar XY br (Int, XY) (S Z)
--     Deq ar (Int, XY) br (Int, XY) (S Z)
--       Deq ar Int br Int (S Z) = True
--       Deq ar XY  br Int (S Z) = True

-- Req ar (Int, (Int, XY)) br (Int, (Int, (Int, XY))) Z -<      -- ReqTuple2
--   Req ar Int br Int (S Z) = True                            -- DeqAtom
--   Req ar (Int, XY) br (Int, (Int, XY)) (S Z) -<              -- ReqTuple2
--     Req ar Int br Int (S S Z) = True                        -- ReqInt
--     Req ar XY  br (Int, XY) (S S Z) >-                       -- ReqExpand
--     Deq ar XY br (Int, XY) (S S Z) >-                        -- DeqXY
--     Deq ar (Int, (Int, XY)) br (Int, XY) (S S Z) -<          -- DeqTuple2
--       Deq ar Int br Int (S S Z) = True                      -- DeqInt
--       Deq ar (Int, XY) br XY (S S Z) >-                      -- DeqTuple2 -> DeqExpand
--       Deq ar (Int, XY) br (Int, (Int, (Int, XY))) (S Z) -<   -- DeqTuple2
--         Deq ar Int br Int (S Z) = True                      -- DeqInt
--         Deq ar XY  br (Int, (Int, XY)) (S Z) >-              -- DeqExpand
--         Deq ar (Int, (Int, XY)) br (Int, (Int, XY)) (S Z) -< -- DeqTuple2
--           Deq ar Int br Int (S Z) = True                    -- DeqInt
--           Deq ar (Int, XY) br (Int, XY) (S Z) -<             -- DeqTuple2
--             Deq ar Int br Int (S Z) = True                  -- DeqInt
--             Deq ar XY  br XY  (S Z) = True                  -- DeqExpand


-- | If @depth a, depth b@ are coprime and @a `:~:` b@,
-- equality will halt before `Z` is reached.
--
-- This also means that d = 'S d' for all other instances,
-- to prevent overlap.

-- Migrate this so `d` is fixed by caller, somehow can use with `Deq` without much
-- modification (hopefully to reduce the number of required instances).

-- type instance Deq (ar :: *) (a :: *) (br :: *) (b :: *) (d :: Nat)


-- -----------------------------------------------------------------------------------

-- -- | This instance is the atomic instance type of
-- -- `Req`. It means we've reached finitary equality
-- -- for this path.
-- type family ReqUnit (b :: *) where
--   ReqUnit () = 'True
--   ReqUnit b  = 'False

-- type instance Req () b d = ReqUnit b -- or: () == b? What if b == XY? Well, then it's ar == (), which is impossible?

-- -----------------------------------------------------------------------------------

-- -- | This is the `Functor` instance type of `Req`.
-- -- It's for the simplest type of zip-equality.
-- type family ReqV1 (a :: *) (b :: *) (d :: Nat) where
--   ReqV1 a (V1 b) d = Req a b ('S d)
--   -- ReqV1 a    XY  d = Deq a XY d
--   ReqV1 a     b  d = 'False

-- type instance Req (V1 a) b d = ReqV1 a b d

-- -----------------------------------------------------------------------------------

-- type family ReqU1 (a :: *) (b :: *) (d :: Nat) where
--   ReqU1 a (U1 b) d = Req a b ('S d)
--   ReqU1 a     b  d = 'False

-- type instance Req (U1 a) b d = ReqU1 a b d

-- -----------------------------------------------------------------------------------

-- type family ReqPar1 (a :: *) (b :: *) (d :: Nat) where
--   ReqPar1 a (Par1 b) d = Req a b ('S d)
--   ReqPar1 a       b  d = 'False

-- type instance Req (Par1 a) b d = ReqPar1 a b d

-- -----------------------------------------------------------------------------------

-- type family ReqRec1 (f :: * -> *) (a :: *) (b :: *) (d :: Nat) where
--   ReqRec1 f a (f b) d = Req a b ('S d)
--   ReqRec1 f a    b  d = 'False

-- type instance Req (Rec1 f a) b d = ReqRec1 f a b d

-- -----------------------------------------------------------------------------------

-- type family ReqGSum (f :: * -> *) (g :: * -> *) (a :: *) (b :: *) (d :: Nat) where
--   ReqGSum f g a ((f :+: g) b) d = Req (f a) (f b) ('S d) :&& Req (g a) (g b) ('S d)
--   ReqGSum g f a            b  d = 'False

-- type instance Req ((f :+: g) a) b d = ReqGSum f g a b d

-- -----------------------------------------------------------------------------------

-- type family ReqGProd (f :: * -> *) (g :: * -> *) (a :: *) (b :: *) (d :: Nat) where
--   ReqGProd f g a ((f :*: g) b) d = Req (f a) (f b) ('S d) :&& Req (g a) (g b) ('S d)
--   ReqGProd f g a            b  d = 'False

-- type instance Req ((f :*: g) a) b d = ReqGProd f g a b d

-- -----------------------------------------------------------------------------------




-- newtype (f :.: g) p = Comp1 (f (g p))

-- depth1 * depth2
-- depthx
-- depthy

-- x to xy depth2 times
-- y to xy depth1 times

-- traverse with equality until one xy is reached
--   that one becomes x
--   current depth is depthx
-- traverse with equality until y's xy is reached
--   decrement depthx
--   repeat until depthx == Z
-- if still all eq, x == y

-- Complexity seems to be branching_factor^(depth x * depth y)
-- Assuming pair branches and depth of 8, we get ~256 steps.

-- I did some math and found that traversing (x*y) steps is eventually arbitrarily slower
-- than traversing lcm x y steps, since gcd x y ~~> inf and lcm x y/(x*y) == 1/gcd x y.
-- However, even with billions of terms, it's still only about 25% slower, which I believe
-- is canceled out by the overhead of computing the lcm on the type level.
--
-- Also, if 1) we suppose that the majorities of equalities will be false, and 2) that
-- this will be replaced by unsafeCoerce after the type-checker finishes, there should be
-- little issue with the additional overhead.

-- sym       :: (  a :~:   b) ->  b :~: a
-- trans     :: (  a :~:   b) -> (b :~: c) ->   a :~:   c
-- apply     :: (f   :~: g  ) -> (a :~: b) -> f a :~: g b
-- inner     :: (f a :~: g b) ->  a :~: b
-- outer     :: (f a :~: g b) ->  f :~: g

-- (Int, XY)
-- (Int, (Int, XY))
-- (Int, (Int, (Int, XY)))


