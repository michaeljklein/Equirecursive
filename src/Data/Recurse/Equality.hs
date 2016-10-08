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
import Data.Recurse
import Data.Recurse.Recursing
import Data.Lifted
import Data.X
import Unsafe.Coerce
import Data.Proxy


-- TODO: Make Recurse a newtype to make casting safe!

-----------------------------------------------------------------------------------

-- | Likely unfinished, but this is will be the interface to the result of this module
class (Req a a b b 'Z ~ eq) => RecurseEq (a :: *) (b :: *) (eq :: Bool) where
  req   :: RecurseL a -> RecurseL b -> Maybe (RecurseL a :~: RecurseL b)
  reqP  :: RecurseL a -> Proxy    b -> Maybe (RecurseL a :~: RecurseL b)

instance (Req a a b b 'Z ~ 'True) => RecurseEq a b 'True where
  req  _ _ = Just . unsafeCoerce $ Refl
  reqP _ _ = Just . unsafeCoerce $ Refl

instance (Req a a b b 'Z ~ 'False) => RecurseEq a b 'False where
  req  _ _ = Nothing
  reqP _ _ = Nothing

-- | Safely cast one recursive datatype to another
rcast :: (Req a a b b 'Z ~ 'True) => RecurseL a -> RecurseL b
rcast = unsafeCoerce

-----------------------------------------------------------------------------------

-- | Recurse, checking equality until `XY` is reached.
-- Then, pass the lower depth to the second stage.
type family Req (ar :: *) (a :: *) (br :: *) (b :: *) (d :: Nat) :: Bool

type family ReqExpand (ar :: *) (br :: *) (b :: *) (d :: Nat) where
  ReqExpand ar br XY d = 'True -- Is this really always True? Yup, if a == b, then True.
  ReqExpand ar br b  d = Deq ar ar br b d
type instance Req ar XY br b d = ReqExpand br b XY d

-----------------------------------------------------------------------------------

type family Deq (ar :: *) (a :: *) (br :: *) (b :: *) (d :: Nat) :: Bool

type instance Deq ar a br b 'Z = a == b

type family DeqExpand (ar :: *) (br :: *) (b :: *) (d :: Nat) where
  DeqExpand ar br XY d = 'True
  DeqExpand ar br b  d = Deq ar ar br b d
type instance Deq ar XY br b ('S d) = DeqExpand ar br b d


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

