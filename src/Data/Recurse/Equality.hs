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
{-# LANGUAGE TypeFamilyDependencies #-}

{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Recurse.Equality where

import Data.Type.Equality
import Data.Kind (Type)
import GHC.Generics ()
import Data.Recurse
import Data.Recurse.Recursing ()
import Data.Lifted
import Data.X
import Unsafe.Coerce
import Data.Proxy
import Data.X.Folding

import GHC.TypeLits hiding (Nat)

-- TODO: Make Recurse a newtype to make casting safe!


-------------------------------------------------------------------------------------------

-- | Likely unfinished, but this is will be the interface to the result of this module
class (RecEq a b ~ eq) => RecurseEq (a :: Type) (b :: Type) (eq :: Bool) where
  req   :: RecurseL a -> RecurseL b -> Maybe (RecurseL a :~: RecurseL b)
  reqP  :: RecurseL a -> Proxy    b -> Maybe (RecurseL a :~: RecurseL b)

instance (RecEq a b ~ 'True) => RecurseEq a b 'True where
  req  _ _ = Just . unsafeCoerce $ Refl
  reqP _ _ = Just . unsafeCoerce $ Refl

instance (RecEq a b ~ 'False) => RecurseEq a b 'False where
  req  _ _ = Nothing
  reqP _ _ = Nothing

-- | Safely cast one recursive datatype to another
rcast :: (RecEq a b ~ 'True) => RecurseL a -> RecurseL b
rcast = unsafeCoerce

-------------------------------------------------------------------------------------------


-- | Type family case impossibility error
type family Impossible (a :: k0) :: k1 where
  Impossible XY = TypeError ('Text "X XY is always follwed immediately by VoidX")

-- | Convenient alias for
-- @`Req` (`UnfoldX` a) (`UnfoldX` a) (`UnfoldX` b) (`UnfoldX` b) `Z`@
type family RecEq (a :: Type) (b :: Type) :: Bool where
  RecEq a b = Req (UnfoldX a) (UnfoldX a) (UnfoldX b) (UnfoldX b) 'Z

-------------------------------------------------------------------------------------------
--
-- Traverse with equality until one `XY` is reached
--   that one becomes a
--   current depth is depthA
-- traverse with equality until b's `XY` is reached
--   decrement current depth
--   repeat until current depth == Z
-- if still all eq, a == b
--
-- Complexity seems to be branching_factor^(depthA * depthB)
-- Assuming pair branches and depth of 8, we get ~256 steps.
--
-- I did some math and found that traversing (x*y) steps is eventually arbitrarily slower
-- than traversing lcm x y steps, since gcd x y ~~> inf and lcm x y/(x*y) == 1/gcd x y.
-- However, even with billions of terms, it's still only about 25% slower, which I believe
-- is canceled out by the overhead of computing the lcm on the type level.
--
-- Also, if 1) we suppose that the majorities of equalities will be false, and 2) that
-- this will be replaced by unsafeCoerce after the type-checker finishes, there should be
-- little issue with the additional overhead.
--
-------------------------------------------------------------------------------------------


-- | First step of the equality algorithm: recurse on constructors, checking
-- equality until `XY` is reached. Then make whichever of @a@ or @b@ the new @a@
-- during the second step, `Deq`. Note that at that point, @d `==` depthA@.
type family Req (ar :: Type) (a :: Type) (br :: Type) (b :: Type) (d :: Nat) :: Bool where
  Req ar (X XY .: VoidX) br (X XY .: VoidX) d = 'True
  Req ar (X XY .: VoidX) br (X XY .: bs   ) d = Impossible XY
  Req ar (X XY .: VoidX) br (X b  .: VoidX) d = Deq ar ar br (X b .: VoidX) d
  Req ar (X XY .: VoidX) br (X b  .: bs   ) d = Deq ar ar br (X b .: bs   ) d
  Req ar (X XY .: VoidX) br (  b  .: VoidX) d = Deq ar ar br (  b .: VoidX) d
  Req ar (X XY .: VoidX) br (  b  .: bs   ) d = Deq ar ar br (  b .: bs   ) d
  Req ar (X XY .: as   ) br (X XY .: VoidX) d = Impossible XY
  Req ar (X XY .: as   ) br (X XY .: bs   ) d = Impossible XY
  Req ar (X XY .: as   ) br (X b  .: VoidX) d = Impossible XY
  Req ar (X XY .: as   ) br (X b  .: bs   ) d = Impossible XY
  Req ar (X XY .: as   ) br (  b  .: VoidX) d = Impossible XY
  Req ar (X XY .: as   ) br (  b  .: bs   ) d = Impossible XY
  Req ar (X a  .: VoidX) br (X XY .: VoidX) d = Deq br br ar (X a .: VoidX) d
  Req ar (X a  .: VoidX) br (X XY .: bs   ) d = Impossible XY
  Req ar (X a  .: VoidX) br (X b  .: VoidX) d = X a == X b
  Req ar (X a  .: VoidX) br (X b  .: bs   ) d = 'False
  Req ar (X a  .: VoidX) br (  b  .: VoidX) d = 'False
  Req ar (X a  .: VoidX) br (  b  .: bs   ) d = 'False
  Req ar (X a  .: as   ) br (X XY .: VoidX) d = Deq br br ar (X a .: as) d
  Req ar (X a  .: as   ) br (X XY .: bs   ) d = Impossible XY
  Req ar (X a  .: as   ) br (X b  .: VoidX) d = 'False
  Req ar (X a  .: as   ) br (X b  .: bs   ) d = (X a == X b) :&& Req ar as br bs d
  Req ar (X a  .: as   ) br (  b  .: VoidX) d = 'False
  Req ar (X a  .: as   ) br (  b  .: bs   ) d = 'False
  Req ar (  a  .: VoidX) br (X XY .: VoidX) d = Deq br br ar (a .: VoidX) d
  Req ar (  a  .: VoidX) br (X XY .: bs   ) d = Impossible XY
  Req ar (  a  .: VoidX) br (X b  .: VoidX) d = 'False
  Req ar (  a  .: VoidX) br (X b  .: bs   ) d = 'False
  Req ar (  a  .: VoidX) br (  b  .: VoidX) d = Req ar a br b ('S d)
  Req ar (  a  .: VoidX) br (  b  .: bs   ) d = 'False
  Req ar (  a  .: as   ) br (X XY .: VoidX) d = Deq br br ar (a .: as) d
  Req ar (  a  .: as   ) br (X XY .: bs   ) d = Impossible XY
  Req ar (  a  .: as   ) br (X b  .: VoidX) d = 'False
  Req ar (  a  .: as   ) br (X b  .: bs   ) d = 'False
  Req ar (  a  .: as   ) br (  b  .: VoidX) d = 'False
  Req ar (  a  .: as   ) br (  b  .: bs   ) d = Req ar a br b ('S d) :&& Req ar as br bs d


-- | The second step of the equality algorithm. Recurse on constructors,
-- checking equality and replacing @a ~ (`X` `XY` `.:` `VoidX`) => a -> ar@
-- without note on the way. However, whenever @b@ reaches `XY`, @d@ is
-- decremented. Once @d ~ `Z`@, the current depth is @`>` depthA * depthB@,
-- which is @`>=` `lcm` depthA depthB@. Thus all possible offsets of @a@
-- and @b@ have been checked and @a `/=` b@.
type family Deq (ar :: Type) (a :: Type) (br :: Type) (b :: Type) (d :: Nat) :: Bool where
  Deq ar (X XY .: VoidX) br (X XY .: VoidX) ('S d) = 'True
  Deq ar (X XY .: VoidX) br (X XY .: bs   ) ('S d) = Impossible XY
  Deq ar (X XY .: VoidX) br (X b  .: VoidX) ('S d) = Deq ar ar br (X b .: VoidX) ('S d)
  Deq ar (X XY .: VoidX) br (X b  .: bs   ) ('S d) = Deq ar ar br (X b .: bs   ) ('S d)
  Deq ar (X XY .: VoidX) br (  b  .: VoidX) ('S d) = Deq ar ar br (  b .: VoidX) ('S d)
  Deq ar (X XY .: VoidX) br (  b  .: bs   ) ('S d) = Deq ar ar br (  b .: bs   ) ('S d)
  Deq ar (X XY .: as   ) br (X XY .: VoidX) ('S d) = Impossible XY
  Deq ar (X XY .: as   ) br (X XY .: bs   ) ('S d) = Impossible XY
  Deq ar (X XY .: as   ) br (X b  .: VoidX) ('S d) = Impossible XY
  Deq ar (X XY .: as   ) br (X b  .: bs   ) ('S d) = Impossible XY
  Deq ar (X XY .: as   ) br (  b  .: VoidX) ('S d) = Impossible XY
  Deq ar (X XY .: as   ) br (  b  .: bs   ) ('S d) = Impossible XY
  Deq ar (X a  .: VoidX) br (X XY .: VoidX) ('S d) = Deq ar (X a .: VoidX) br br d
  Deq ar (X a  .: VoidX) br (X XY .: bs   ) ('S d) = Impossible XY
  Deq ar (X a  .: VoidX) br (X b  .: VoidX) ('S d) = X a == X b
  Deq ar (X a  .: VoidX) br (X b  .: bs   ) ('S d) = 'False
  Deq ar (X a  .: VoidX) br (  b  .: VoidX) ('S d) = 'False
  Deq ar (X a  .: VoidX) br (  b  .: bs   ) ('S d) = 'False
  Deq ar (X a  .: as   ) br (X XY .: VoidX) ('S d) = Deq ar (X a .: as   ) br br d
  Deq ar (X a  .: as   ) br (X XY .: bs   ) ('S d) = Impossible XY
  Deq ar (X a  .: as   ) br (X b  .: VoidX) ('S d) = 'False
  Deq ar (X a  .: as   ) br (X b  .: bs   ) ('S d) = (X a == X b) :&& Deq ar as br bs ('S d)
  Deq ar (X a  .: as   ) br (  b  .: VoidX) ('S d) = 'False
  Deq ar (X a  .: as   ) br (  b  .: bs   ) ('S d) = 'False
  Deq ar (  a  .: VoidX) br (X XY .: VoidX) ('S d) = Deq ar (a .: VoidX) br br d
  Deq ar (  a  .: VoidX) br (X XY .: bs   ) ('S d) = Impossible XY
  Deq ar (  a  .: VoidX) br (X b  .: VoidX) ('S d) = 'False
  Deq ar (  a  .: VoidX) br (X b  .: bs   ) ('S d) = 'False
  Deq ar (  a  .: VoidX) br (  b  .: VoidX) ('S d) = Deq ar a br b ('S d)
  Deq ar (  a  .: VoidX) br (  b  .: bs   ) ('S d) = 'False
  Deq ar (  a  .: as   ) br (X XY .: VoidX) ('S d) = Deq ar (a .: as) br br d
  Deq ar (  a  .: as   ) br (X XY .: bs   ) ('S d) = Impossible XY
  Deq ar (  a  .: as   ) br (X b  .: VoidX) ('S d) = 'False
  Deq ar (  a  .: as   ) br (X b  .: bs   ) ('S d) = 'False
  Deq ar (  a  .: as   ) br (  b  .: VoidX) ('S d) = 'False
  Deq ar (  a  .: as   ) br (  b  .: bs   ) ('S d) = Deq ar a br b ('S d) :&& Deq ar as br bs ('S d)
  Deq ar (  a          ) br (  b          ) ('Z  ) = 'False

