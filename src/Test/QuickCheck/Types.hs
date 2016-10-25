{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeInType #-}

module Test.QuickCheck.Types where

import Data.Type.Equality
import GHC.TypeLits
import Data.Kind
import Data.X
import Data.X.Pair

import Test.QuickCheck.Types.TH
import Data.TyResult


-- | Type-level `mod`, though not very efficient
type family Mod (n :: Nat) (m :: Nat) :: Nat where
  Mod n m = Mod' (CmpNat n m == 'LT) n m

-- | Helper for `Mod`
type family Mod' (b :: Bool) (n :: Nat) (m :: Nat) :: Nat where
  Mod' 'True  n m = n
  Mod' 'False n m = Mod (n - m) m

-- | Type-level `div`, though not very efficient
type family Div (n :: Nat) (d :: Nat) :: Nat where
  Div n 0 = TypeError ('Text "Division by 0::Nat.")
  Div n d = Div' 0 n d

-- | First helper for `Div`
type family Div' (q :: Nat ) (r :: Nat) (d :: Nat) :: Nat where
  Div' q r d = Div'' (CmpNat r d == 'GT) q r d

-- | Second helper for `Div`
type family Div'' (b :: Bool) (q :: Nat) (r :: Nat) (d :: Nat) :: Nat where
  Div'' 'True  q r d = Div' (q + 1) (r - d) d
  Div'' 'False q r d = q

-- | Add a note to a `TyResult`, e.g.
--
-- @
--  (x === y) \``Note`\` (`X` x :. `X` y :. "should have the same kind")
-- @
type family Note (r :: TyResult) (n :: Type) :: TyResult where
  Note (f e) n = f (Noting ('Text ": " ':<>: e) n)
  Note (  e) n = TypeError ('Text "Test.QuickCheck.Types.Note called on: " ':<>: 'ShowType e ':$$: 'ShowType n)

-- | Helper for `Note`
type family Noting (e :: ErrorMessage) (n :: k) :: ErrorMessage where
  Noting e (as :. (  a :: Symbol)) = Noting ('Text     a ':<>: 'Text " " ':<>: e) as
  Noting e (as :. (X a :: Type  )) = Noting ('ShowType a ':<>: 'Text " " ':<>: e) as
  Noting e (      (  a :: Symbol)) =        ('Text     a ':<>: 'Text " " ':<>: e)
  Noting e (      (X a :: Type  )) =        ('ShowType a ':<>: 'Text " " ':<>: e)


-- | Not sure what should be lifted besides Bool.
type family LiftTyResult (f :: k0 -> k) (a :: k0) :: TyResult
type instance LiftTyResult (f :: k -> Bool) (a :: k) = (f a === 'True) `Note` ("LiftTyResult" :. X f :. X a)

type family LiftTyResult2 (f :: k0 -> k1 -> k) (a :: k0) (b :: k1) :: TyResult

-- | Another way to drop `TyResult`
type family ToConstraint (r :: TyResult) :: Constraint where
  ToConstraint (TySuccess e) = ()
  ToConstraint (TyFails   e) = TypeError e


type ShowInfix i a b = 'Text "(" ':<>: 'ShowType a ':<>: 'Text " " ':<>: 'Text i ':<>: 'Text " " ':<>: 'ShowType b ':<>: 'Text ")"
type ShowPrefix p a = 'Text "(" ':<>: 'Text p ':<>: 'Text " " ':<>: ShowType a ':<>: 'Text ")"


type ShowEq a b = ShowInfix "===" a b

-- | Akin to the function with the same name from QuickCheck
type family (===) (a :: k) (b :: k) = (r :: TyResult) | r -> a b where
  a === a = 'TySuccess (ShowEq a a)
  a === b = 'TyFails   (ShowEq a b)

type ShowImplies a b = ShowInfix "==>" a b

-- | Akin to the function with the same name from QuickCheck
type family (==>) (a :: TyResult) (b :: TyResult) = (r :: TyResult) | r -> a b where
  ('TySuccess e0) ==> ('TyFails e1) = 'TyFails   (ShowImplies ('TySuccess e0) ('TyFails e1))
  x               ==> y             = 'TySuccess (ShowImplies x y)

type ShowAnd a b = ShowInfix ".&&." a b
-- | Akin to the function with the same name from QuickCheck
type family (.&&.) (a :: TyResult) (b :: TyResult) = (r :: TyResult) | r -> a b where
  ('TySuccess e0) .&&. ('TySuccess e1) = 'TySuccess (ShowAnd ('TySuccess e0) ('TySuccess e1))
  x               .&&. y               = 'TyFails   (ShowAnd x y)

type ShowOr a b = ShowInfix ".||." a b
-- | Akin to the function with the same name from QuickCheck
type family (.||.) (a :: TyResult) (b :: TyResult) = (r :: TyResult) | r -> a b where
  ('TyFails e0) .||. ('TyFails e1) = 'TyFails (ShowOr ('TyFails e0) ('TyFails e1))
  x             .||. y             = 'TySuccess (ShowOr x y)

type ShowAssertFails a = ShowPrefix "AssertFails" a
-- | Akin to the function with the same name from QuickCheck
type family AssertFails (a :: TyResult) = (r :: TyResult) | r -> a where
  AssertFails ('TySuccess e) = 'TyFails   (ShowAssertFails e)
  AssertFails ('TyFails   e) = 'TySuccess (ShowAssertFails e)



-- | A simple test family, should always return `TySuccess`
type family Reflexive (a :: k) :: TyResult where
  Reflexive a = a === a

$((dropFamilyD ''Reflexive))


-- | Another simple test family, succeeds if input is `True`
type family IsTrue (a :: Bool) :: TyResult where
  IsTrue a = a === 'True

$(dropFamilyD ''IsTrue)


-- | Another simple test family, should always fail
type family Fails (a :: k) :: TyResult where
  Fails a = Int === Bool

$(dropFamilyD ''Fails)


