{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeInType #-}

module Test.QuickCheck.Types where

import Data.Type.Equality
import GHC.TypeLits
import Data.Kind
import Data.X
import Data.X.Pair
import Data.Type.Bool

import Test.QuickCheck.Types.TH
import qualified Language.Haskell.TH as TH
import Control.Monad
import Data.Default

u = error "My undefined"


type family Mod (n :: Nat) (m :: Nat) :: Nat where
  Mod n m = Mod' (CmpNat n m == 'LT) n m

type family Mod' (b :: Bool) (n :: Nat) (m :: Nat) :: Nat where
  Mod' 'True  n m = n
  Mod' 'False n m = Mod (n - m) m

type family Div (n :: Nat) (d :: Nat) :: Nat where
  Div n 0 = TypeError ('Text "Division by 0::Nat.")
  Div n d = Div' 0 n d

type family Div' (q :: Nat ) (r :: Nat) (d :: Nat) :: Nat where
  Div' q r d = Div'' (CmpNat r d == 'GT) q r d

type family Div'' (b :: Bool) (q :: Nat) (r :: Nat) (d :: Nat) :: Nat where
  Div'' 'True  q r d = Div' (q + 1) (r - d) d
  Div'' 'False q r d = q


data TyResult = TySuccess ErrorMessage | TyFails ErrorMessage

class Success (r :: TyResult) where
  success :: X r -> Bool

instance Success ('TySuccess e) where
  success _ = True

instance Success ('TyFails e) where
  success _ = False

class DropTyError (r :: TyResult) where
  dropTyError :: X r -> X r

instance DropTyError (TySuccess e) where
  dropTyError = id

instance TypeError e => DropTyError (TyFails e) where
  dropTyError = id


-- | Add a note to a `TyResult`, e.g.
-- @(x === y) `Note` (X x :. X y :. "should have the same kind")@
type family Note (r :: TyResult) (n :: Type) :: TyResult where
  Note (f e) n = f (Noting ('Text ": " ':<>: e) n)
  Note (  e) n = TypeError ('Text "Test.QuickCheck.Types.Note called on: " ':<>: 'ShowType e ':$$: 'ShowType n)

type family Noting (e :: ErrorMessage) (n :: k) :: ErrorMessage where
  Noting e (as :. (  a :: Symbol)) = Noting ('Text     a ':<>: 'Text " " ':<>: e) as
  Noting e (as :. (X a :: Type  )) = Noting ('ShowType a ':<>: 'Text " " ':<>: e) as
  Noting e (      (  a :: Symbol)) =        ('Text     a ':<>: 'Text " " ':<>: e)
  Noting e (      (X a :: Type  )) =        ('ShowType a ':<>: 'Text " " ':<>: e)


-- | Not sure what should be lifted besides Bool.
type family LiftTyResult (f :: k0 -> k) (a :: k0) :: TyResult
type instance LiftTyResult (f :: k -> Bool) (a :: k) = (f a === 'True) `Note` ("LiftTyResult" :. X f :. X a)

type family LiftTyResult2 (f :: k0 -> k1 -> k) (a :: k0) (b :: k1) :: TyResult

type family ToConstraint (r :: TyResult) :: Constraint where
  ToConstraint (TySuccess e) = ()
  ToConstraint (TyFails   e) = TypeError e


type ShowInfix i a b = 'Text "(" ':<>: 'ShowType a ':<>: 'Text " " ':<>: 'Text i ':<>: 'Text " " ':<>: 'ShowType b ':<>: 'Text ")"
type ShowPrefix p a = 'Text "(" ':<>: 'Text p ':<>: 'Text " " ':<>: ShowType a ':<>: 'Text ")"


type ShowEq a b = ShowInfix "===" a b
type family (===) (a :: k) (b :: k) = (r :: TyResult) | r -> a b where
  a === a = 'TySuccess (ShowEq a a)
  a === b = 'TyFails   (ShowEq a b)

type ShowImplies a b = ShowInfix "==>" a b
type family (==>) (a :: TyResult) (b :: TyResult) = (r :: TyResult) | r -> a b where
  ('TySuccess e0) ==> ('TyFails e1) = 'TyFails   (ShowImplies ('TySuccess e0) ('TyFails e1))
  x               ==> y             = 'TySuccess (ShowImplies x y)

type ShowAnd a b = ShowInfix ".&&." a b
type family (.&&.) (a :: TyResult) (b :: TyResult) = (r :: TyResult) | r -> a b where
  ('TySuccess e0) .&&. ('TySuccess e1) = 'TySuccess (ShowAnd ('TySuccess e0) ('TySuccess e1))
  x               .&&. y               = 'TyFails   (ShowAnd x y)

type ShowOr a b = ShowInfix ".||." a b
type family (.||.) (a :: TyResult) (b :: TyResult) = (r :: TyResult) | r -> a b where
  ('TyFails e0) .||. ('TyFails e1) = 'TyFails (ShowOr ('TyFails e0) ('TyFails e1))
  x             .||. y             = 'TySuccess (ShowOr x y)

type ShowAssertFails a = ShowPrefix "AssertFails" a
type family AssertFails (a :: TyResult) = (r :: TyResult) | r -> a where
  AssertFails ('TySuccess e) = 'TyFails   (ShowAssertFails e)
  AssertFails ('TyFails   e) = 'TySuccess (ShowAssertFails e)


type family Reflexive (a :: k) :: TyResult where
  Reflexive a = a === a

defx :: Default (X a) => X a
defx = def

type TExpQ a = TH.Q (TH.TExp a)

reflexive :: TypeKQ k -> TH.ExpQ
reflexive x = x >>= \(TypeK t _) -> [| let result = $([| reflexive_ |])(def :: $(TH.conT ''X `TH.appT` return t)) in if success result then True else False |]

reflexive_ :: (X (a :: k)) -> X (Reflexive a)
reflexive_ _ = def

type family IsTrue (a :: Bool) :: TyResult where
  IsTrue a = a === 'True

isTrue :: TypeKQ Bool -> TH.ExpQ
isTrue x = x >>= \(TypeK t _) -> [| let result = isTrue_ (def :: $(TH.conT ''X `TH.appT` return t)) in if success result then True else False |]

isTrue_ :: X (a :: Bool) -> X (IsTrue a)
isTrue_ _ = def

$(fmap return ( (mtt <$> (show <$> TH.reify ''F))))


-- $((dropFamily ''Reflexive))

-- $(dropped_Reflexive undefined undefined)
-- reflexive :: Reflexive (a :: k)
-- reflexive = "reflexive1"

-- $(

type family Fails (a :: k) :: TyResult where
  Fails a = Int === Bool



-- fails :: Fails (a :: k)
-- fails = "fails"

-- type IsTrue a = a ~ 'True


