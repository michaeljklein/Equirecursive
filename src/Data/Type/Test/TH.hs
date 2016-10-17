{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Type.Test.TH where

import Data.Exists
import Data.Type.Test.Assert
import Data.Type.Test.Internal
import Data.Type.Test.ShrinkType
import GHC.TypeLits
-- import Language.Haskell.TH hiding (Type)
import Data.Type.Bool
import Data.X
import Data.Default
import Data.Kind
import Test.QuickCheck
import Data.Typeable


data SomeBool = forall (a :: Bool). Typeable a => SomeBool { getSomeBool :: X a } deriving (Typeable)

instance Show SomeBool where
  show (SomeBool _) = "SomeBool"

instance Arbitrary SomeBool where
  arbitrary = elements [ SomeBool (def :: X 'True )
                       , SomeBool (def :: X 'False)
                       ]

type family TestAssert (a :: Bool) :: Passes where
  TestAssert a = Assert a ('Text "Hi, there")


-- testAssert :: ExistsK Bool NiceX -> APasses
-- testAssert (ExistsK x0) = APasses (testAssert_ x0)

-- | Super cool, uses the Default r => Default (e -> r) instance
testAssert_ :: X (a0 :: Bool) -> X (TestAssert a0)
testAssert_ = def

-- | Check whether a type is atomic
type family IsAtom (a :: k) :: Bool where
  IsAtom (c a) = 'False
  IsAtom    a  = 'True

-- | Properly passes on TestNonAtom IO Int
type family TestNonAtom (a :: k0 -> k1) (b :: k0) :: Passes where
  TestNonAtom a b = Assert (Not (IsAtom (a b))) ('Text "TestNonAtom run on:" ':<>: ShowType2 a b)

-- | Properly fails on TestNonAtom2 IO Int
type family TestNonAtom2 (a :: k0 -> k1) (b :: k0) :: Passes where
  TestNonAtom2 a b = Assert ((IsAtom (a b))) ('Text "TestNonAtom run on:" ':<>: ShowType2 a b)

testNonAtom :: ExistsK (Type -> Type) NiceX -> ExistsK Type Nice -> APasses
testNonAtom (ExistsK x0) (ExistsK x1) = APasses (testNonAtom_ x0 x1)

testNonAtom_ :: X (a0 :: k0 -> k1) -> X (a1 :: k0) -> X (TestNonAtom a0 a1)
testNonAtom_ = def

-- type family TestRefl (a :: k) :: Passes where
--   TestRefl a = a === a

-- testRefl0 :: Exists Nice -> APasses
-- testRefl0 (ExistsK x0) = APasses (testRefl_ x0)

-- testRefl_ :: X (a0 :: k) -> X (TestRefl a0)
-- testRefl_ = def

-- sketch of how this could be used with quickcheck:
-- testNonAtom :: OfKind (k0 -> k1) -> OfKind k0 -> Passes
-- testNonAtom (OfKind x) (OfKind y) = testNonAtom_ x y
-- testNonAtom_ :: X (a :: k0 -> k1) -> X (b :: k0) -> TestNonAtom a b
--
-- Here's a use for template haskell:
-- $(kindFamilyTest ''TestNonAtom)

-- data OfKind k = forall (a :: k). OfKind { getOfKind :: X a }
-- data APasses = forall (a :: Passes). APasses { getAPasses :: X a }

-- testNonAtom :: OfKind (k0 -> k1) -> OfKind k0 -> APasses
-- testNonAtom (OfKind x) (OfKind y) = APasses (testNonAtom_ x y)

-- -- | Should be able to make a classy version of this, except that type families
-- -- can't be passed to other type families without evaluation.
-- testNonAtom_ :: X (a :: k0 -> k1) -> X (b :: k0) -> X (TestNonAtom a b)
-- testNonAtom_ _ _ = (undefined :: X 'Passes)

-- -- | Results in a `TypeError` if `False`
-- type family Assert (b :: Bool) (e :: ErrorMessage) :: Passes where
--   Assert 'True  e = 'Passes
--   Assert 'False e = TypeError e

-- type family ShowType2 (a0 :: k0) (a1 :: k1) :: ErrorMessage where
--   ShowType2 a0 a1 = 'Text "\n  " ':<>: 'ShowType a0 ':<>: 'Text "\n  " ':<>: 'ShowType a1 ':<>: 'Text "\n"

-- -- | Results in a `TypeError` if `/=`. Reports its arguments upon error.
-- type family AssertEq (a :: k) (b :: k) :: Passes where
--   AssertEq a b = Assert (a == b) ('Text "AssertEq failed with arguments:" ':<>: ShowType2 a b)


