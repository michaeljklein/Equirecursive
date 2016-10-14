{-# LANGUAGE AllowAmbiguousTypes #-}
module Data.Type.Test where

-- import Data.Type.Test.Internal
-- import Data.Type.Test.TH
-- import Data.X
-- import Data.Lifted
-- import GHC.TypeLits


-- | Check whether a type is atomic
type family IsAtom (a :: k) :: Bool where
  IsAtom (c a) = 'False
  IsAtom    a  = 'True

-- testNonAtom :: OfKind (k0 -> k1) -> OfKind k0 -> APasses
-- testNonAtom (OfKind x) (OfKind y) = APasses (testNonAtom_ x y)

-- -- | Should be able to make a classy version of this, except that type families
-- -- can't be passed to other type families without evaluation.
-- testNonAtom_ :: X (a :: k0 -> k1) -> X (b :: k0) -> X (TestNonAtom a b)
-- testNonAtom_ _ _ = (undefined :: X 'Passes)

-- type family TestNonAtom (a :: k0 -> k1) (b :: k0) :: Passes where
--   TestNonAtom a b = Assert (Not (IsAtom (a b))) ('Text "TestNonAtom run on:" ':<>: ShowType2 a b)


