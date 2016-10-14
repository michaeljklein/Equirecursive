{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeInType #-}
module Data.Type.Test.Internal where

import GHC.TypeLits
import Data.X
import Data.Type.Equality
import Data.Typeable
import Test.QuickCheck
import Data.Kind
import Data.Exists

-- | Unit type with @'`Passes` :: `Passes`@ for assertions.
data Passes = Passes deriving (Eq, Ord, Show)

inX :: X a
inX = X undefined

existsGen :: Constraints c a => X (a :: k) -> Gen (ExistsK k c)
existsGen = return . existsK


-- aType = oneof [ OfKind (inX :: X Int)
--               , OfKind (inX :: X ())
--               ]

-- aType2 :: [OfKind (Type -> Type)]
-- aType2 = oneof [ OfKind (inX :: X Maybe)
--                , OfKind (inX :: X IO)
--                ]

-- instance Arbitrary (OfKind (Type -> Type)) => Arbitrary (OfKind Type) where
--   arbitrary = oneof [ OfKind (inX :: X Int)
--                     , OfKind (inX :: X ())
--                     ]

-- instance Arbitrary (OfKind (Type -> Type)) where
--   arbitrary = oneof [ OfKind (inX :: X Maybe)
--                     , OfKind (inX :: X IO)
--                     ]

data OfKind k = forall (a :: k). OfKind { getOfKind :: X a } deriving (Typeable)

instance Typeable k => Show (OfKind k) where
  show = show . typeOf

data APasses = forall (a :: Passes). Typeable a => APasses { getAPasses :: X a }

instance Show APasses where
  show (APasses xa) = show xa


-- | Results in a `TypeError` if `False`
type family Assert (b :: Bool) (e :: ErrorMessage) :: Passes where
  Assert 'True  e = 'Passes
  Assert 'False e = TypeError e

type family ShowType2 (a0 :: k0) (a1 :: k1) :: ErrorMessage where
  ShowType2 a0 a1 = 'Text "\n  " ':<>: 'ShowType a0 ':<>: 'Text "\n  " ':<>: 'ShowType a1 ':<>: 'Text "\n"

-- | Results in a `TypeError` if `/=`. Reports its arguments upon error.
type family AssertEq (a :: k) (b :: k) :: Passes where
  AssertEq a b = Assert (a == b) ('Text "AssertEq failed with arguments:" ':<>: ShowType2 a b)

