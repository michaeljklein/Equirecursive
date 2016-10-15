{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Exists where

import Data.Kind
import Data.X
import Data.Typeable
import Test.QuickCheck
import Data.Typeable
import Data.Default

class    (Arbitrary (X a), CoArbitrary (X a), Typeable (X a), Default (X a)) => NiceX a
instance (Arbitrary (X a), CoArbitrary (X a), Typeable (X a), Default (X a)) => NiceX a

data (:.) (a0 :: k0) (a1 :: k1) = (:.) (ToStar a0) (ToStar a1)

type family ToStar (a :: k) :: * where
  ToStar (a :: *) =   a
  ToStar (a :: k) = X a

type family Constraints (a :: k0) (b :: k) = (c :: Constraint) where
  Constraints (Typeable :: k0 -> Constraint) (b :: k) = Typeable b
  Constraints (NiceX    :: k0 -> Constraint) (b :: k) = NiceX b
  Constraints (a  :: k  -> Constraint) (b :: k) = a (b :: k)
  Constraints (a0 :. a1              ) (b :: k) = (Constraints a0 (b :: k), Constraints a1 (b :: k))

data ExistsK k (c :: k0) = forall (a :: k). Constraints c a => ExistsK { getExistsK :: X a }

type Exists c = ExistsK Type c

