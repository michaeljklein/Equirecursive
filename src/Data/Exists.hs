{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Exists where

import Data.Kind
import Data.X

data (:.) (a0 :: k0) (a1 :: k1) = (:.) (ToStar a0) (ToStar a1)

type family ToStar (a :: k) :: * where
  ToStar (a :: *) =   a
  ToStar (a :: k) = X a

type family Constraints (a :: k0) (b :: k) = (c :: Constraint) where
  Constraints (a  :: k  -> Constraint) (b :: k) = a (b :: k)
  Constraints (a  ::       Constraint) (b :: k) = a
  Constraints (a0 .: a1              ) (b :: k) = (Constraints a0 (b :: k), Constraints a1 (b :: k))

data ExistsK k (c :: k -> Constraint) = forall (a :: k). Constraints c a => ExistsK { getExistsK :: X a }

type Exists c = ExistsK Type c

exists :: Constraints c a => (a :: Type) -> Exists c
exists = ExistsK . return

existsK :: Constraints c a => X (a :: k) -> ExistsK k c
existsK = ExistsK




