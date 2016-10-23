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
import Data.X.Folding
import Unsafe.Coerce
import Data.X.Pair
import Data.X.Drop



class    (Arbitrary (X a), CoArbitrary (X a), Typeable a, Default (X a), ToTree (UnfoldX (X a))) => NiceX a
instance (Arbitrary (X a), CoArbitrary (X a), Typeable a, Default (X a), ToTree (UnfoldX (X a))) => NiceX a

-- instance Show (ExistsK (Type -> Type) NiceX) where
--   show (ExistsK x) = show (typeOf x)

-- instance Show (ExistsK (Type -> Type -> Type) NiceX) where
--   show (ExistsK x) = show (typeOf x)

-- instance Show (ExistsK (Type -> Type -> Type -> Type) NiceX) where
--   show (ExistsK x) = show (typeOf x)

-- instance Show (ExistsK (Type -> Type -> Type -> Type -> Type) NiceX) where
--   show (ExistsK x) = show (typeOf x)

-- | Needs to be moved
joinX :: X (X (a :: k)) -> X (a :: k)
joinX = unsafeCoerce

duplicateX :: X (a :: k) -> X (X (a :: k))
duplicateX = unsafeCoerce

instance Show (ExistsK k NiceX) where
  show (ExistsK x) = showUnfolded x


-- | This is a bit of a hack to allow passing a collection of @k -> `Constraint`@
-- objects to `ExistsK`
type family Constraints (a :: k0) (b :: k) = (c :: Constraint) where
  Constraints () b = ()
  Constraints (Typeable :: k0 -> Constraint) (b :: k) = Typeable b
  Constraints (DropX    :: k0 -> Constraint) (b :: k) = DropX b
  Constraints (NiceX    :: k0 -> Constraint) (b :: k) = NiceX b
  Constraints (a  :: k  -> Constraint) (b :: k) = a (b :: k)
  Constraints (a0 :. a1              ) (b :: k) = (Constraints a0 (b :: k), Constraints a1 (b :: k))

-- | An existential type, 'containing' an object of type @k@ under constraint(s) @c@.
data ExistsK k (c :: k0) = forall (a :: k). Constraints c a => ExistsK { getExistsK :: X a }

-- | Simple existential type.
type Exists c = ExistsK Type c

-- TODO: Check these and implement

weakenExistsL :: ExistsK k (cl :. cr) -> ExistsK k cl
weakenExistsL = undefined

weakenExistsR :: ExistsK k (cl :. cr) -> ExistsK k cr
weakenExistsR = undefined

strengthenExists :: (c => a) -> ExistsK k c0 -> Maybe (ExistsK k (c :. c0))
strengthenExists = undefined

-- impliesExists :: Elem c0 cs => c0 :- c1 -> ExistsK k cs -> ExistsK k (c1 :. cs)

