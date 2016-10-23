module Data.X.Drop where

import Control.Monad
-- import Data.Exists
import Data.IntSet (IntSet)
import Data.Kind
import Data.Proxy
import Data.Sequence (Seq)
import Data.Type.Equality
import Data.Type.Bool
import Data.Typeable
import Data.Word
import Data.X
import GHC.TypeLits
import Numeric.Natural
import Test.QuickCheck
import Test.QuickCheck.Poly
import Control.Comonad
import Data.Foldable (toList)
import Data.Default
import Test.QuickCheck.Property (succeeded)
import Data.X.Pair
import Data.Bifunctor
import Unsafe.Coerce

-- class DropX2  (a :: k   ) where
--   type Drop2  (k :: Type) :: Type
--   dropX2 :: X (a :: k   ) -> Drop2 k

-- instance DropX2 ('True :: Bool) where
--   type Drop2 Bool = Bool
--   dropX2 _ = True

-- instance DropX2 ('False :: Bool) where
--   type Drop2 Bool = Bool
--   dropX2 _ = False



-- | Drop a type-level object down to the value level.
class  DropX (a :: k) where
  -- | This is the dropped type
  type Drop  (a :: k) :: Type

  -- | Drop the type.
  dropX ::  X a -> Drop a

-- instance Typeable a => DropX (a :: Bool) where
--   type   Drop  (a :: Bool) = Bool
--   dropX x | typeOf x == typeOf (def :: X 'True ) = True
--           | typeOf x == typeOf (def :: X 'False) = False
--           | otherwise                           = error "Data.X.Drop.dropX: (a :: Bool), but a is not 'True or 'False"

-- | Drop `True`
instance DropX 'True where
  type   Drop  'True = Bool
  dropX _ =     True

-- | Drop `False`
instance DropX 'False where
  type   Drop  'False = Bool
  dropX _ =     False

-- | Drop any "real" `Nat`
instance KnownNat n => DropX (n :: Nat) where
  type Drop (n :: Nat) = Integer
  dropX = natVal

-- | Just like for `Nat`
instance KnownSymbol s => DropX (s :: Symbol) where
  type Drop (s :: Symbol) = String
  dropX = symbolVal

instance (DropX a, DropX b, XPair a b (IsStarX (X a)) (IsStarX (X b))) => DropX (a :. b) where
  type Drop (a :. b) = Drop a :. Drop b
  dropX = bimap dropX dropX . xPair . extract

