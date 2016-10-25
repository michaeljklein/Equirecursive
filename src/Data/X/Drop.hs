module Data.X.Drop where

import Control.Comonad
import Data.Bifunctor
import Data.Kind
import Data.X
import Data.X.Pair
import GHC.TypeLits


-- | Drop a type-level object down to the value level.
class  DropX (a :: k) where
  -- | This is the dropped type
  type Drop  (a :: k) :: Type

  -- | Drop the type.
  dropX ::  X a -> Drop a

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

