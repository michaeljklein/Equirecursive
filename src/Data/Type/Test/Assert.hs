module Data.Type.Test.Assert where


import Control.Monad
import Data.Exists
import Data.IntSet (IntSet)
import Data.Kind
import Data.Proxy
import Data.Sequence (Seq)
import Data.Type.Equality
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



-- | Unit type with @'`Passes` :: `Passes`@ for assertions.
data Passes = Passes deriving (Eq, Ord, Show)

-- | This is probably superflous, but I think it might not be.
data APasses = forall (a :: Passes). Typeable a => APasses { getAPasses :: X a }

-- | Trivial instance, mostly to make sure `APasses` is fully evaluated.
instance Show APasses where
  show (APasses xa) = show xa


-- | Results in the given `TypeError` if `False`
type family Assert (b :: Bool) (e :: ErrorMessage) :: Passes where
  Assert 'True  e = 'Passes
  Assert 'False e = TypeError e

-- | Show two types
type family ShowType2 (a0 :: k0) (a1 :: k1) :: ErrorMessage where
  ShowType2 a0 a1 = 'Text "\n  " ':<>: 'ShowType a0 ':<>: 'Text "\n  " ':<>: 'ShowType a1 ':<>: 'Text "\n"

-- | Results in a `TypeError` if `/=`. Reports its arguments upon error.
type family AssertEq (a :: k) (b :: k) :: Passes where
  AssertEq a b = Assert (a == b) ('Text "AssertEq failed with arguments:" ':<>: ShowType2 a b)


