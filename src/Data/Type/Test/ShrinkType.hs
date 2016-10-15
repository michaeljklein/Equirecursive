module Data.Type.Test.ShrinkType where

import Control.Monad
import Data.Exists
import Data.Int
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
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.Default.Orphans


-- | Nice types for nice type families
class    (Arbitrary a, CoArbitrary a, Typeable a, ShrinkType a, Default a) => Nice a

-- | This instance makes `Nice` a class synonym.
instance (Arbitrary a, CoArbitrary a, Typeable a, ShrinkType a, Default a) => Nice a


-- | Don't only recursively shrink the value, but also the type.
-- Made possible by @`ExistsK` `Type` `Nice`@'s truly.
class ShrinkType (a :: Type) where
  -- | This allows us to apply `typeShrink` inside
  -- `Exists` `Nice`.
  typeShrink :: a -> [Exists Nice]


-- | Just `shrink` the value
instance ShrinkType Bool where typeShrink = fmap ExistsK . shrink . return

-- | Just `shrink` the value
instance ShrinkType Char where typeShrink = fmap ExistsK . shrink . return

-- | Just `shrink` the value
instance ShrinkType Double where typeShrink = fmap ExistsK . shrink . return

-- | Just `shrink` the value
instance ShrinkType Float where typeShrink = fmap ExistsK . shrink . return

-- | Just `shrink` the value
instance ShrinkType Int where typeShrink = fmap ExistsK . shrink . return

-- | Just `shrink` the value
instance ShrinkType Int8 where typeShrink = fmap ExistsK . shrink . return

-- | Just `shrink` the value
instance ShrinkType Int16 where typeShrink = fmap ExistsK . shrink . return

-- | Just `shrink` the value
instance ShrinkType Int32 where typeShrink = fmap ExistsK . shrink . return

-- | Just `shrink` the value
instance ShrinkType Int64 where typeShrink = fmap ExistsK . shrink . return

-- | Just `shrink` the value
instance ShrinkType Integer where typeShrink = fmap ExistsK . shrink . return

-- | Just `shrink` the value
instance ShrinkType Ordering where typeShrink = fmap ExistsK . shrink . return

-- | Just `shrink` the value
instance ShrinkType Word where typeShrink = fmap ExistsK . shrink . return

-- | Just `shrink` the value
instance ShrinkType Word8 where typeShrink = fmap ExistsK . shrink . return

-- | Just `shrink` the value
instance ShrinkType Word16 where typeShrink = fmap ExistsK . shrink . return

-- | Just `shrink` the value
instance ShrinkType Word32 where typeShrink = fmap ExistsK . shrink . return

-- | Just `shrink` the value
instance ShrinkType Word64 where typeShrink = fmap ExistsK . shrink . return

-- | Just `shrink` the value
instance ShrinkType () where typeShrink = fmap ExistsK . shrink . return

-- | Just `shrink` the value
instance ShrinkType Natural where typeShrink = fmap ExistsK . shrink . return

-- | Just `shrink` the value
instance ShrinkType IntSet where typeShrink = fmap ExistsK . shrink . return

-- | Just `shrink` the value
instance ShrinkType OrdC where typeShrink = fmap ExistsK . shrink . return

-- | Just `shrink` the value
instance ShrinkType OrdB where typeShrink = fmap ExistsK . shrink . return

-- | Just `shrink` the value
instance ShrinkType OrdA where typeShrink = fmap ExistsK . shrink . return

-- | Just `shrink` the value
instance ShrinkType C where typeShrink = fmap ExistsK . shrink . return

-- | Just `shrink` the value
instance ShrinkType B where typeShrink = fmap ExistsK . shrink . return

-- | Just `shrink` the value
instance ShrinkType A where typeShrink = fmap ExistsK . shrink . return


-- | This splits the instances for `X`
class (IsStarX a ~ star) => ShrinkTypeX (a :: Type) (star :: Bool) where
  typeShrinkX :: a -> [Exists Nice]

-- | This returns the shrinks of @a@ as well as the shrinks of @`X` a@.
-- In other words, it tries shrinking both the value and the type.
instance ShrinkType a => ShrinkTypeX (X a) 'True where
  typeShrinkX x = typeShrink (extract x) >>= (\(ExistsK y) -> [ExistsK y, ExistsK ((return :: b -> X b) y)])

-- | Trivial instance
instance (IsStarX (X a) ~ 'False) => ShrinkTypeX (X a) 'False where
  typeShrinkX _ = []

-- | See `ShrinkTypeX`
instance ShrinkTypeX (X a) star => ShrinkType (X a) where
  typeShrink = typeShrinkX


-- | First shrinks and returns all elements then shrinks and returns the whole list
instance Nice a => ShrinkType [a] where
  typeShrink x = typeShrunk ++ listShrunk
    where
      typeShrunk = join (typeShrink <$> x) >>= (\(ExistsK y) -> [ExistsK y, ExistsK ((return :: b -> [b]) <$> y)])
      listShrunk = (ExistsK . return) <$> shrink x

-- | Shrinks the value and type if `Just`
instance ShrinkType a => ShrinkType (Maybe a) where
  typeShrink (Just  x) = typeShrink x >>= (\(ExistsK y) -> [ExistsK y, ExistsK (Just <$> y)])
  typeShrink (Nothing) = []

-- | See the `[]` instance
instance Nice a => ShrinkType (Seq a) where
  typeShrink x = typeShrunk ++ seqShrunk
    where
      typeShrunk = join (toList (typeShrink <$> x)) >>= (\(ExistsK y) -> [ExistsK y, ExistsK ((return :: b -> [b]) <$> y)])
      seqShrunk  = (ExistsK . return) <$> shrink x

-- | Returns @a@, @b@, @(a, b)@, each shrunk
instance (Nice a, Nice b) => ShrinkType (a, b) where
  typeShrink (x, y) = xShrunk ++ yShrunk ++ xyShrunk
    where
      xShrunk  = (ExistsK . return) <$> shrink x
      yShrunk  = (ExistsK . return) <$> shrink y
      xyShrunk = (ExistsK . return) <$> shrink (x, y)

-- | Shrinks the type of whichever of `Left` or `Right` inhabits it, then the whole
instance (Nice a, Nice b) => ShrinkType (Either a b) where
  typeShrink x = oneShrunk ++ bothShrunk
    where
      oneShrunk  = either (fmap (ExistsK . return) . shrink) (fmap (ExistsK . return) . shrink) x
      bothShrunk = (ExistsK . return) <$> shrink x

-- | Returns no shrinks
instance ShrinkType (a -> b) where
  typeShrink _ = []

-- | Shrinks all removals of a single element then whe whole tuple
instance (Nice a, Nice b, Nice c) => ShrinkType (a, b, c) where
  typeShrink (x, y, z) = xyShrunk ++ yzShrunk ++ xzShrunk ++ xyzShrunk
    where
      xyShrunk  = (ExistsK . return) <$> shrink (x, y)
      yzShrunk  = (ExistsK . return) <$> shrink (y, z)
      xzShrunk  = (ExistsK . return) <$> shrink (x, z)
      xyzShrunk = (ExistsK . return) <$> shrink (x, y, z)

-- | Shrinks all removals of a single element then whe whole tuple
instance (Nice a, Nice b, Nice c, Nice d) => ShrinkType (a, b, c, d) where
  typeShrink (x, y, z, w) = yzwShrunk ++ xzwShrunk ++ xywShrunk ++ xyzShrunk ++ xyzwShrunk
    where
      yzwShrunk  = (ExistsK . return) <$> shrink (y, z, w)
      xzwShrunk  = (ExistsK . return) <$> shrink (x, z, w)
      xywShrunk  = (ExistsK . return) <$> shrink (x, y, w)
      xyzShrunk  = (ExistsK . return) <$> shrink (x, y, z)
      xyzwShrunk = (ExistsK . return) <$> shrink (x, y, z, w)

