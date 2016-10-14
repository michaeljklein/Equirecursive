{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Data.X where

import GHC.Prim (Any)
import Unsafe.Coerce (unsafeCoerce)
import Control.Comonad (Comonad(..))
import Control.Monad.Fix (MonadFix(..))
import Control.Monad.Zip (MonadZip(..))
import Data.String (IsString(..))
import Foreign.Storable (Storable(..))
import Control.Applicative (liftA2)
import Foreign.Ptr (Ptr, castPtr)
import Data.Bifunctor (bimap)
import Data.Function (fix)
import Data.Kind
import GHC.TypeLits( ErrorMessage(..), TypeError )
import Data.Typeable
import Test.QuickCheck
import Data.Default
import Control.DeepSeq
import Data.Hashable
import Data.Distributive



-- | TODO: Use `IsStarX` for the rest of the classes, when possible.
type family IsStarX (a :: *) :: Bool where
  IsStarX (X (a :: *)) = 'True
  IsStarX (X (a :: k)) = 'False



-- | `X` is a semi-magic type. As an inhabited type, it may be safely
-- coerced to and from using `pure`/`return` and `extract`.
-- As an uninhabited type, it is used in type families
-- and is effectively a unit type with phantom argument.
--
-- Note: Need to make 100% sure that none of these operations can kill `XX`. Or at least accidentally.
newtype X (a :: k) = X { getX :: Any } deriving (Typeable)

type family UnX (a :: *) :: k where
  UnX (X a) =                                                   a
  UnX    a  = TypeError ('Text "UnX called on " ':<>: 'ShowType a ':<>: 'Text ", which is not of the form 'X _'.")

-- | Convenience alias
type XX k = X (X :: k -> *)

-- | Synonym for ease of typing
type Y = (X :: * -> *)

-- | Synonym for ease of typing
type XY = X Y

type XV = X VoidX

-- | `X`-level `[]`. Could handle being renamed.
data VoidX

-- | Stub instance
instance Show VoidX where
  show _ = "VoidX"


infixr 1 .:
-- | `X`-level `(:)`
data (.:) (a :: *) (b :: *) = (.:) a b


infixr 2 .$
-- | `X`-level `($)`
type family (.$) (a :: *) (b :: *) = (c :: *) | c -> a b where
  (.$) (X (c :: k -> k1)) (X (a :: k)) = X (c a)


type family XApp (a :: *) = (b :: *) | b -> a where
  XApp (X (c :: k -> k1) .: X (a :: k)) = X (c a)


infixr 0 .||
-- | `X`-level `(||)`
type family (.||) (a :: *) (b :: *) :: * where
  (.||) (X VoidX) b = b
  (.||)  a        b = a


class (IsStarX a ~ star) => DefaultX (a :: *) (star :: Bool) where
  defX :: a
instance Default a => DefaultX (X a) 'True where
  defX = return def
instance (IsStarX (X a) ~ 'False) => DefaultX (X a) 'False where
  defX = X (error "(xX :: X (a :: k)) with k /~ Type is uninhabited..and you can't extract the X...WTF did you do?")
instance DefaultX (X a) star => Default (X a) where
  def = defX


class (IsStarX a ~ star) => BoundedX (a :: *) (star :: Bool) where
  minBoundX :: a
  maxBoundX :: a
instance Bounded a => BoundedX (X a) 'True where
  minBoundX = return minBound
  maxBoundX = return maxBound
instance (IsStarX (X a) ~ 'False) => BoundedX (X a) 'False where
  minBoundX = def
  maxBoundX = def
instance BoundedX (X a) star => Bounded (X a) where
  minBound = minBoundX
  maxBound = maxBoundX


-- | I find this really neat. When `X` is inhabited, it
-- behaves like a trivial newtype wrapper. Uninhabited, it
-- behaves like a unit type with phantom argument.
class (IsStarX a ~ star) => EqX (a :: *) (star :: Bool) where
  eqX :: a -> a -> Bool
instance Eq a => EqX (X a) 'True where
  x `eqX` y = extract x == extract y
instance (IsStarX (X a) ~ 'False) => EqX (X a) 'False where
  eqX _ _ = True
instance EqX (X a) star => Eq (X a) where
  (==) = eqX


class (EqX a star, IsStarX a ~ star) => OrdX (a :: *) (star :: Bool) where
  compareX :: a -> a -> Ordering
instance Ord a => OrdX (X a) 'True where
  x `compareX` y = extract x `compare` extract y
instance (IsStarX (X a) ~ 'False) => OrdX (X a) 'False where
  compareX _ _ = EQ
instance OrdX (X a) star => Ord (X a) where
  compare = compareX


class (IsStarX a ~ star) => ShowX (a :: *) (star :: Bool) where
  showX :: a -> String
instance Show a => ShowX (X a) 'True where
  showX x = "X " ++ show (extract x)
instance (IsStarX (X a) ~ 'False, Typeable a, Typeable k) => ShowX (X (a :: k)) 'False where
  showX = show . typeOf
instance ShowX (X a) star => Show (X a) where
  show = showX


class (IsStarX a ~ star) => EnumX (a :: *) (star :: Bool) where
  succX :: a -> a
  predX :: a -> a
  toEnumX :: Int -> a
  fromEnumX :: a -> Int
  enumFromX :: a -> [a]
instance Enum a => EnumX (X a) 'True where
  succX = fmap succ
  predX = fmap pred
  toEnumX = return . toEnum
  fromEnumX = fromEnum . extract
  enumFromX = fmap return . enumFrom . extract
instance (IsStarX (X a) ~ 'False) => EnumX (X a) 'False where
  succX = id
  predX = id
  toEnumX _ = def
  fromEnumX _ = 0
  enumFromX = (:[])
instance EnumX (X a) star => Enum (X a) where
  succ = succX
  pred = predX
  toEnum = toEnumX
  fromEnum = fromEnumX


class (IsStarX a ~ star) => StorableX (a :: *) (star :: Bool) where
  sizeOfX :: a -> Int
  alignmentX :: a -> Int
  peekElemOffX :: Ptr a -> Int -> IO a
  pokeElemOffX :: Ptr a -> Int -> a -> IO ()
  peekByteOffX :: Ptr b -> Int -> IO a
  pokeByteOffX :: Ptr b -> Int -> a -> IO ()
  peekX :: Ptr a -> IO a
  pokeX :: Ptr a -> a -> IO ()
instance Storable a => StorableX (X a) 'True where
  sizeOfX          = sizeOf . extract
  alignmentX       = alignment . extract
  peekElemOffX p n = return <$> peekElemOff (castPtr p) n
  pokeElemOffX p n = pokeElemOff (castPtr p) n . extract
  peekByteOffX p n = return <$> peekByteOff (castPtr p) n
  pokeByteOffX p n = pokeByteOff (castPtr p) n . extract
  peekX            = fmap return . peek . castPtr
  pokeX        p   = poke (castPtr p) . extract
instance (IsStarX (X a) ~ 'False) => StorableX (X a) 'False where
  sizeOfX      _     = 0
  alignmentX   _     = 0
  peekElemOffX _ _   = return def
  pokeElemOffX _ _ _ = return ()
  peekByteOffX _ _   = return def
  pokeByteOffX _ _ _ = return ()
  peekX        _     = return def
  pokeX        _ _   = return ()
instance StorableX (X a) star => Storable (X a) where
  sizeOf      = sizeOfX
  alignment   = alignmentX
  peekElemOff = peekElemOffX
  pokeElemOff = pokeElemOffX
  peekByteOff = peekByteOffX
  pokeByteOff = pokeByteOffX
  peek        = peekX
  poke        = pokeX


class (IsStarX a ~ star) => NumX (a :: *) (star :: Bool) where
  plusX :: a -> a -> a
  minusX :: a -> a -> a
  timesX :: a -> a -> a
  negateX :: a -> a
  absX :: a -> a
  signumX :: a -> a
  fromIntegerX :: Integer -> a
instance Num a => NumX (X a) 'True where
  plusX = liftA2 (+)
  minusX = liftA2 (-)
  timesX = liftA2 (*)
  negateX = fmap negate
  absX = fmap abs
  signumX = fmap signum
  fromIntegerX = return . fromInteger
instance (IsStarX (X a) ~ 'False) => NumX (X a) 'False where
  plusX _ = id
  minusX _ = id
  timesX _ = id
  negateX = id
  absX = id
  signumX = id
  fromIntegerX _ = def
instance NumX (X a) star => Num (X a) where
  (+) = plusX
  (-) = minusX
  (*) = timesX
  negate = negateX
  abs = absX
  signum = signumX
  fromInteger = fromIntegerX


class (IsStarX a ~ star) => MonoidX (a :: *) (star :: Bool) where
  memptyX :: a
  mappendX :: a -> a -> a
  mconcatX :: [a] -> a
instance Monoid a => MonoidX (X a) 'True where
  memptyX = return mempty
  mappendX = liftA2 mappend
  mconcatX = return . mconcat . fmap extract
instance (IsStarX (X a) ~ 'False) => MonoidX (X a) 'False where
  memptyX   = def
  mappendX _ = id
  mconcatX _ = def
instance MonoidX (X a) star => Monoid (X a) where
  mempty = memptyX
  mappend = mappendX
  mconcat = mconcatX


class (IsStarX a ~ star) => ArbitraryX (a :: *) (star :: Bool) where
  arbitraryX :: Gen a
  shrinkX :: a -> [a]
instance Arbitrary a => ArbitraryX (X a) 'True where
  arbitraryX = fmap return arbitrary
  shrinkX = fmap return . shrink . extract
instance (IsStarX (X a) ~ 'False) => ArbitraryX (X a) 'False where
  arbitraryX = return def
  shrinkX _ = []
instance ArbitraryX (X a) star => Arbitrary (X a) where
  arbitrary = arbitraryX
  shrink = shrinkX


class (IsStarX a ~ star) => CoArbitraryX (a :: *) (star :: Bool) where
  coarbitraryX :: a -> Gen b -> Gen b
instance CoArbitrary a => CoArbitraryX (X a) 'True where
  coarbitraryX = coarbitrary . extract
instance (IsStarX (X a) ~ 'False) => CoArbitraryX (X a) 'False where
  coarbitraryX _ = id
instance CoArbitraryX (X a) star => CoArbitrary (X a) where
  coarbitrary = coarbitraryX

-- TODO: Needs to wait for QuickCheck 2.9.2 (which is released, I just don't want to migrate right now).
-- import Test.QuickCheck.Function
-- class (IsStarX a ~ star) => FunctionX (a :: *) (star :: Bool) where
--   functionX :: (a -> b) -> a :-> b
-- instance Function a => FunctionX (X a) 'True where
--   functionX = functionMap extract return
-- instance (IsStarX (X a) ~ 'False) => FunctionX (X a) 'False where
--   functionX = functionBoundedEnum
-- instance FunctionX (X a) star => Function (X a) where
--   function = functionX


class (IsStarX a ~ star) => NFDataX (a :: *) (star :: Bool) where
  rnfX :: a -> ()
instance NFData a => NFDataX (X a) 'True where
  rnfX = rnf . extract
instance (IsStarX (X a) ~ 'False) => NFDataX (X a) 'False where
  rnfX _ = ()
instance NFDataX (X a) star => NFData (X a) where
  rnf = rnfX


class (IsStarX a ~ star) => HashableX (a :: *) (star :: Bool) where
  hashWithSaltX :: Int -> a -> Int
  hashX :: a -> Int
instance Hashable a => HashableX (X a) 'True where
  hashWithSaltX s = hashWithSalt s . extract
  hashX = hash . extract
instance (IsStarX (X a) ~ 'False) => HashableX (X a) 'False where
  hashWithSaltX salt _ = salt * 16777619
  hashX _ = 0
instance HashableX (X a) star => Hashable (X a) where
  hashWithSalt = hashWithSaltX
  hash = hashX


instance IsString a => IsString (X (a :: *)) where
  fromString = return . fromString


-- | Trivial instance
instance Functor X where
  fmap :: (a -> b) -> X a -> X b
  {-# INLINE fmap #-}
  fmap f (X x) = X (unsafeCoerce (f (unsafeCoerce x)))

-- | Trivial instance
instance Distributive X where
  collect f = return . fmap (extract . f)
  distribute = return . fmap extract

-- | Trivial instance
instance Comonad X where
  extract :: X a -> a
  {-# INLINE extract #-}
  extract (X x) = unsafeCoerce x

  duplicate :: X a -> X (X a)
  duplicate = pure

  extend :: (X a -> b) -> X a -> X b
  extend f = pure . f

-- instance ComonadApply X where
--   (<@>) :: X (a -> b) -> X a -> X b
--   (<@>) = (<*>)
--
--   (@>) :: X a -> X b -> X b
--   (@>) = (*>)
--
--   (<@) :: X a -> X b -> X a
--   (<@) = (<*)


-- | Trivial instance
instance Applicative X where
  pure :: a -> X a
  {-# INLINE pure #-}
  pure x = X (unsafeCoerce x)

  {-# INLINE (<*>) #-}
  f <*> x = pure (extract f $ extract x)


-- | Trivial instance
instance Monad X where
  return :: a -> X a
  {-# INLINE return #-}
  return = pure

  {-# INLINE (>>=) #-}
  x >>= f = f (extract x)


-- | Trivial instance
instance MonadZip X where
  mzip :: X a -> X b -> X (a, b)
  mzip x y = return (extract x, extract y)

  mzipWith :: (a -> b -> c) -> X a -> X b -> X c
  mzipWith = liftA2

  munzip :: X (a, b) -> (X a, X b)
  munzip = bimap return return . extract


-- | Should be equivalent to `fix` at compile time
instance MonadFix X where
  mfix :: (a -> X a) -> X a
  mfix f = return (fix (extract . f))


-- | Trivial instance: @`X` (a :: *)@ contains exactly one
-- object of type @a@.
instance Foldable X where
  foldMap f = f . extract
  foldr f x = flip f x . extract
  foldl f x = f x . extract
  foldr1 _ = extract
  foldl1 _ = extract
  null _ = False
  length _ = 1
  elem x = (x ==) . extract
  maximum = extract
  minimum = extract
  sum = extract
  product = extract



