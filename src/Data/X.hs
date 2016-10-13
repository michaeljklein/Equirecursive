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
import Foreign.Ptr (castPtr)
import Data.Bifunctor (bimap)
import Data.Function (fix)
import Data.Kind
import GHC.TypeLits( ErrorMessage(..), TypeError )

-- | `X` is a semi-magic type. As an inhabited type, it may be safely
-- coerced to and from using `pure`/`return` and `extract`.
-- As an uninhabited type, it is used in type families
-- and is effectively a unit type with phantom argument.
--
-- Note: Need to make 100% sure that none of these operations can kill `XX`. Or at least accidentally.
newtype X (a :: k) = X { getX :: Any }

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

-- | An empty value
xX :: X (a :: k -> *)
xX = X (error "(xX :: X (a :: k -> *)) is uninhabited..and you can't extract the X...WTF did you do?")


-- -- | Clean notation for coercing to `xX`
-- class XCoerce a b where
--   (>-) :: X a -> b -> b
--   (-<) :: b -> X a -> b
--   (-<) = flip (>-)
--
-- -- instance XCoerce (X X) (Rec a) where
-- --   (>-) :: X X -> Rec a -> Rec a
-- --   (>-) (X x) r = Rec (unsafeCoerce x)
--
-- instance XCoerce a (X X) where
--   (>-) x _ = X (unsafeCoerce x)


-- | Wrapper instance
instance Bounded a => Bounded (X (a :: *)) where
  minBound = return minBound
  maxBound = return maxBound

-- | Stub instance
instance Bounded (X (a :: k -> *)) where
  minBound = xX
  maxBound = xX

-- | I find this really neat. When `X` is inhabited, it
-- behaves like a trivial newtype wrapper. Uninhabited, it
-- behaves like a unit type with phantom argument.
instance Eq a => Eq (X (a :: *)) where
  x == y = extract x == extract y

-- | Stub instance
instance Eq (X (a :: k -> *)) where
  (==) _ _ = True


-- Wrapper instance
instance Ord a => Ord (X a) where
  x `compare` y = extract x `compare` extract y

-- | Stub instance
instance Ord (X (a :: k -> *)) where
  compare _ _ = EQ


-- | Wrapper instance
instance Show (a :: *) => Show (X (a :: *)) where
  show x = "X " ++ show (extract x)

-- | Stub/defaulting instance
instance Show (X (a :: k -> *)) where
  show _ = "X X"


-- | Trivial instance
instance Functor X where
  fmap :: (a -> b) -> X a -> X b
  {-# INLINE fmap #-}
  fmap f (X x) = X (unsafeCoerce (f (unsafeCoerce x)))


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


-- | Wrapper instance
instance Enum a => Enum (X (a :: *)) where
  succ :: X a -> X a
  succ = fmap succ

  pred :: X a -> X a
  pred = fmap pred

  toEnum :: Int -> X a
  toEnum = return . toEnum

  fromEnum :: X a -> Int
  fromEnum = fromEnum . extract

  enumFrom :: X a -> [X a]
  enumFrom = fmap return . enumFrom . extract

-- | Stub instance
instance Enum (X (a :: k -> *)) where
  succ = id
  pred = id
  toEnum _ = xX
  fromEnum _ = 0


-- | Wrapper instance
instance Storable a => Storable (X (a :: *)) where
  sizeOf = sizeOf . extract
  alignment = alignment . extract
  peekElemOff p n = return <$> peekElemOff (castPtr p) n
  pokeElemOff p n = pokeElemOff (castPtr p) n . extract
  peek = fmap return . peek . castPtr
  poke p = poke (castPtr p) . extract

-- | Stub instance
instance Storable (X (a :: k -> *)) where
  sizeOf _ = 0
  alignment _ = 0
  peekElemOff _ _ = return xX
  pokeElemOff _ _ _ = return ()
  peek _ = return xX
  poke _ _ = return ()


-- | Wrapper instance
instance Num a => Num (X (a :: *)) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = return . fromInteger

-- | Stub instance
instance Num (X (a :: k -> *)) where
  (+) _ _ = xX
  (-) _ _ = xX
  (*) _ _ = xX
  negate = id
  abs = id
  signum = id
  fromInteger _ = xX


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


-- | Wrapper instance
instance Monoid a => Monoid (X (a :: *)) where
  mempty = return mempty
  mappend = liftA2 mappend
  mconcat = return . mconcat . fmap extract

-- | Stub instance
instance Monoid (X (a :: k -> *)) where
  mempty = xX
  mappend _ _ = xX
  mconcat _ = xX


-- | Wrapper instance
instance IsString a => IsString (X (a :: *)) where
  fromString = return . fromString



