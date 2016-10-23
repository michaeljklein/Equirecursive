{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Test.Data.X where

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

-- | TODO: Add Bits instance

-- type family UnX (a :: *) :: k where
--   UnX (X a) =                                                   a
--   UnX    a  = TypeError ('Text "UnX called on " ':<>: 'ShowType a ':<>: 'Text ", which is not of the form 'X _'.")

-- type family ToStar (a :: k) :: * where
--   ToStar (a :: *) =   a
--   ToStar (a :: k) = X a

-- instance Show VoidX where
--   show _ = "VoidX"

-- infixr 2 .$
-- type family (.$) (a :: *) (b :: *) = (c :: *) | c -> a b where
--   (.$) (X (c :: k -> k1)) (X (a :: k)) = X (c a)


-- infixr 0 .||
-- type family (.||) (a :: *) (b :: *) :: * where
--   (.||) (X VoidX) b = b
--   (.||)  a        b = a

-- type family IsStarX (a :: *) :: Bool where
--   IsStarX (X (a :: *)) = 'True
--   IsStarX (X (a :: k)) = 'False

notBottom :: a -> Bool
notBottom x = x `seq` true

testDefault :: Default a => a -> Bool
testDefault x = notBottom $ def `asProxyTypeOf` x

class (IsStarX a ~ star) => DefaultX (a :: *) (star :: Bool) where
  defX :: a
instance Default a => DefaultX (X a) 'True where
  defX = return def
instance (IsStarX (X a) ~ 'False) => DefaultX (X a) 'False where
  defX = X (error "(xX :: X (a :: k)) with k /~ Type is uninhabited..and you can't extract the X...WTF did you do?")
instance DefaultX (X a) star => Default (X a) where
  def = defX

-- testBounded :: (Ord a, Bounded a) => a -> Bool
-- minBound <= maxBound

testBounded :: (Bounded a, Ord a) => Proxy a -> Bool
testBounded p = minBound <= maxBound `asProxyTypeOf` p

prop_bounded_X :: Proxy (X a) -> Bool
prop_bounded_X = testBounded

prop_bounded_Y :: Proxy (Y a) -> Bool
prop_bounded_Y = testBounded

functorEq :: (Functor f, Eq a, Eq (f a)) => a -> a -> f a -> Bool
functorEq x y fx = (x == y) `iff` (x <$ fx == y <$ fx)

prop_eq_X :: X (a :: * -> *) -> X (a :: * -> *) -> Bool
prop_eq_X = (==)

prop_eq_Y :: a -> a -> X a -> Bool
prop_eq_Y = functorEq



testOrd :: Ord a => Proxy a -> Test -- (String, Property)
testOrd x y = case x `compare` y of
                LT ->  (x < y) && not (x > y) &&  (x <= y) && not (x >= y)
                EQ -> not (x < y) && not (x > y) &&  (x <= y) &&  (x >= y)
                GT -> not (x < y) &&  (x > y) && not (x <= y) &&  (x >= y)

class (EqX a star, IsStarX a ~ star) => OrdX (a :: *) (star :: Bool) where
  compareX :: a -> a -> Ordering
instance Ord a => OrdX (X a) 'True where
  x `compareX` y = extract x `compare` extract y
instance (IsStarX (X a) ~ 'False) => OrdX (X a) 'False where
  compareX _ _ = EQ
instance OrdX (X a) star => Ord (X a) where
  compare = compareX

inverses :: Eq a => (a -> b) -> (b -> a) -> a -> Bool
inverses f f' x = ((f . f') x == x) && ((f' . f) y == y)

testShowRead :: (Eq a, Show a, Read a) :: a -> Bool
testShowRead = inverses show read

class Read where
  -- default definitions
  readsPrec    = readPrec_to_S readPrec
  readList     = readPrec_to_S (list readPrec) 0
  readPrec     = readS_to_Prec readsPrec
  readListPrec = readS_to_Prec (\_ -> readList)

readListDefault :: Read a => ReadS [a]
-- ^ A possible replacement definition for the 'readList' method (GHC only).
--   This is only needed for GHC, and even then only for 'Read' instances
--   where 'readListPrec' isn't defined as 'readListPrecDefault'.
readListDefault = readPrec_to_S readListPrec 0

readListPrecDefault :: Read a => ReadPrec [a]
-- ^ A possible replacement definition for the 'readListPrec' method,
--   defined using 'readPrec' (GHC only).
readListPrecDefault = list readPrec

class Show where
    showsPrec _ x s = show x ++ s
    show x          = shows x ""
    showList ls   s = showList__ shows ls s

showList__ :: (a -> ShowS) ->  [a] -> ShowS
showList__ _     []     s = "[]" ++ s
showList__ showx (x:xs) s = '[' : showx x (showl xs)
  where
    showl []     = ']' : s
    showl (y:ys) = ',' : showx y (showl ys)

class (IsStarX a ~ star) => ShowX (a :: *) (star :: Bool) where
  showX :: a -> String
instance Show a => ShowX (X a) 'True where
  showX x = "X " ++ show (extract x)
instance (IsStarX (X a) ~ 'False, Typeable a, Typeable k) => ShowX (X (a :: k)) 'False where
  showX = show . typeOf
instance ShowX (X a) star => Show (X a) where
  show = showX


class Enum where
    succ                   = toEnum . (+ 1)  . fromEnum
    pred                   = toEnum . (subtract 1) . fromEnum
    enumFrom x             = map toEnum [fromEnum x ..]
    enumFromThen x y       = map toEnum [fromEnum x, fromEnum y ..]
    enumFromTo x y         = map toEnum [fromEnum x .. fromEnum y]
    enumFromThenTo x1 x2 y = map toEnum [fromEnum x1, fromEnum x2 .. fromEnum y]

-- Default methods for bounded enumerations
boundedEnumFrom :: (Enum a, Bounded a) => a -> [a]
boundedEnumFrom n = map toEnum [fromEnum n .. fromEnum (maxBound `asTypeOf` n)]

boundedEnumFromThen :: (Enum a, Bounded a) => a -> a -> [a]
boundedEnumFromThen n1 n2
  | i_n2 >= i_n1  = map toEnum [i_n1, i_n2 .. fromEnum (maxBound `asTypeOf` n1)]
  | otherwise     = map toEnum [i_n1, i_n2 .. fromEnum (minBound `asTypeOf` n1)]
  where
    i_n1 = fromEnum n1
    i_n2 = fromEnum n2

inverses succ pred
inverses toEnum fromEnum
toEnum (x+1) = succ (toEnum x) -- or the like

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

peekElemOff addr idx = IOExts.fixIO $ \result ->
  peek (addr `plusPtr` (idx * sizeOf result))

pokeElemOff addr idx x =
  poke (addr `plusPtr` (idx * sizeOf x)) x

peekByteOff addr off = peek (addr `plusPtr` off)

pokeByteOff addr off x = poke (addr `plusPtr` off) x

peekElemOff = peekElemOff_ undefined
   where peekElemOff_ :: a -> Ptr a -> Int -> IO a
         peekElemOff_ undef ptr off = peekByteOff ptr (off * sizeOf undef)
pokeElemOff ptr off val = pokeByteOff ptr (off * sizeOf val) val

peekByteOff ptr off = peek (ptr `plusPtr` off)
pokeByteOff ptr off = poke (ptr `plusPtr` off)

peek ptr = peekElemOff ptr 0
poke ptr = pokeElemOff ptr 0

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


-- Assuming exact math, so won't work for Float, etc
(x + y) * z == x * z + y * z
signum (abs x) = fromInteger 1
abs x = abs (negate x)
abs (x + y) <= abs x + abs y
fromInteger 2 * x == x + x
x - y               = x + negate y
negate x            = 0 - x

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

mempty is id, mconcat like default
mempty `mappend` x == x
x `mappend` mempty == x
mconcat x == foldr mappend mempty x

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

arbitrary is not bottom, shrink is not bottom/infinite
notBottom arbitrary
notBottom . length . shrink
shrink . shrink . shrink .. converges

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

coarbitrary is not bottom

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

force x `seq` True
force x == x

class (IsStarX a ~ star) => NFDataX (a :: *) (star :: Bool) where
  rnfX :: a -> ()
instance NFData a => NFDataX (X a) 'True where
  rnfX = rnf . extract
instance (IsStarX (X a) ~ 'False) => NFDataX (X a) 'False where
  rnfX _ = ()
instance NFDataX (X a) star => NFData (X a) where
  rnf = rnfX

hmmmm, at least check not bottom

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

test simple (X String -> X String)?

-- | Just uses `X` as a wrapper
instance IsString a => IsString (X (a :: *)) where
  fromString = return . fromString

functor

-- | Trivial instance
instance Functor X where
  fmap :: (a -> b) -> X a -> X b
  {-# INLINE fmap #-}
  fmap f (X x) = X (unsafeCoerce (f (unsafeCoerce x)))

test props
distribute  = collect id
collect f   = distribute . fmap f
distributeM = fmap unwrapMonad . distribute . WrapMonad
collectM f  = distributeM . liftM f

-- | Trivial instance
instance Distributive X where
  collect f = return . fmap (extract . f)
  distribute = return . fmap extract


duplicate . duplicate = fmap duplicate . duplicate
duplicate = extend id
extend extract = id
extend f . extend g = extend (f . extend g)
extend f = fmap f . duplicate
extract . duplicate = id
extract . extend f = f
fmap extract . duplicate = id
fmap f = extend (f . extract)

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

applicative

-- | Trivial instance
instance Applicative X where
  pure :: a -> X a
  {-# INLINE pure #-}
  pure x = X (unsafeCoerce x)

  {-# INLINE (<*>) #-}
  f <*> x = pure (extract f $ extract x)

monad

-- | Trivial instance
instance Monad X where
  return :: a -> X a
  {-# INLINE return #-}
  return = pure

  {-# INLINE (>>=) #-}
  x >>= f = f (extract x)


liftM (f *** g) (mzip ma mb) = mzip (liftM f ma) (liftM g mb)

liftM (const ()) ma = liftM (const ()) mb
`implies`
munzip (mzip ma mb) = (ma, mb)

  mzip :: m a -> m b -> m (a,b)
  mzip = mzipWith (,)

  mzipWith :: (a -> b -> c) -> m a -> m b -> m c
  mzipWith f ma mb = liftM (uncurry f) (mzip ma mb)

  munzip :: m (a,b) -> (m a, m b)
  munzip mab = (liftM fst mab, liftM snd mab)

-- | Trivial instance
instance MonadZip X where
  mzip :: X a -> X b -> X (a, b)
  mzip x y = return (extract x, extract y)

  mzipWith :: (a -> b -> c) -> X a -> X b -> X c
  mzipWith = liftA2

  munzip :: X (a, b) -> (X a, X b)
  munzip = bimap return return . extract

test some simple recursions, props, including:
fibbonacci
factorial

-- | Should give @`return` `0`@ when mfixed
fixConst0 :: (Monad m, Num a) => a -> a
fixConst0 x = return (abs (x - 1))

-- purity
mfix (return . h) = return (fix h)
-- left shrinking (or tightening)
mfix (\x -> a >>= \y -> f x y) = a >>= \y -> mfix (\x -> f x y)
-- sliding
mfix (liftM h . f) = liftM h (mfix (f . h)), for strict h.
-- nesting
mfix (\x -> mfix (\y -> f x y)) = mfix (\x -> f x x)

-- | Should be equivalent to `fix` at compile time
instance MonadFix X where
  mfix :: (a -> X a) -> X a
  mfix f = return (fix (extract . f))


-- | Possible to use a state transformer to make this take a finite number of steps?
fold = foldMap id
foldMap f = foldr (mappend . f) mempty
foldr f z t = appEndo (foldMap (Endo #. f) t) z
foldr' f z0 xs = foldl f' id xs z0
  where f' k x z = k $! f x z
foldl f z t = appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z
foldl' f z0 xs = foldr f' id xs z0
      where f' x k z = k $! f z x
foldr1 :: (a -> a -> a) -> t a -> a
foldr1 f xs = fromMaybe (errorWithoutStackTrace "foldr1: empty structure")
                (foldr mf Nothing xs)
  where
    mf x m = Just (case m of
                     Nothing -> x
                     Just y  -> f x y)
foldl1 :: (a -> a -> a) -> t a -> a
foldl1 f xs = fromMaybe (errorWithoutStackTrace "foldl1: empty structure")
                (foldl mf Nothing xs)
  where
    mf m y = Just (case m of
                     Nothing -> y
                     Just x  -> f x y)
toList t = build (\ c n -> foldr c n t)
null = foldr (\_ _ -> False) True
length = foldl' (\c _ -> c+1) 0
elem = any . (==)
maximum = fromMaybe (errorWithoutStackTrace "maximum: empty structure") .
       getMax . foldMap (Max #. (Just :: a -> Maybe a))
minimum = fromMaybe (errorWithoutStackTrace "minimum: empty structure") .
       getMin . foldMap (Min #. (Just :: a -> Maybe a))
sum = getSum #. foldMap Sum
product = getProduct #. foldMap Product

-- | If also Functor
foldMap f = fold . fmap f
foldMap f . fmap g = foldMap (f . g)


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

traversable

-- | Trivial instance
instance Traversable X where
  traverse f x = return <$> f (extract x)
  sequenceA x = return <$> extract x


