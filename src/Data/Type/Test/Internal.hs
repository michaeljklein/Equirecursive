{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeInType #-}
module Data.Type.Test.Internal where

-- import Control.Applicative (ZipList)
-- import Data.Functor.Const
-- import Data.Functor.Identity
-- import Data.Map.Lazy (Map)
-- import Data.Monoid (All, Any) -- needs QuickCheck 2.9.2
-- import Data.Monoid (Dual, Endo, Sum, Product, First, Last)
-- import Data.Version (Version) -- needs QuickCheck 2.9.2
-- import GHC.Generics hiding (C)
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
                    -- , existsGen (Proxy :: Proxy (X Version))
                    -- , existsGen (Proxy :: Proxy (X All))
                    -- , existsGen (Proxy :: Proxy (X Any))


-- | Unit type with @'`Passes` :: `Passes`@ for assertions.
data Passes = Passes deriving (Eq, Ord, Show)

inX :: X a
inX = X undefined

existsGen :: forall k0 k (c :: k0) (a :: k). (ArbitraryX (X a) (IsStarX (X a)), Constraints c a) => Proxy (X a) -> Gen (ExistsK k c)
existsGen p = fmap ExistsK $ flip asProxyTypeOf p <$> arbitrary

xApp :: Proxy (X t) -> Proxy (X a) -> Proxy (X (t a))
xApp _ _ = Proxy


----------------------------------------------------------------------------------------------------------------------------------
-- The following four functions represent the steps needed to
-- transform any existential Nice type generator to an
-- existential Nice list of that type generator.
-- This should work for all unary types that only require Nice.

-- arb1 :: Nice a => X a -> Gen (X [a])
-- arb1 _ = arbitrary

-- arb2 :: Nice a => X a -> Gen (ExistsK Type Nice)
-- arb2 = fmap existsK . arb1

-- arb3 :: ExistsK Type Nice -> Gen (ExistsK Type Nice)
-- arb3 = (\(ExistsK x :: ExistsK Type Nice) -> arb2 x)

-- arb4 :: Gen (ExistsK Type Nice) -> Gen (ExistsK Type Nice)
-- arb4 x = x >>= arb3

-- arb5 :: Gen (ExistsK Type Nice) -> Gen (ExistsK Type Nice)
-- arb5 x = x >>= (\(ExistsK x :: ExistsK Type Nice) -> (fmap existsK . (const arbitrary :: Nice a => X a -> Gen (X [a]))) x)

-- arb6 :: Gen (ExistsK Type Nice) -> Gen (ExistsK Type Nice)
-- arb6 x = x >>= (\(ExistsK x :: ExistsK Type Nice) -> (fmap existsK . (const arbitrary :: Nice a => X a -> Gen (X (Maybe a)))) x)

-- -- arb7 :: Gen (ExistsK Type Nice) -> Gen (ExistsK Type Nice)
-- -- arb7 x = x >>= (\(ExistsK x :: ExistsK Type Nice) -> (fmap existsK . (const arbitrary :: Nice a => X a -> Gen (X (NonEmptyList a)))) x)

-- arb8 :: Gen (ExistsK Type Nice) -> Gen (ExistsK Type Nice)
-- arb8 x = x >>= (\(ExistsK x :: ExistsK Type Nice) -> (fmap existsK . (const arbitrary :: Nice a => X a -> Gen (X (X a)))) x)

-- arb9 :: Gen (ExistsK Type Nice) -> Gen (ExistsK Type Nice) -> Gen (ExistsK Type Nice)
-- arb9 = undefined (,)

----------------------------------------------------------------------------------------------------------------------------------

arbitraryExistsK1 :: [Gen (ExistsK Type Nice) -> Gen (ExistsK Type Nice)]
arbitraryExistsK1 = map (=<<) [ (\(ExistsK x0 :: ExistsK Type Nice) -> existsX1 (Proxy :: Proxy X    ) x0)
                            , (\(ExistsK x0 :: ExistsK Type Nice) -> existsX1 (Proxy :: Proxy []   ) x0)
                            , (\(ExistsK x0 :: ExistsK Type Nice) -> existsX1 (Proxy :: Proxy Maybe) x0)
                            , (\(ExistsK x0 :: ExistsK Type Nice) -> existsX1 (Proxy :: Proxy Seq  ) x0)
                            ]

arbitraryExistsK2 :: [Gen (ExistsK Type Nice) -> Gen (ExistsK Type Nice) -> Gen (ExistsK Type Nice)]
arbitraryExistsK2 = map bind2 [ (\(ExistsK x0 :: ExistsK Type Nice) (ExistsK x1 :: ExistsK Type Nice) -> existsX2 (Proxy :: Proxy (,)) x0 x1)
                              , (\(ExistsK x0 :: ExistsK Type Nice) (ExistsK x1 :: ExistsK Type Nice) -> existsX2 (Proxy :: Proxy (->)) x0 x1)
                              , (\(ExistsK x0 :: ExistsK Type Nice) (ExistsK x1 :: ExistsK Type Nice) -> existsX2 (Proxy :: Proxy Either) x0 x1)
                              ]

arbitraryExistsK3 :: [Gen (ExistsK Type Nice) -> Gen (ExistsK Type Nice) -> Gen (ExistsK Type Nice) -> Gen (ExistsK Type Nice)]
arbitraryExistsK3 = map bind3 []


arbitraryX1 :: Nice (t a) => Proxy t -> X a -> Gen (X (t a))
arbitraryX1 _ _ = arbitrary

arbitraryX2 :: Nice (t a b) => Proxy t -> X a -> X b -> Gen (X (t a b))
arbitraryX2 _ _ _ = arbitrary

arbitraryX3 :: Nice (t a b c) => Proxy t -> X a -> X b -> X c -> Gen (X (t a b c))
arbitraryX3 _ _ _ _ = arbitrary

arbitraryX4 :: Nice (t a b c d) => Proxy t -> X a -> X b -> X c -> X d -> Gen (X (t a b c d))
arbitraryX4 _ _ _ _ _ = arbitrary

arbitraryX5 :: Nice (t a b c d e) => Proxy t -> X a -> X b -> X c -> X d -> X e -> Gen (X (t a b c d e))
arbitraryX5 _ _ _ _ _ _ = arbitrary

existsX1 :: Nice (t a) => Proxy t -> X a -> Gen (ExistsK Type Nice)
existsX1 t x0 = existsK <$> arbitraryX1 t x0

existsX2 :: Nice (t a b) => Proxy t -> X a -> X b -> Gen (ExistsK Type Nice)
existsX2 t x0 x1 = existsK <$> arbitraryX2 t x0 x1

existsX3 :: Nice (t a b c) => Proxy t -> X a -> X b -> X c -> Gen (ExistsK Type Nice)
existsX3 t x0 x1 x2 = existsK <$> arbitraryX3 t x0 x1 x2

existsX4 :: Nice (t a b c d) => Proxy t -> X a -> X b -> X c -> X d -> Gen (ExistsK Type Nice)
existsX4 t x0 x1 x2 x3 = existsK <$> arbitraryX4 t x0 x1 x2 x3

existsX5 :: Nice (t a b c d e) => Proxy t -> X a -> X b -> X c -> X d -> X e -> Gen (ExistsK Type Nice)
existsX5 t x0 x1 x2 x3 x4 = existsK <$> arbitraryX5 t x0 x1 x2 x3 x4

bind2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bind2 = ((join .) .) . liftM2

bind3 :: Monad m => (a -> b -> c -> m d) -> m a -> m b -> m c -> m d
bind3 = (((join .) .) .) . liftM3

bind4 :: Monad m => (a -> b -> c -> d -> m e) -> m a -> m b -> m c -> m d -> m e
bind4 = ((((join .) .) .) .) . liftM4

bind5 :: Monad m => (a -> b -> c -> d -> e -> m f) -> m a -> m b -> m c -> m d -> m e -> m f
bind5 = (((((join .) .) .) .) .) . liftM5

-- Has to derive n to be useful. Too out sidetracking for now.
-- class MultiCompose (n :: Nat) (as :: Type) | as -> n where
--   type ComposeResult (n :: Nat) (as :: Type) (a :: Type) :: Type
--   (./) :: X n -> (b -> c) -> ComposeResult n as b -> ComposeResult n as c
--   (\.) :: (b -> c) -> X n -> ComposeResult n as b -> ComposeResult n as c
-- instance MultiCompose (1 :: Nat) (a0, a1) where
--   type ComposeResult 1 (a0, a1) a = a0 -> a1 -> a
--   (./) _   = (.) . (.)
--   (\.) f _ = ((.) . (.)) f



-- needs its own instance, not worth it
-- (>>= (\(ExistsK x :: ExistsK Type Nice) -> (fmap existsK . (const arbitrary :: Nice a => X a -> Gen (X (NonEmpty a)))) x))

-- needs newer QuickCheck
-- (>>= (\(ExistsK x :: ExistsK Type Nice) -> (fmap existsK . (const arbitrary :: Nice a => X a -> Gen (X (Identity a)))) x))
-- (>>= (\(ExistsK x :: ExistsK Type Nice) -> (fmap existsK . (const arbitrary :: Nice a => X a -> Gen (X (ZipList a)))) x))
-- (>>= (\(ExistsK x :: ExistsK Type Nice) -> (fmap existsK . (const arbitrary :: Nice a => X a -> Gen (X (Dual a)))) x))
-- (>>= (\(ExistsK x :: ExistsK Type Nice) -> (fmap existsK . (const arbitrary :: Nice a => X a -> Gen (X (Sum a)))) x))
-- (>>= (\(ExistsK x :: ExistsK Type Nice) -> (fmap existsK . (const arbitrary :: Nice a => X a -> Gen (X (Product a)))) x))
-- (>>= (\(ExistsK x :: ExistsK Type Nice) -> (fmap existsK . (const arbitrary :: Nice a => X a -> Gen (X (First a)))) x))
-- (>>= (\(ExistsK x :: ExistsK Type Nice) -> (fmap existsK . (const arbitrary :: Nice a => X a -> Gen (X (Last a)))) x))
-- (>>= (\(ExistsK x :: ExistsK Type Nice) -> (fmap existsK . (const arbitrary :: Nice a => X a -> Gen (X (Endo a)))) x))
-- (\(ExistsK x0 :: ExistsK Type Nice) (ExistsK x1 :: ExistsK Type Nice) -> existsX2 (Proxy :: Proxy Map) x0 x1)
-- (\(ExistsK x0 :: ExistsK Type Nice) (ExistsK x1 :: ExistsK Type Nice) -> existsX2 (Proxy :: Proxy Const) x0 x1)

-- they're strict, so probably shouldn't include
-- (>>= (\(ExistsK x :: ExistsK Type Nice) -> (fmap existsK . (const arbitrary :: Nice a => X a -> Gen (X (IntMap a)))) x))
-- (>>= (\(ExistsK x :: ExistsK Type Nice) -> (fmap existsK . (const arbitrary :: Nice a => X a -> Gen (X (Set a)))) x))



-- existsGen1 :: (Nice a, Nice (t a)) => Proxy (X (t :: Type -> Type)) -> Gen (ExistsK Type Nice) -> Gen (ExistsK Type Nice)
-- existsGen1 = undefined

-- listGen :: Gen (ExistsK Type Nice) -> Gen (ExistsK Type Nice)
-- listGen g = undefined

-- What about the far simpler template haskell solution of [Constraint] -> arbitrary types that fall under?
-- Nah, that's currently overkill for something like this. A "real" solution would require instance chains.

-- Just make Arbitrary (ExistsK k (class)) instances with template haskell.
-- Then can make a class of the form: (Arbitrary (ExistsK k c0), Arbitrary (ExistsK k c1)) => Arbitrary (ExistsK k (c0 :. c1))
-- TH supports finding all instances of a class, so can find all instances of Arbitrary, then find all unary instances of those classes
-- to make Arbitrary (ExistsK k class) instances.
--
-- e.g.
-- instance Arbitrary Int
-- instance Eq Int
-- instance Arbitrary (ExistsK Type Eq) where arbitrary = oneof [ existsGen (Proxy :: Proxy (X Int)), .. ]
--
-- This should make ExistsK a powerful tool for testing.
--
-- Consider: testReflexive (\x y -> (x == y) == (y == x)) :: Eq a => a -> a -> Bool
-- This can be lifted to: ExistsK Type (Eq :. Typeable) -> ExistsK Type (Eq :. Typeable) -> Bool
--
-- TODO: HOWEVER: should first just make ExistsK Type (Arbitrary :. Typeable), ExistsK (Type -> Type) (Arbitrary :. Typeable), etc. instances.
-- All the above is a bit out of scope and, while I think it's a good idea, it's worthless if the original case I was thinking of fails.
--
-- Will also need the above for CoArbitrary. I'm not sure how things like (Integral a => Arbitrary (Ratio a)) should work, so lets leave them off
-- for now.

-- Want the constraint Nice a => Nice (f a). Hmm.. One way is to instantiate the type when generated through Exists k Nice


-- It looks like CoArbitrary has exactly the "easy" types from "Arbitrary", including the main ones I want: Maybe, Either, (,), etc
class    (Arbitrary a, CoArbitrary a, Typeable a) => Nice a
instance (Arbitrary a, CoArbitrary a, Typeable a) => Nice a


oneKindType :: [Gen (ExistsK Type Nice)]
oneKindType = [ existsGen (Proxy :: Proxy (X Bool))
              , existsGen (Proxy :: Proxy (X Char))
              , existsGen (Proxy :: Proxy (X Double))
              , existsGen (Proxy :: Proxy (X Float))
              , existsGen (Proxy :: Proxy (X Int))
              , existsGen (Proxy :: Proxy (X Int8))
              , existsGen (Proxy :: Proxy (X Int16))
              , existsGen (Proxy :: Proxy (X Int32))
              , existsGen (Proxy :: Proxy (X Int64))
              , existsGen (Proxy :: Proxy (X Integer))
              , existsGen (Proxy :: Proxy (X Ordering))
              , existsGen (Proxy :: Proxy (X Word))
              , existsGen (Proxy :: Proxy (X Word8))
              , existsGen (Proxy :: Proxy (X Word16))
              , existsGen (Proxy :: Proxy (X Word32))
              , existsGen (Proxy :: Proxy (X Word64))
              , existsGen (Proxy :: Proxy (X ()))
              , existsGen (Proxy :: Proxy (X Natural))
              , existsGen (Proxy :: Proxy (X IntSet))
              , existsGen (Proxy :: Proxy (X OrdC))
              , existsGen (Proxy :: Proxy (X OrdB))
              , existsGen (Proxy :: Proxy (X OrdA))
              , existsGen (Proxy :: Proxy (X C))
              , existsGen (Proxy :: Proxy (X B))
              , existsGen (Proxy :: Proxy (X A))
              ]


-- (CoArbitrary a, CoArbitrary b, CoArbitrary c) => CoArbitrary (a, b, c)
-- (CoArbitrary a, CoArbitrary b, CoArbitrary c, CoArbitrary d) => CoArbitrary (a, b, c, d)
-- (CoArbitrary a, CoArbitrary b, CoArbitrary c, CoArbitrary d, CoArbitrary e) => CoArbitrary (a, b, c, d, e)


-- HasResolution a => Arbitrary (Fixed a)
-- Integral a => Arbitrary (Ratio a)
-- Integral a => Arbitrary (Small a)
-- (Integral a, Bounded a) => Arbitrary (Large a)

-- (Arbitrary a, ShrinkState s a) => Arbitrary (Shrinking s a)
-- (Num a, Eq a, Arbitrary a) => Arbitrary (NonZero a)
-- (Num a, Ord a, Arbitrary a) => Arbitrary (NonNegative a)
-- (Num a, Ord a, Arbitrary a) => Arbitrary (Positive a)
-- (Ord a, Arbitrary a) => Arbitrary (OrderedList a)
-- (Ord a, Arbitrary a) => Arbitrary (Set a)
-- (RealFloat a, Arbitrary a) => Arbitrary (Complex a)
-- Arbitrary (f a) => Arbitrary (Alt * f a)
-- Arbitrary a => Arbitrary (Blind a)
-- Arbitrary a => Arbitrary (Const * a b)
-- Arbitrary a => Arbitrary (Constant * a b)
-- Arbitrary a => Arbitrary (Dual a)
-- Arbitrary a => Arbitrary (First a)
-- Arbitrary a => Arbitrary (Fixed a)
-- Arbitrary a => Arbitrary (Identity a)
-- Arbitrary a => Arbitrary (IntMap a)
-- Arbitrary a => Arbitrary (Last a)
-- Arbitrary a => Arbitrary (Maybe a)
-- Arbitrary a => Arbitrary (NonEmpty a)
-- Arbitrary a => Arbitrary (NonEmptyList a)
-- Arbitrary a => Arbitrary (Product a)
-- Arbitrary a => Arbitrary (Seq a)
-- Arbitrary a => Arbitrary (Shrink2 a)
-- Arbitrary a => Arbitrary (Smart a)
-- Arbitrary a => Arbitrary (Sum a)
-- Arbitrary a => Arbitrary (ZipList a)
-- Arbitrary a => Arbitrary [a]

-- (Arbitrary a, CoArbitrary a) => Arbitrary (Endo a)
-- (CoArbitrary a, Arbitrary b) => Arbitrary (a -> b)
-- (Arbitrary a, Arbitrary b) => Arbitrary (Either a b)
-- (Arbitrary a, Arbitrary b) => Arbitrary (a, b)
-- (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Map k v)
-- (Function a, CoArbitrary a, Arbitrary b) => Arbitrary (Fun a b)
-- (Function a, CoArbitrary a, Arbitrary b) => Arbitrary ((:->) a b)

-- (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (a, b, c)
-- (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (a, b, c, d)
-- (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e) => Arbitrary (a, b, c, d, e)
-- (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e, Arbitrary f) => Arbitrary (a, b, c, d, e, f)
-- (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e, Arbitrary f, Arbitrary g) => Arbitrary (a, b, c, d, e, f, g)
-- (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e, Arbitrary f, Arbitrary g, Arbitrary h) => Arbitrary (a, b, c, d, e, f, g, h)
-- (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e, Arbitrary f, Arbitrary g, Arbitrary h, Arbitrary i) => Arbitrary (a, b, c, d, e, f, g, h, i)
-- (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e, Arbitrary f, Arbitrary g, Arbitrary h, Arbitrary i, Arbitrary j) => Arbitrary (a, b, c, d, e, f, g, h, i, j)


-- data OfKind k = forall (a :: k). OfKind { getOfKind :: X a } deriving (Typeable)

-- instance Typeable k => Show (OfKind k) where
--   show = show . typeOf

data APasses = forall (a :: Passes). Typeable a => APasses { getAPasses :: X a }

instance Show APasses where
  show (APasses xa) = show xa


-- | Results in a `TypeError` if `False`
type family Assert (b :: Bool) (e :: ErrorMessage) :: Passes where
  Assert 'True  e = 'Passes
  Assert 'False e = TypeError e

type family ShowType2 (a0 :: k0) (a1 :: k1) :: ErrorMessage where
  ShowType2 a0 a1 = 'Text "\n  " ':<>: 'ShowType a0 ':<>: 'Text "\n  " ':<>: 'ShowType a1 ':<>: 'Text "\n"

-- | Results in a `TypeError` if `/=`. Reports its arguments upon error.
type family AssertEq (a :: k) (b :: k) :: Passes where
  AssertEq a b = Assert (a == b) ('Text "AssertEq failed with arguments:" ':<>: ShowType2 a b)

