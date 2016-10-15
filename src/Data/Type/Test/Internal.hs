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
import Control.Comonad
import Data.Foldable (toList)


-- | Unit type with @'`Passes` :: `Passes`@ for assertions.
data Passes = Passes deriving (Eq, Ord, Show)


-- | Nice types for type families
class    (Arbitrary a, CoArbitrary a, Typeable a, ShrinkType a) => Nice a

-- | This instance makes `Nice` a class synonym.
instance (Arbitrary a, CoArbitrary a, Typeable a, ShrinkType a) => Nice a

-- | Don't only recursively shrink the value, but also the type.
-- Made possible by @`ExistsK` `Type` `Nice`@'s truly.
class ShrinkType (a :: Type) where
  typeShrink :: a -> [ExistsK Type Nice]


instance ShrinkType Bool where typeShrink = fmap ExistsK . shrink . return
instance ShrinkType Char where typeShrink = fmap ExistsK . shrink . return
instance ShrinkType Double where typeShrink = fmap ExistsK . shrink . return
instance ShrinkType Float where typeShrink = fmap ExistsK . shrink . return
instance ShrinkType Int where typeShrink = fmap ExistsK . shrink . return
instance ShrinkType Int8 where typeShrink = fmap ExistsK . shrink . return
instance ShrinkType Int16 where typeShrink = fmap ExistsK . shrink . return
instance ShrinkType Int32 where typeShrink = fmap ExistsK . shrink . return
instance ShrinkType Int64 where typeShrink = fmap ExistsK . shrink . return
instance ShrinkType Integer where typeShrink = fmap ExistsK . shrink . return
instance ShrinkType Ordering where typeShrink = fmap ExistsK . shrink . return
instance ShrinkType Word where typeShrink = fmap ExistsK . shrink . return
instance ShrinkType Word8 where typeShrink = fmap ExistsK . shrink . return
instance ShrinkType Word16 where typeShrink = fmap ExistsK . shrink . return
instance ShrinkType Word32 where typeShrink = fmap ExistsK . shrink . return
instance ShrinkType Word64 where typeShrink = fmap ExistsK . shrink . return
instance ShrinkType () where typeShrink = fmap ExistsK . shrink . return
instance ShrinkType Natural where typeShrink = fmap ExistsK . shrink . return
instance ShrinkType IntSet where typeShrink = fmap ExistsK . shrink . return
instance ShrinkType OrdC where typeShrink = fmap ExistsK . shrink . return
instance ShrinkType OrdB where typeShrink = fmap ExistsK . shrink . return
instance ShrinkType OrdA where typeShrink = fmap ExistsK . shrink . return
instance ShrinkType C where typeShrink = fmap ExistsK . shrink . return
instance ShrinkType B where typeShrink = fmap ExistsK . shrink . return
instance ShrinkType A where typeShrink = fmap ExistsK . shrink . return


-- | This returns the shrinks of @a@ as well as the shrinks of @`X` a@.
-- In other words, it tries shrinking both the value and the type.
instance ShrinkType a => ShrinkType (X a) where
  typeShrink x = typeShrink (extract x) >>= (\(ExistsK y) -> [ExistsK y, ExistsK ((return :: b -> X b) y)])

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


-- | Includes the types:
-- `Bool` `Char`, `Double`, `Float`, `Int`,
-- `Int8`, `Int16`, `Int32`, `Int64`,
-- `Integer`, `Ordering`, `Word`, `Word8`,
-- `Word16`, `Word32`, `Word64`, `()`,
-- `Natural`, `IntSet`, `OrdC`, `OrdB`,
-- `OrdA`, `C`, `B`, and `A`.
arbitraryExistsK0 :: [ Gen (ExistsK Type Nice)
                     ]
arbitraryExistsK0 = [ existsX0 (Proxy :: Proxy Bool)
                    , existsX0 (Proxy :: Proxy Char)
                    , existsX0 (Proxy :: Proxy Double)
                    , existsX0 (Proxy :: Proxy Float)
                    , existsX0 (Proxy :: Proxy Int)
                    , existsX0 (Proxy :: Proxy Int8)
                    , existsX0 (Proxy :: Proxy Int16)
                    , existsX0 (Proxy :: Proxy Int32)
                    , existsX0 (Proxy :: Proxy Int64)
                    , existsX0 (Proxy :: Proxy Integer)
                    , existsX0 (Proxy :: Proxy Ordering)
                    , existsX0 (Proxy :: Proxy Word)
                    , existsX0 (Proxy :: Proxy Word8)
                    , existsX0 (Proxy :: Proxy Word16)
                    , existsX0 (Proxy :: Proxy Word32)
                    , existsX0 (Proxy :: Proxy Word64)
                    , existsX0 (Proxy :: Proxy ())
                    , existsX0 (Proxy :: Proxy Natural)
                    , existsX0 (Proxy :: Proxy IntSet)
                    , existsX0 (Proxy :: Proxy OrdC)
                    , existsX0 (Proxy :: Proxy OrdB)
                    , existsX0 (Proxy :: Proxy OrdA)
                    , existsX0 (Proxy :: Proxy C)
                    , existsX0 (Proxy :: Proxy B)
                    , existsX0 (Proxy :: Proxy A)
                    ]

-- | Includes the types:
-- `X`, `[]`, `Maybe`, and `Seq`.
arbitraryExistsK1 :: [ Gen (ExistsK Type Nice)
                     ->Gen (ExistsK Type Nice)
                     ]
arbitraryExistsK1 = map (=<<) [ (\(ExistsK x0 :: ExistsK Type Nice) -> existsX1 (Proxy :: Proxy X    ) x0)
                            , (\(ExistsK x0 :: ExistsK Type Nice) -> existsX1 (Proxy :: Proxy []   ) x0)
                            , (\(ExistsK x0 :: ExistsK Type Nice) -> existsX1 (Proxy :: Proxy Maybe) x0)
                            , (\(ExistsK x0 :: ExistsK Type Nice) -> existsX1 (Proxy :: Proxy Seq  ) x0)
                            ]

-- | Includes the types:
-- `(,)`, `(->)`, and `Either`.
arbitraryExistsK2 :: [ Gen (ExistsK Type Nice)
                     ->Gen (ExistsK Type Nice)
                     ->Gen (ExistsK Type Nice)
                     ]
arbitraryExistsK2 = map bind2 [ (\(ExistsK x0 :: ExistsK Type Nice)
                                  (ExistsK x1 :: ExistsK Type Nice) -> existsX2 (Proxy :: Proxy (,)) x0 x1)
                              , (\(ExistsK x0 :: ExistsK Type Nice)
                                  (ExistsK x1 :: ExistsK Type Nice) -> existsX2 (Proxy :: Proxy (->)) x0 x1)
                              , (\(ExistsK x0 :: ExistsK Type Nice)
                                  (ExistsK x1 :: ExistsK Type Nice) -> existsX2 (Proxy :: Proxy Either) x0 x1)
                              ]

-- | Includes the type:
-- `(,,)`.
arbitraryExistsK3 :: [ Gen (ExistsK Type Nice)
                     ->Gen (ExistsK Type Nice)
                     ->Gen (ExistsK Type Nice)
                     ->Gen (ExistsK Type Nice)
                     ]
arbitraryExistsK3 = map bind3 [ (\(ExistsK x0 :: ExistsK Type Nice)
                                  (ExistsK x1 :: ExistsK Type Nice)
                                  (ExistsK x2 :: ExistsK Type Nice) -> existsX3 (Proxy :: Proxy (,,)) x0 x1 x2)
                              ]

-- | Includes the type:
-- `(,,,)`.
arbitraryExistsK4 :: [ Gen (ExistsK Type Nice)
                     ->Gen (ExistsK Type Nice)
                     ->Gen (ExistsK Type Nice)
                     ->Gen (ExistsK Type Nice)
                     ->Gen (ExistsK Type Nice)
                     ]
arbitraryExistsK4 = map bind4 [ (\(ExistsK x0 :: ExistsK Type Nice)
                                  (ExistsK x1 :: ExistsK Type Nice)
                                  (ExistsK x2 :: ExistsK Type Nice)
                                  (ExistsK x3 :: ExistsK Type Nice) -> existsX4 (Proxy :: Proxy (,,,)) x0 x1 x2 x3)
                              ]

-- | Constrain the instance of `arbitrary` using a `Proxy`
arbitraryX0 :: Nice t => Proxy t -> Gen (X t)
arbitraryX0 _ = arbitrary

-- | Constrain the instance of `arbitrary` using a `Proxy`
-- and an `X`.
arbitraryX1 :: Nice (t a) => Proxy t -> X a -> Gen (X (t a))
arbitraryX1 _ _ = arbitrary

-- | See `arbitraryX1`
arbitraryX2 :: Nice (t a b) => Proxy t -> X a -> X b -> Gen (X (t a b))
arbitraryX2 _ _ _ = arbitrary

-- | See `arbitraryX1`
arbitraryX3 :: Nice (t a b c) => Proxy t -> X a -> X b -> X c -> Gen (X (t a b c))
arbitraryX3 _ _ _ _ = arbitrary

-- | See `arbitraryX1`
arbitraryX4 :: Nice (t a b c d) => Proxy t -> X a -> X b -> X c -> X d -> Gen (X (t a b c d))
arbitraryX4 _ _ _ _ _ = arbitrary

-- | See `arbitraryX1`
arbitraryX5 :: Nice (t a b c d e) => Proxy t -> X a -> X b -> X c -> X d -> X e -> Gen (X (t a b c d e))
arbitraryX5 _ _ _ _ _ _ = arbitrary

-- | Generate an @`ExistsK` `Type` `Nice`@ using a `Proxy`
existsX0 :: Nice t => Proxy t -> Gen (ExistsK Type Nice)
existsX0 t = existsK <$> arbitraryX0 t

-- | Generate an @`ExistsK` `Type` `Nice`@ using a `Proxy`
-- and an `X`.
existsX1 :: Nice (t a) => Proxy t -> X a -> Gen (ExistsK Type Nice)
existsX1 t x0 = existsK <$> arbitraryX1 t x0

-- | See `existsX1`
existsX2 :: Nice (t a b) => Proxy t -> X a -> X b -> Gen (ExistsK Type Nice)
existsX2 t x0 x1 = existsK <$> arbitraryX2 t x0 x1

-- | See `existsX1`
existsX3 :: Nice (t a b c) => Proxy t -> X a -> X b -> X c -> Gen (ExistsK Type Nice)
existsX3 t x0 x1 x2 = existsK <$> arbitraryX3 t x0 x1 x2

-- | See `existsX1`
existsX4 :: Nice (t a b c d) => Proxy t -> X a -> X b -> X c -> X d -> Gen (ExistsK Type Nice)
existsX4 t x0 x1 x2 x3 = existsK <$> arbitraryX4 t x0 x1 x2 x3

-- | See `existsX1`
existsX5 :: Nice (t a b c d e) => Proxy t -> X a -> X b -> X c -> X d -> X e -> Gen (ExistsK Type Nice)
existsX5 t x0 x1 x2 x3 x4 = existsK <$> arbitraryX5 t x0 x1 x2 x3 x4

-- | If `(=<<)` took another argument
bind2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bind2 = ((join .) .) . liftM2

-- | See `bind2`
bind3 :: Monad m => (a -> b -> c -> m d) -> m a -> m b -> m c -> m d
bind3 = (((join .) .) .) . liftM3

-- | See `bind2`
bind4 :: Monad m => (a -> b -> c -> d -> m e) -> m a -> m b -> m c -> m d -> m e
bind4 = ((((join .) .) .) .) . liftM4

-- | See `bind2`
bind5 :: Monad m => (a -> b -> c -> d -> e -> m f) -> m a -> m b -> m c -> m d -> m e -> m f
bind5 = (((((join .) .) .) .) .) . liftM5


-- | This recursively generates instances of @`ExistsK` `Type` `Nice`@,
-- using `arbitraryExistsK0`, `arbitraryExistsK1`, etc. Shrinking is
-- accomplished through `ShrinkType`.
instance Arbitrary (ExistsK Type Nice) where
  arbitrary = oneof [
      join $ elements arbitraryExistsK0
    , join $ elements arbitraryExistsK1 <*> return arbitrary
    , join $ elements arbitraryExistsK2 <*> return arbitrary <*> return arbitrary
    , join $ elements arbitraryExistsK3 <*> return arbitrary <*> return arbitrary <*> return arbitrary
    , join $ elements arbitraryExistsK4 <*> return arbitrary <*> return arbitrary <*> return arbitrary <*> return arbitrary
    ]

  shrink (ExistsK x) = typeShrink x

-- | Given `Nice`, this instance is a piece of cake.
-- Go ahead, check the source if you don't believe me.
instance CoArbitrary (ExistsK Type Nice) where
  coarbitrary (ExistsK x) = coarbitrary x




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



-- Has to derive n to be useful. Too out sidetracking for now.
-- class MultiCompose (n :: Nat) (as :: Type) | as -> n where
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


