{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeInType #-}
module Data.Type.Test.Internal where

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


-- TODO: add support for monad transformers


instance Default Bool where
  def = False
instance Default a => Default (Either a t) where
  def = Left def
instance Default Char where
  def = '\0'
instance Default Natural where
  def = 0
instance Default A where
  def = A 0
instance Default B where
  def = B 0
instance Default C where
  def = C 0
instance Default OrdA where
  def = OrdA 0
instance Default OrdB where
  def = OrdB 0
instance Default OrdC where
  def = OrdC 0


-- | Nice types for nice type families
class    (Arbitrary a, CoArbitrary a, Typeable a, ShrinkType a, Default a) => Nice a

-- | This instance makes `Nice` a class synonym.
instance (Arbitrary a, CoArbitrary a, Typeable a, ShrinkType a, Default a) => Nice a


-- | Don't only recursively shrink the value, but also the type.
-- Made possible by @`ExistsK` `Type` `Nice`@'s truly.
class ShrinkType (a :: Type) where
  typeShrink :: a -> [Exists Nice]


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
-- instance ShrinkType a => ShrinkType (X a) where
--   typeShrink x = typeShrink (extract x) >>= (\(ExistsK y) -> [ExistsK y, ExistsK ((return :: b -> X b) y)])


class (IsStarX a ~ star) => ShrinkTypeX (a :: Type) (star :: Bool) where
  typeShrinkX :: a -> [Exists Nice]
instance ShrinkType a => ShrinkTypeX (X a) 'True where
  typeShrinkX x = typeShrink (extract x) >>= (\(ExistsK y) -> [ExistsK y, ExistsK ((return :: b -> X b) y)])
instance (IsStarX (X a) ~ 'False) => ShrinkTypeX (X a) 'False where
  typeShrinkX _ = []
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


-- | Includes the types:
-- `Bool` `Char`, `Double`, `Float`, `Int`,
-- `Int8`, `Int16`, `Int32`, `Int64`,
-- `Integer`, `Ordering`, `Word`, `Word8`,
-- `Word16`, `Word32`, `Word64`, `()`,
-- `Natural`, `IntSet`, `OrdC`, `OrdB`,
-- `OrdA`, `C`, `B`, and `A`.
arbitraryExistsK0 :: [ Gen (Exists Nice)
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
arbitraryExistsK1 :: [ Gen (Exists Nice)
                     ->Gen (Exists Nice)
                     ]
arbitraryExistsK1 = map (=<<) [ (\(ExistsK x0 :: Exists Nice) -> existsX1 (Proxy :: Proxy X    ) x0)
                            , (\(ExistsK x0 :: Exists Nice) -> existsX1 (Proxy :: Proxy []   ) x0)
                            , (\(ExistsK x0 :: Exists Nice) -> existsX1 (Proxy :: Proxy Maybe) x0)
                            , (\(ExistsK x0 :: Exists Nice) -> existsX1 (Proxy :: Proxy Seq  ) x0)
                            ]

-- | Includes the types:
-- `(,)`, `(->)`, and `Either`.
arbitraryExistsK2 :: [ Gen (Exists Nice)
                     ->Gen (Exists Nice)
                     ->Gen (Exists Nice)
                     ]
arbitraryExistsK2 = map bind2 [ (\(ExistsK x0 :: Exists Nice)
                                  (ExistsK x1 :: Exists Nice) -> existsX2 (Proxy :: Proxy (,)) x0 x1)
                              , (\(ExistsK x0 :: Exists Nice)
                                  (ExistsK x1 :: Exists Nice) -> existsX2 (Proxy :: Proxy (->)) x0 x1)
                              , (\(ExistsK x0 :: Exists Nice)
                                  (ExistsK x1 :: Exists Nice) -> existsX2 (Proxy :: Proxy Either) x0 x1)
                              ]

-- | Includes the type:
-- `(,,)`.
arbitraryExistsK3 :: [ Gen (Exists Nice)
                     ->Gen (Exists Nice)
                     ->Gen (Exists Nice)
                     ->Gen (Exists Nice)
                     ]
arbitraryExistsK3 = map bind3 [ (\(ExistsK x0 :: Exists Nice)
                                  (ExistsK x1 :: Exists Nice)
                                  (ExistsK x2 :: Exists Nice) -> existsX3 (Proxy :: Proxy (,,)) x0 x1 x2)
                              ]

-- | Includes the type:
-- `(,,,)`.
arbitraryExistsK4 :: [ Gen (Exists Nice)
                     ->Gen (Exists Nice)
                     ->Gen (Exists Nice)
                     ->Gen (Exists Nice)
                     ->Gen (Exists Nice)
                     ]
arbitraryExistsK4 = map bind4 [ (\(ExistsK x0 :: Exists Nice)
                                  (ExistsK x1 :: Exists Nice)
                                  (ExistsK x2 :: Exists Nice)
                                  (ExistsK x3 :: Exists Nice) -> existsX4 (Proxy :: Proxy (,,,)) x0 x1 x2 x3)
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
existsX0 :: Nice t => Proxy t -> Gen (Exists Nice)
existsX0 t = ExistsK <$> arbitraryX0 t

-- | Generate an @`ExistsK` `Type` `Nice`@ using a `Proxy`
-- and an `X`.
existsX1 :: Nice (t a) => Proxy t -> X a -> Gen (Exists Nice)
existsX1 t x0 = ExistsK <$> arbitraryX1 t x0

-- | See `existsX1`
existsX2 :: Nice (t a b) => Proxy t -> X a -> X b -> Gen (Exists Nice)
existsX2 t x0 x1 = ExistsK <$> arbitraryX2 t x0 x1

-- | See `existsX1`
existsX3 :: Nice (t a b c) => Proxy t -> X a -> X b -> X c -> Gen (Exists Nice)
existsX3 t x0 x1 x2 = ExistsK <$> arbitraryX3 t x0 x1 x2

-- | See `existsX1`
existsX4 :: Nice (t a b c d) => Proxy t -> X a -> X b -> X c -> X d -> Gen (Exists Nice)
existsX4 t x0 x1 x2 x3 = ExistsK <$> arbitraryX4 t x0 x1 x2 x3

-- | See `existsX1`
existsX5 :: Nice (t a b c d e) => Proxy t -> X a -> X b -> X c -> X d -> X e -> Gen (Exists Nice)
existsX5 t x0 x1 x2 x3 x4 = ExistsK <$> arbitraryX5 t x0 x1 x2 x3 x4

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
instance Arbitrary (Exists Nice) where
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
instance CoArbitrary (Exists Nice) where
  coarbitrary (ExistsK x) = coarbitrary x


instance CoArbitrary (ExistsK (Type -> Type) NiceX) where
  coarbitrary (ExistsK x) = coarbitrary x

instance Arbitrary (ExistsK (Type -> Type) NiceX) where
  arbitrary = oneof . concat $ [ return <$> arbitraryExistsKTypeType0
                               , (\x -> liftM x arbitrary) <$> arbitraryExistsKTypeType1
                               , (\x -> liftM2 x arbitrary arbitrary) <$> arbitraryExistsKTypeType2
                               , (\x -> liftM3 x arbitrary arbitrary arbitrary) <$> arbitraryExistsKTypeType3
                               ]

arbitraryExistsKTypeType0 :: [ ExistsK (Type -> Type) NiceX ]
arbitraryExistsKTypeType0 =  [ ExistsK (def :: X X)
                             , ExistsK (def :: X [])
                             , ExistsK (def :: X Maybe)
                             , ExistsK (def :: X Seq)
                             ]

arbitraryExistsKTypeType1 :: [ Exists Nice -> ExistsK (Type -> Type) NiceX ]
arbitraryExistsKTypeType1 =  [ (\(ExistsK x) -> ExistsK (xApp (def :: X (,)) x))
                             , (\(ExistsK x) -> ExistsK (xApp (def :: X (->)) x))
                             , (\(ExistsK x) -> ExistsK (xApp (def :: X Either) x))
                             ]

arbitraryExistsKTypeType2 :: [ Exists Nice -> Exists Nice -> ExistsK (Type -> Type) NiceX ]
arbitraryExistsKTypeType2 =  [ (\(ExistsK x) (ExistsK y) -> ExistsK (xApp (xApp (def :: X (,,)) x) y))
                             ]

arbitraryExistsKTypeType3 :: [ Exists Nice -> Exists Nice -> Exists Nice -> ExistsK (Type -> Type) NiceX ]
arbitraryExistsKTypeType3 =  [ (\(ExistsK x) (ExistsK y) (ExistsK z) -> ExistsK (xApp (xApp (xApp (def :: X (,,,)) x) y) z))
                             ]


instance Arbitrary (ExistsK (Type -> Type -> Type) NiceX) where
  arbitrary = oneof . concat $ [ return <$> arbitraryExistsKTypeTypeType0
                               , (\x -> liftM x arbitrary) <$> arbitraryExistsKTypeTypeType1
                               , (\x -> liftM2 x arbitrary arbitrary) <$> arbitraryExistsKTypeTypeType2
                               ]

arbitraryExistsKTypeTypeType0 :: [ ExistsK (Type -> Type -> Type) NiceX ]
arbitraryExistsKTypeTypeType0 =  [ ExistsK (def :: X (,))
                                 , ExistsK (def :: X (->))
                                 , ExistsK (def :: X Either)
                                 ]

arbitraryExistsKTypeTypeType1 :: [ Exists Nice -> ExistsK (Type -> Type -> Type) NiceX ]
arbitraryExistsKTypeTypeType1 =  [ (\(ExistsK x) -> ExistsK (xApp (def :: X (,,)) x))
                                 ]

arbitraryExistsKTypeTypeType2 :: [ Exists Nice -> Exists Nice -> ExistsK (Type -> Type -> Type) NiceX ]
arbitraryExistsKTypeTypeType2 =  [ (\(ExistsK x) (ExistsK y) -> ExistsK (xApp (xApp (def :: X (,,,)) x) y))
                                 ]


instance Arbitrary (ExistsK (Type -> Type -> Type -> Type) NiceX) where
  arbitrary = oneof . concat $ [ return <$> arbitraryExistsTypeTypeTypeType0
                               , (\x -> liftM x arbitrary) <$> arbitraryExistsTypeTypeTypeType1
                               ]

arbitraryExistsTypeTypeTypeType0 :: [ ExistsK (Type -> Type -> Type -> Type) NiceX ]
arbitraryExistsTypeTypeTypeType0 =  [ ExistsK (def :: X (,,))
                                    ]

arbitraryExistsTypeTypeTypeType1 :: [ Exists Nice -> ExistsK (Type -> Type -> Type -> Type) NiceX ]
arbitraryExistsTypeTypeTypeType1 =  [ (\(ExistsK x) -> ExistsK (xApp (def :: X (,,,)) x))
                                    ]


instance Arbitrary (ExistsK (Type -> Type -> Type -> Type -> Type) NiceX) where
  arbitrary = oneof . concat $ [ return <$> arbitraryExistsTypeTypeTypeTypeType0
                               ]

arbitraryExistsTypeTypeTypeTypeType0 :: [ ExistsK (Type -> Type -> Type -> Type -> Type) NiceX ]
arbitraryExistsTypeTypeTypeTypeType0 =  [ ExistsK (def :: X (,,,))
                                        ]

xApp :: NiceX (a b) => X (a :: Type -> k) -> X (b :: Type) -> X (a b :: k)
xApp _ _ = def





