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
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.Default.Orphans

import Control.Lens.Setter
import Control.Lens.Internal.Setter
-- import Data.X.Folding
import Data.Type.Bool


-- class SetResultAtom s t a b atom => SetResult s t a b | s -> a, t -> b, s b -> t, t a -> s where
--   setResult :: Setter s t a b
--   setResult = setResultAtom

-- instance SetResultAtom s t a b atom => SetResult s t a b

type family IsFunction (a :: Type) :: Bool where
  IsFunction ((->) a b) = 'True
  IsFunction (     a  ) = 'False

type family FunctionDepth (a :: Type) :: Nat where
  FunctionDepth ((->) a b) = 1 + FunctionDepth b
  FunctionDepth (     a  ) = 0

type family ResultOf (a :: Type) :: Type where
  ResultOf ((->) a b) = ResultOf b
  ResultOf (     a  ) =          a

type family MapResult (r :: Type) (a :: Type) :: Type where
  MapResult r ((->) a b) = a -> MapResult r b
  MapResult r (     a  ) =                r

class ((s == a) ~ eq1, (t == b) ~ eq2) => SetResult s t a b eq1 eq2 | s -> a, t -> b, s b -> t, t a -> s where
  setResult :: Setter s t a b

instance SetResult s t s t 'True 'True where
  setResult = id

instance ((s == a) ~ 'True, (t == b) ~ 'False, TypeError ('Text "err")) => SetResult s t a b 'True 'False where
  setResult = error "invalid SetResult instance"

instance ((s == a) ~ 'False, (t == b) ~ 'True, TypeError ('Text "err")) => SetResult s t a b 'False 'True where
  setResult = error "invalid SetResult instance"

instance (((c -> s) == a) ~ 'False, ((c -> t) == b) ~ 'False, SetResult s t a b 'True 'True) => SetResult (c -> s) (c -> t) a b 'False 'False where
  setResult f x = pure $ (untainted . setResult f) . x

-- class SetResult s t a b | s -> a, t -> b, s b -> t, t a -> s where
--   setResult :: Setter s t a b

-- instance SetResult s t a b eq => SetResult (c -> s) (c -> t) a b 'False where
--   setResult f x = pure $ (untainted . setResult f) . x

-- instance SetResult s t s t 'True where
--   setResult = id


-- (./) :: (a -> b) -> s -> t
-- (./) =

-- λ> :t (.)
-- (.) ∷ (b → c) → (a → b) → a → c

-- λ> :t (.) . (.)
-- (.) . (.) ∷ (b → c) → (a → a1 → b) → a → a1 → c

-- λ> :t (.) . (.) . (.)
-- (.) . (.) . (.) ∷ (b → c) → (a → a1 → a2 → b) → a → a1 → a2 → c

-- λ> :t (.) . (.) . (.) . (.)
-- (.) . (.) . (.) . (.)
--   ∷ (b → c) → (a → a1 → a2 → a3 → b) → a → a1 → a2 → a3 → c

-- TODO: add support for monad transformers

-- | The maximum depth of generated types
-- TODO: Tune
maxDepth :: Int
maxDepth = 4



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

type GenS a = StateT Int Gen a

arbitraryS :: Arbitrary a => GenS a
arbitraryS = decrementS >> lift arbitrary

decrementS :: GenS ()
decrementS = modify (+ (-1))

oneofS :: [GenS a] -> GenS a
oneofS xs = lift (choose (0, length xs - 1)) >>= (xs !!)

elementS :: [a] -> GenS a
elementS = lift . elements



-- | Includes the types:
-- `Bool` `Char`, `Double`, `Float`, `Int`,
-- `Int8`, `Int16`, `Int32`, `Int64`,
-- `Integer`, `Ordering`, `Word`, `Word8`,
-- `Word16`, `Word32`, `Word64`, `()`,
-- `Natural`, `IntSet`, `OrdC`, `OrdB`,
-- `OrdA`, `C`, `B`, and `A`.
arbitraryExistsK0 :: [ GenS (Exists Nice)
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
arbitraryExistsK1 :: [ GenS (Exists Nice)
                     ->GenS (Exists Nice)
                     ]
arbitraryExistsK1 = map (=<<) [ (\(ExistsK x0 :: Exists Nice) -> existsX1 (Proxy :: Proxy X    ) x0)
                            , (\(ExistsK x0 :: Exists Nice) -> existsX1 (Proxy :: Proxy []   ) x0)
                            , (\(ExistsK x0 :: Exists Nice) -> existsX1 (Proxy :: Proxy Maybe) x0)
                            , (\(ExistsK x0 :: Exists Nice) -> existsX1 (Proxy :: Proxy Seq  ) x0)
                            ]

-- | Includes the types:
-- `(,)`, `(->)`, and `Either`.
arbitraryExistsK2 :: [ GenS (Exists Nice)
                     ->GenS (Exists Nice)
                     ->GenS (Exists Nice)
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
arbitraryExistsK3 :: [ GenS (Exists Nice)
                     ->GenS (Exists Nice)
                     ->GenS (Exists Nice)
                     ->GenS (Exists Nice)
                     ]
arbitraryExistsK3 = map bind3 [ (\(ExistsK x0 :: Exists Nice)
                                  (ExistsK x1 :: Exists Nice)
                                  (ExistsK x2 :: Exists Nice) -> existsX3 (Proxy :: Proxy (,,)) x0 x1 x2)
                              ]

-- | Includes the type:
-- `(,,,)`.
arbitraryExistsK4 :: [ GenS (Exists Nice)
                     ->GenS (Exists Nice)
                     ->GenS (Exists Nice)
                     ->GenS (Exists Nice)
                     ->GenS (Exists Nice)
                     ]
arbitraryExistsK4 = map bind4 [ (\(ExistsK x0 :: Exists Nice)
                                  (ExistsK x1 :: Exists Nice)
                                  (ExistsK x2 :: Exists Nice)
                                  (ExistsK x3 :: Exists Nice) -> existsX4 (Proxy :: Proxy (,,,)) x0 x1 x2 x3)
                              ]

-- | Constrain the instance of `arbitrary` using a `Proxy`
arbitraryX0 :: Nice t => Proxy t -> GenS (X t)
arbitraryX0 _ = arbitraryS

-- | Constrain the instance of `arbitrary` using a `Proxy`
-- and an `X`.
arbitraryX1 :: Nice (t a) => Proxy t -> X a -> GenS (X (t a))
arbitraryX1 _ _ = arbitraryS

-- | See `arbitraryX1`
arbitraryX2 :: Nice (t a b) => Proxy t -> X a -> X b -> GenS (X (t a b))
arbitraryX2 _ _ _ = arbitraryS

-- | See `arbitraryX1`
arbitraryX3 :: Nice (t a b c) => Proxy t -> X a -> X b -> X c -> GenS (X (t a b c))
arbitraryX3 _ _ _ _ = arbitraryS

-- | See `arbitraryX1`
arbitraryX4 :: Nice (t a b c d) => Proxy t -> X a -> X b -> X c -> X d -> GenS (X (t a b c d))
arbitraryX4 _ _ _ _ _ = arbitraryS

-- | See `arbitraryX1`
arbitraryX5 :: Nice (t a b c d e) => Proxy t -> X a -> X b -> X c -> X d -> X e -> GenS (X (t a b c d e))
arbitraryX5 _ _ _ _ _ _ = arbitraryS

-- | Generate an @`ExistsK` `Type` `Nice`@ using a `Proxy`
existsX0 :: Nice t => Proxy t -> GenS (Exists Nice)
existsX0 t = ExistsK <$> arbitraryX0 t

-- | Generate an @`ExistsK` `Type` `Nice`@ using a `Proxy`
-- and an `X`.
existsX1 :: Nice (t a) => Proxy t -> X a -> GenS (Exists Nice)
existsX1 t x0 = ExistsK <$> arbitraryX1 t x0

-- | See `existsX1`
existsX2 :: Nice (t a b) => Proxy t -> X a -> X b -> GenS (Exists Nice)
existsX2 t x0 x1 = ExistsK <$> arbitraryX2 t x0 x1

-- | See `existsX1`
existsX3 :: Nice (t a b c) => Proxy t -> X a -> X b -> X c -> GenS (Exists Nice)
existsX3 t x0 x1 x2 = ExistsK <$> arbitraryX3 t x0 x1 x2

-- | See `existsX1`
existsX4 :: Nice (t a b c d) => Proxy t -> X a -> X b -> X c -> X d -> GenS (Exists Nice)
existsX4 t x0 x1 x2 x3 = ExistsK <$> arbitraryX4 t x0 x1 x2 x3

-- | See `existsX1`
existsX5 :: Nice (t a b c d e) => Proxy t -> X a -> X b -> X c -> X d -> X e -> GenS (Exists Nice)
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


arbitraryNice :: GenS (Exists Nice)
arbitraryNice = do
  n <- get
  if n < 1
     then    join $ elementS arbitraryExistsK0
  else do
    decrementS
    oneofS [ join $ elementS arbitraryExistsK0
           , join $ elementS arbitraryExistsK1 <*> return arbitraryNice
           , join $ elementS arbitraryExistsK2 <*> return arbitraryNice <*> return arbitraryNice
           , join $ elementS arbitraryExistsK3 <*> return arbitraryNice <*> return arbitraryNice <*> return arbitraryNice
           , join $ elementS arbitraryExistsK4 <*> return arbitraryNice <*> return arbitraryNice <*> return arbitraryNice <*> return arbitraryNice
           ]

-- | This recursively generates instances of @`ExistsK` `Type` `Nice`@,
-- using `arbitraryExistsK0`, `arbitraryExistsK1`, etc. Shrinking is
-- accomplished through `ShrinkType`.
instance Arbitrary (Exists Nice) where
  arbitrary = fst <$> runStateT arbitraryNice maxDepth
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





