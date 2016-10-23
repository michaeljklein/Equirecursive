{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeInType #-}
module Data.Type.Test.Internal where

import Control.Comonad
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Default
import Data.Default.Orphans
import Data.Exists
import Data.Foldable (toList)
import Data.Int
import Data.IntSet (IntSet)
import Data.Kind
import Data.Proxy
import Data.Sequence (Seq)
import Data.Type.Equality
import Data.Type.Test.ShrinkType
import Data.Type.Test.Star
import Data.Typeable
import Data.Word
import Data.X
import Data.X.Pair
import GHC.TypeLits
import Numeric.Natural
import Test.QuickCheck
import Test.QuickCheck.GenS
import Test.QuickCheck.Poly

-- TODO: add support for monad transformers
-- Consider adding `ToTree` to `Nice`/`NiceX`

-- | The maximum (total) depth of generated types.
-- On my computer, this allows GHCi to generate
-- and print about @1000@ types/sec
maxDepth :: Int
maxDepth = 30

natVals :: (KnownNat n0, KnownNat n1) => ((n0 :: Nat) :. (n1 :: Nat)) -> (Integer, Integer)
natVals ns = let (nx :. ny) = xPair ns in (natVal nx, natVal ny)

chooseNat :: (KnownNat n0, KnownNat n1) => ((n0 :: Nat) :. (n1 :: Nat)) -> Gen SomeNat
chooseNat ns = ((\n -> case n of Just s -> s) . someNatVal) <$> choose (natVals ns)


-- suchThat :: Gen a -> (a -> Bool) -> Gen a
-- Generates a value that satisfies a predicate.

-- suchThatMaybe :: Gen a -> (a -> Bool) -> Gen (Maybe a)
-- Tries to generate a value that satisfies a predicate.

-- | How nice is this? Really nice.
instance (Constraints c 'True, Constraints c 'False) => Arbitrary (ExistsK Bool c) where
  arbitrary = elements [ ExistsK (def :: X 'True)
                       , ExistsK (def :: X 'False)
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


-- | Given `NiceX`, this instance is a piece of cake.
instance CoArbitrary (ExistsK (Type -> Type) NiceX) where
  coarbitrary (ExistsK x) = coarbitrary x

-- | Given `NiceX`, this instance is a piece of cake.
instance CoArbitrary (ExistsK (Type -> Type -> Type) NiceX) where
  coarbitrary (ExistsK x) = coarbitrary x

-- | Given `NiceX`, this instance is a piece of cake.
instance CoArbitrary (ExistsK (Type -> Type -> Type -> Type) NiceX) where
  coarbitrary (ExistsK x) = coarbitrary x

-- | Given `NiceX`, this instance is a piece of cake.
instance CoArbitrary (ExistsK (Type -> Type -> Type -> Type -> Type) NiceX) where
  coarbitrary (ExistsK x) = coarbitrary x


-- | This performs explicit type application within `X`. It's sole
-- purpose is to orient operations within `ExistsK`.
xApp :: NiceX (a b) => X (a :: Type -> k) -> X (b :: Type) -> X (a b :: k)
xApp _ _ = def


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




