module Data.Type.Test.Star where

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
import Test.QuickCheck.GenS
import Data.Type.Test.ShrinkType
import Data.Type.Test.Star.Utils


-- | Includes the types:
-- `Bool` `Char`, `Double`, `Float`, `Int`,
-- `Int8`, `Int16`, `Int32`, `Int64`,
-- `Integer`, `Ordering`, `Word`, `Word8`,
-- `Word16`, `Word32`, `Word64`, `()`,
-- `Natural`, `IntSet`, `OrdC`, `OrdB`,
-- `OrdA`, `C`, `B`, and `A`.
arbitraryExistsK0 :: [ GenS (Exists Nice)
                     ]
arbitraryExistsK0 =  [ existsX0 (Proxy :: Proxy Bool)
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
                            -- , (\(ExistsK x0 :: Exists Nice) -> existsX1 (Proxy :: Proxy []   ) x0)
                            , (\(ExistsK x0 :: Exists Nice) -> existsX1 (Proxy :: Proxy Maybe) x0)
                            -- , (\(ExistsK x0 :: Exists Nice) -> existsX1 (Proxy :: Proxy Seq  ) x0)
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


