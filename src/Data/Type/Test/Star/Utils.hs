module Data.Type.Test.Star.Utils where

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


