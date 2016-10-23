{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Data.X.Pair where

import Data.X
import Data.Kind
import Data.Default
import Data.Typeable
import Data.Bifunctor
import GHC.TypeLits

infixr 1 .:
-- | `X`-level @(`:`)@
-- data (.:) (a :: Type) (b :: Type) = (.:) a b
data a .: b = (:.:) a b

instance Bifunctor (.:) where
  bimap f g (x :.: y) = f x :.: g y

type family (!!) (a :: Type) (b :: Nat) :: Type where
  (a .: b) !! 0 = a
  (a .: b) !! n = b !! (n-1)
  (a     ) !! n = TypeError ('Text "Data.X.Pair.!! called on:" ':$$: ShowType a ':$$: ShowType n)

type family (!..) (a :: Nat) (b :: Nat) (c :: Type) :: Type where
  (a !.. a) c = VoidX
  (a !.. 0) c = VoidX
  (0 !.. b) (c .: d) = c .: ((0     !.. (b-1)) d)
  (a !.. b) (c .: d) =      (((a-1) !.. (b-1)) d)

-- | Polykinded @(`.:`)@.
-- In a way, this is the binary form of `X`.
data (:.) (a0 :: k0) (a1 :: k1) = (:.) (ToStar a0) (ToStar a1) deriving (Typeable)

deriving instance (Eq (ToStar a0), Eq (ToStar a1)) => Eq (a0 :. a1)

deriving instance (Ord (ToStar a0), Ord (ToStar a1)) => Ord (a0 :. a1)

deriving instance (Show (ToStar a0), Show (ToStar a1)) => Show (a0 :. a1)

instance Bifunctor (:.) where
  bimap  f g ((:.) x y) = f x :. g y
  first  f   ((:.) x y) = f x :.   y
  second   g ((:.) x y) =   x :. g y

-- | Just like `swap`
swapX :: (a :. b) -> (b :. a)
swapX ((:.) x y) = y :. x


class (IsStarX (X a0) ~ star0, IsStarX (X a1) ~ star1) => XPair (a0 :: k0) (a1 :: k1) (star0 :: Bool) (star1 :: Bool) where
  xPair :: ((a0 :: k0) :. (a1 :: k1)) -> (X (a0 :: k0) :. X (a1 :: k1))

instance (IsStarX (X a0) ~ 'True, IsStarX (X a1) ~ 'True) => XPair (a0 :: Type) (a1 :: Type) 'True 'True where
  xPair (x :. y) = (return x :. return y)

instance (IsStarX (X a0) ~ 'True, IsStarX (X a1) ~ 'False) => XPair (a0 :: Type) (a1 :: k1) 'True 'False where
  xPair (x :. _) = (return x :. def)

instance (IsStarX (X a0) ~ 'False, IsStarX (X a1) ~ 'True) => XPair (a0 :: k0) (a1 :: Type) 'False 'True where
  xPair (_ :. y) = (def :. return y)

instance (IsStarX (X a0) ~ 'False, IsStarX (X a1) ~ 'False) => XPair (a0 :: k0) (a1 :: k1) 'False 'False where
  xPair _ = (def :. def)


type family XApp (a :: *) = (b :: *) | b -> a where
  XApp (X (c :: k -> k1) .: X (a :: k)) = X (c a)

