{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}

{-# LANGUAGE TypeInType #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Reflexive where

import Control.Applicative
import Data.Bifunctor
import Data.Coerce
import Data.Kind
import Data.Proxy
import Data.Type.Equality
import Data.Type.Equality
import GHC.Generics
import GHC.Prim
import Unsafe.Coerce

-- let p = \x -> (x + 1, p) in p :: Int -> Refl (Int -> (Int, Refl X)
t1 :: Num a => t -> (a -> (a, t))
t1 = \r -> (\x -> (x + 1, r))

-- let t = (0, first (+1) t) in t :: Refl (Int, Refl X)
t2 :: (Bifunctor t, Num a) => t a b -> (a, t a b)
t2 = \r -> (0, first (+1) r)

-- let t = ((2*) <$> t, (2*).(+1) <$> t, 0) :: Refl (Refl X, Refl X, Int)
t3 :: (Functor t, Num a) => t a -> (t a, t a, a)
t3 = \r -> ((2*) <$> r, (+1).(*2) <$> r, 0)

t4 :: (Functor t, Num a) => t a -> (t a, a)
t4 = \r -> ((+1) <$> r, 0)


-- | `X` is a semi-magic type. As an inhabited type, it may be safely
-- coerced to and from using `pure`/`return` and `extract`.
-- As an uninhabited type, it is used in type families.
newtype X (a :: k) = X { getX :: Any }

-- | I find this really neat. When `X` is inhabited, it
-- behaves like a trivial newtype wrapper. Uninhabited, it
-- behaves like a unit type with phantom argument.
instance Eq a => Eq (X (a :: *)) where
  x == y = extract x == extract y

instance Eq (X (a :: k -> *)) where
  (==) _ _ = True


instance Ord a => Ord (X a) where
  x `compare` y = extract x `compare` extract y

instance Ord (X (a :: k -> *)) where
  compare _ _ = EQ


instance Show (a :: *) => Show (X (a :: *)) where
  show x = "X " ++ show (extract x)

instance Show (X (a :: k -> *)) where
  show _ = "X X"


instance Functor X where
  fmap :: (a -> b) -> X a -> X b
  fmap f (X x) = X (unsafeCoerce (f (unsafeCoerce x)))

class Functor w => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)
  extend :: (w a -> b) -> w a -> w b

instance Comonad X where
  extract (X x) = unsafeCoerce x
  duplicate = pure
  extend f = pure . f

instance Applicative X where
  pure x = X (unsafeCoerce x)

  f <*> x = pure (extract f $ extract x)

instance Monad X where
  return = pure
  x >>= f = f (extract x)

type XX = X X

-- xx :: a -> X X
-- xx x = X (unsafeCoerce x)

-- liftX :: f a -> X a
-- liftX = undefined

-- | Clean notation for coercing to `Rec` and `XX`
class XCoerce a b where
  (>-) :: a -> b -> b
  (-<) :: b -> a -> b
  (-<) = flip (>-)

instance XCoerce (X X) (Rec a) where
  (>-) :: X X -> Rec a -> Rec a
  (>-) (X x) r = Rec (unsafeCoerce x)

instance XCoerce a (X X) where
  (>-) x _ = X (unsafeCoerce x)

-- An empty value, solely to use with `(>-)`, `(-<)`
xx :: X (X :: * -> *)
xx = X (error "(xx :: X X) is uninhabited..and you can't extract the X...WTF did you do?")

-- Don't export constructors or accessors.
-- Not a newtype so that it can't be coerced to/from without `unsafeCoerce`.
data Rec a = Rec { getRec :: a }


-- Do I need more type maps?
-- E.g. MapPar1 like MapX


-- | The class of pseudo-equirecursive types.
class Recursive (r :: *) where
  -- | This should be the most general type that can be used to construct @r@
  type ForallRec r (t :: *) :: *

  -- | Hide the equi-recursion in a `Rec` wrapper
  rec :: (forall t. ForallRec r t) -> Rec r

  -- | Perform a single unfolding of an equi-recursive type
  pull :: Rec r -> XMap (Rec r) r

  -- | Perform a single folding a of an equi-recursive type
  push :: XMap (Rec r) r -> Rec r


instance Recursive (Int, X (X :: * -> *)) where
  type ForallRec (Int, X (X :: * -> *)) t = (Int, t) -> (Int, (Int, t))
  rec      r       = let t = second (>- xx) $ r t in Rec t
  pull     r@(Rec (a, x))  =     (a,  r     -< x )
  push (a, r@(Rec (b, x))) = Rec (a, (b, x) >- xx)


instance Recursive (X (X :: * -> *), Int) where
  type ForallRec (X (X :: * -> *), Int) t = (t, Int) -> ((t, Int), Int)
  rec   r          = let t = first (>- xx) $ r t in Rec t
  pull  r@(Rec (x, a))     =     ( r     -< x , a)
  push (r@(Rec (x, b)), a) = Rec ((x, b) >- xx, a)


-- | The fundamental fixed-point of `XMap`
instance Recursive (X (X :: * -> *)) where
  type ForallRec (X (X :: * -> *)) t = t -> X X
  rec _ = Rec xx
  pull = id
  push = id


-- | I'm not sure this one is a good idea...
instance Recursive (X (X a)) where
  type ForallRec (X (X a)) t = t -> X a
  rec f = let t = f t in Rec (return t)
  pull = undefined
  push = undefined


-- void :: V1 a
-- void = error "(void :: V1 a) accessed. Where'd you get a (V1 a)? Huh?"
instance Recursive a => Recursive (V1 a) where
  type ForallRec (V1 a) t = t -> V1 a
  rec x = Rec (x xx :: V1 a)

  -- | Note that both of these types are uninhabited.
  -- Also note that @forall a b. `XMap` a (`V1` b) ~ `V1` c@
  -- for some @c@.
  pull :: Rec (V1 a) -> XMap (Rec (V1 a)) (V1 a)
  pull (Rec v) = unsafeCoerce v

  -- | Just like with the `pull` definition, both
  -- types are uninhabited.
  push :: XMap (Rec (V1 a)) (V1 a) -> Rec (V1 a)
  push v = Rec (unsafeCoerce v)


instance Recursive a => Recursive (U1 a) where
  type ForallRec (U1 a) t = t -> U1 a
  rec _ = Rec (U1 :: U1 a)

  -- | Note that, similar to `V1`, `U1`s parameter is
  -- phantom and @forall a b. `XMap` a (`U1` b) ~ `U1` c@
  -- for some @c@
  pull :: Rec (U1 a) -> XMap (Rec (U1 a)) (U1 a)
  pull (Rec U1) = unsafeCoerce U1

  -- | In this case, @`Rec` `U1` :: forall a. Rec (U1 a)@,
  -- removing the need for coercion.
  push :: XMap (Rec (U1 a)) (U1 a) -> Rec (U1 a)
  push _ = Rec (U1 :: U1 a)


type family LiftFunc (f :: * -> *) (x :: *) :: *
type instance LiftFunc f (a -> b) = a -> f b


-- Par1 { unPar1 :: p } :: Par1 p
instance Recursive a => Recursive (Par1 a) where
  type ForallRec (Par1 a) t = LiftFunc Par1 (ForallRec a t)

  -- | Ok, so this is a bit nasty.
  -- It boils down to the fact that `Par1` is a newtype.
  -- Then, we just need to bypass GHC's lack of ability to convert
  -- @`Par1` (`ForallRec` a t)@ to @`ForallRec` a t@...
  rec f = Rec . Par1 . getRec . rec $ (unsafeCoerce f :: forall t. ForallRec a t)

  -- | Justification goes here
  pull :: Rec (Par1 a) -> XMap (Rec (Par1 a)) (Par1 a)
  pull (Rec (Par1 x)) = Par1 . unsafeCoerce . pull . Rec $ x

  -- | Ok, so first we strip the Par1's from the type. Then we push and go back.
  push :: XMap (Rec (Par1 a)) (Par1 a) -> Rec (Par1 a)
  push (Par1 x) = Rec . Par1 . getRec . (push :: XMap (Rec a) a -> Rec a) $ (unsafeCoerce x :: XMap (Rec a) a)


type family LiftRec1 f a b where
  LiftRec1 f a (f a) = Rec1 f a
  LiftRec1 f a (b -> c) = LiftRec1 f a b -> LiftRec1 f a c

instance (Recursive (f a), XMap1 f (Rec (f a)) a, Functor f) => Recursive (Rec1 f a) where
  type ForallRec (Rec1 f a) t = LiftRec1 f a (ForallRec (f a) t)

  -- rec :: (forall t. ForallRec (Rec1 f a) t) -> Rec (Rec1 f a)
  rec f = Rec . Rec1 . getRec . rec $ (unsafeCoerce f :: forall t. ForallRec (f a) t)

  pull :: Rec (Rec1 f a) -> XMap (Rec (Rec1 f a)) (Rec1 f a)
  pull (Rec (Rec1 x)) = Rec1 . fmap unsafeCoerce . pull . Rec $ x

  push :: XMap (Rec (Rec1 f a)) (Rec1 f a) -> Rec (Rec1 f a)
  push (Rec1 x) = Rec . Rec1 . getRec . (push :: XMap (Rec (f a)) (f a) -> Rec (f a)) $ (unsafeCoerce x :: XMap (Rec (f a)) (f a))

  -- xmap1 :: XMap (Rec (f a)) (f a) -> f (XMap (Rec (f a)) a)



-- K1 { unK1 :: c } :: K1 i c p
instance Recursive a => Recursive (K1 i a p) where
  type ForallRec (K1 i a p) t = LiftFunc Par1 (ForallRec a t)

  rec f = Rec . K1 . getRec . rec $ (unsafeCoerce f :: forall t. ForallRec a t)

  pull :: Rec (K1 i a p) -> XMap (Rec (K1 i a p)) (K1 i a p)
  pull (Rec (K1 x)) = K1 . unsafeCoerce . pull . Rec $ x

  push :: XMap (Rec (K1 i a p)) (K1 i a p) -> Rec (K1 i a p)
  push (K1 x) = Rec . K1 . getRec . (push :: XMap (Rec a) a -> Rec a) $ (unsafeCoerce x :: XMap (Rec a) a)


type family LiftM1 i c f a b where
  LiftM1 i c f a (f a) = M1 i c f a
  LiftM1 i c f a (b -> d) = LiftM1 i c f a b -> LiftM1 i c f a d

-- M1 { unM1 :: f p } :: M1 i c f p
instance (Recursive (f a), XMap1 f (Rec (f a)) a, Functor f) => Recursive (M1 i c f a) where
  type ForallRec (M1 i c f a) t = LiftM1 i c f a (ForallRec (f a) t)

  rec f = Rec . M1 . getRec . rec $ (unsafeCoerce f :: forall t. ForallRec (f a) t)

  pull :: Rec (M1 i c f a) -> XMap (Rec (M1 i c f a)) (M1 i c f a)
  pull (Rec (M1 x)) = M1 . fmap unsafeCoerce . pull . Rec $ x

  push :: XMap (Rec (M1 i c f a)) (M1 i c f a) -> Rec (M1 i c f a)
  push (M1 x) = Rec . M1 . getRec . (push :: XMap (Rec (f a)) (f a) -> Rec (f a)) $ (unsafeCoerce x :: XMap (Rec (f a)) (f a))


-- (cf t -> df t)
-- (cg t -> dg t)
-- (cf :+: cg) t -> (df :+: dg) t
type family LiftSum f g a b c where
  LiftSum f g a (f a) (g a) = (f :+: g) a
  LiftSum f g a (b0 -> b1) (c0 -> c1) = LiftSum f g a b0 c0 -> LiftSum f g a b1 c1

-- L1 (f p) | R1 (g p) :: (f :+: g) p
instance (Recursive (f a), Recursive (g a)) => Recursive ((f :+: g) a) where
  type ForallRec ((f :+: g) a) t = LiftSum f g a (ForallRec (f a) t) (ForallRec (g a) t)

  -- rec :: (forall t. ForallRec ((:+:) f g a) t) -> Rec ((:+:) f g a)
  rec = undefined

  pull :: Rec ((:+:) f g a) -> XMap (Rec ((:+:) f g a)) ((:+:) f g a)
  pull (Rec (L1 x)) = undefined
  pull (Rec (R1 y)) = undefined

  push :: XMap (Rec ((:+:) f g a)) ((:+:) f g a) -> Rec ((:+:) f g a)
  push (L1 x) = undefined
    where
      x' :: f (XMap (Rec ((:+:) f g a)) a)
      x' = x


  push (R1 y) = undefined
    where
      y' :: g (XMap (Rec ((:+:) f g a)) a)
      y' = y


-- (cf t -> df t)
-- (cg t -> dg t)
-- (cf :*: cg) t -> (df :*: dg) t
type family LiftProd f g a b c where
  LiftProd f g a (f a) (g a) = (f :*: g) a
  LiftProd f g a (b0 -> b1) (c0 -> c1) = LiftProd f g a b0 c0 -> LiftProd f g a b1 c1

-- | This is the naive implementation, which I believe should work given the kind of `(:*:)`:
--   (:*:) :: (* -> *) -> (* -> *) -> * -> *
-- Then, the remaining concerns will likely be with the base-case instances.
-- (f p) :*: (g p) :: (f :*: g) p
instance (Recursive (f a), Recursive (g a)) => Recursive ((f :*: g) a) where
  type ForallRec ((f :*: g) a) t = LiftProd f g a (ForallRec (f a) t) (ForallRec (g a) t)

  rec = undefined

  pull :: Rec ((:*:) f g a) -> XMap (Rec ((:*:) f g a)) ((:*:) f g a)
  pull (Rec (x :*: y)) = undefined
  -- easy: pull x, pull y, apply (:*:)? pushing is safer..

  push :: XMap (Rec ((:*:) f g a)) ((:*:) f g a) -> Rec ((:*:) f g a)
  push (x :*: y) = undefined
  -- easy: push x, push y, apply (:*:)

  -- rec      r       = let t = second (>- xx) $ r t in Rec t
  -- pull     r@(Rec (a, x))  =     (a,  r     -< x )
  -- push (a, r@(Rec (b, x))) = Rec (a, (b, x) >- xx)


-- cg t -> dg t
-- f (cg t) -> f (dg t)
-- f (g a) ~ (f :.: g) a

type family LiftComp f g a b where
  LiftComp f g a (g a) = (f :.: g) a
  LiftComp f g a (b0 -> b1) = LiftComp f g a b0 -> LiftComp f g a b1

-- Comp1 { unComp1 :: f (g a) } :: (f :.: g) a
instance (Functor f, Functor g, Recursive (g a), XMap1 g (Rec (g a)) a) => Recursive ((f :.: g) a) where
  type ForallRec ((f :.: g) a) t = LiftComp f g a (ForallRec (g a) t)

  -- rec :: (forall t. ForallRec ((:.:) f g a) t) -> Rec ((:.:) f g a)
  rec = undefined

  pull :: Rec ((:.:) f g a) -> XMap (Rec ((:.:) f g a)) ((:.:) f g a)
  pull (Rec (Comp1 x)) = Comp1 . fmap unsafeCoerce $ (pull . Rec) <$> x

  push :: XMap (Rec ((:.:) f g a)) ((:.:) f g a)   -> Rec ((:.:) f g a)
  -- push :: ((:.:) f g) (XMap (Rec ((:.:) f g a)) a) -> Rec ((:.:) f g a)
  push = Rec . Comp1 . fmap (getRec . push) . fmap (fmap unsafeCoerce) . unComp1

-- class Recursive (r :: *) where
--   type ForallRec r (t :: *) :: *
--   rec :: (forall t. ForallRec r t) -> Rec r
--   pull :: Rec r -> XMap (Rec r) r
--   push :: XMap (Rec r) r -> Rec r

type family LiftMaybe a where
  LiftMaybe (a -> b) = Maybe a -> Maybe b

instance Recursive a => Recursive (Maybe a) where
  type ForallRec (Maybe a) t = LiftMaybe (ForallRec a t)












xmap :: Rec a -> XMap (Rec a) (Rec a)
xmap = error "xmap is onlt defined to debug the UnXMap type family"

-- | @`XMap` a b@ replaces all instances of @`X` t@ in @b@ with @a@
type family XMap (a :: *) (b :: *) = (c :: *)

-- | Unary constructors, or those that are treated as such by `XMap`,
-- can always be pushed forward one step of `XMap`s evaluation.
--
-- It'd be great if the constraint could be
-- @forall a b. `XMap a (f b) ~ f (`XMap` a b)@, but GHC won't take it.
class (XMap a (f b) ~ f (XMap a b)) => XMap1 f a b where
  xmap1 :: XMap a (f b) -> f (XMap a b)
  xmap1 = id

-- XMap1 g (Rec (g a)) a

-- f (XMap (Rec (g a)) (g a)) -> f (g (XMap (Rec (g a)) a))

-- Comp1 :: f (g a) -> ((:.:) f g) a

-- f (XMap (Rec (g a)) (g a)) -> ((:.:) f g) (XMap (Rec ((:.:) f g a)) a)


-- f (XMap (Rec (g a)) (g a)) -> XMap (Rec (g a)) (f (g a))

-- f (XMap (Rec (g a)) (g a)) -> ((:.:) f g) (XMap (Rec ((:.:) f g a)) a)



-- | This is a key instance.
-- It allows one to recurse on non-@k -> *@ types.
-- It remains to be seen whether `XMap` should do so.
type instance XMap a (X (b :: k -> *)) = a
type instance XMap a (X (b :: * -> *)) = a -- This overlaps the (k -> *) instance, but `k` can be `*`.


type instance XMap a () = a
type instance XMap a Int = Int
type instance XMap a Bool = Bool

type instance XMap a (Rec b) = Rec (XMap a b)
type instance XMap a (V1 b) = V1 (XMap a b)
type instance XMap a (U1 b) = U1 (XMap a b)
type instance XMap a (Par1 b) = Par1 (XMap a b)
type instance XMap a (Rec1 f b) = Rec1 f (XMap a b)
type instance XMap a (K1 i b p) = K1 i (XMap a b) p
type instance XMap a (M1 i c f b) = M1 i c f (XMap a b)

type instance XMap a (b, c) = (XMap a b, XMap a c)
type instance XMap a (b -> c) = XMap a b -> XMap a c
type instance XMap a ((f :+: g) (b :: *)) = (f :+: g) (XMap a b) -- (:+:) is not poly-kinded, but (b :: *) is a reminder that X is a functor
type instance XMap a ((f :*: g) (b :: *)) = (f :*: g) (XMap a b) -- (:*:) is not poly-kinded, but (b :: *) is a reminder that X is a functor
type instance XMap a ((f :.: g) (b :: *)) = (f :.: g) (XMap a b) -- (:.:) is not poly-kinded, but (b :: *) is a reminder that X is a functor


instance XMap1 Rec a b
instance XMap1 V1 a b
instance XMap1 U1 a b
instance XMap1 Par1 a b
instance XMap1 (Rec1 f) a b
instance XMap1 (M1 i c f) a b
instance XMap1 (f :+: g) a b
instance XMap1 (f :*: g) a b
instance XMap1 (f :.: g) a b


-- | Here's what I'd use if I could figure out how to deduce the type
class XMap2 f where
  xmap2 :: XMap a (f b) -> f (XMap a b)
  bmap2 :: f (XMap a b) -> XMap a (f b)

instance XMap2 U1 where
  xmap2 = id
  bmap2 = id

instance XMap2 (f :.: g) where
  xmap2 = id
  bmap2 = id




-- Comp1 { unComp1 :: f (g a) } :: (f :.: g) a
-- L1 (f p) | R1 (g p) :: (f :+: g) p



-- | This is not fully tested and it seems quite possible
-- that it's not possible within the scope of this library.
type family UnXMap (a :: *) (b :: *) where
  UnXMap a a = X X
  UnXMap a () = ()
  UnXMap a Int = Int
  UnXMap a Bool = Bool
  UnXMap a (Rec a) = Rec (X X)
  UnXMap a (Rec b) = Rec (UnXMap a b)
  -- UnXMap a (a, b) = (X X, UnXMap a b)
  -- UnXMap a (b, a) = (UnXMap a b, X X)
  UnXMap a (b, c) = (UnXMap a b, UnXMap a c)
  UnXMap a (a -> b) = X X -> UnXMap a b

unxmap :: a -> b -> UnXMap a b
unxmap = error "unxmap is only defined for debugging the UnXMap type family"

-- rec :: (forall t. t -> (a, t)) -> Rec (a, X X)
-- rec f = let t = xx <$> f t in Rec t

-- rec' :: Functor c => (forall t. t -> c t) -> Rec (c (X X))
-- rec' f = let t = unsafeCoerce (f t) in Rec t



-- type family Plus a b = (sum :: Nat) | sum a -> b, sum b -> a where
-- type family H a b c = xyz | xyz a -> b c, xyz b -> a c


-- type family ToStar (a :: k) :: *
-- type instance ToStar (a :: *) = a
-- type instance ToStar (d :: Determination) = Determination




-- data Rec a = {


-- t -> (I -> (I, t))
-- Reflexive (I -> (I, X X))
-- pull :: Reflexive (I -> (I, Reflexive V)) -> I -> Reflexive (I -> (I, Reflexive V))










-- data R a = R { getR :: a }

-- r1 :: Num a => R (a -> (a, R (a -> a)))
-- r1 = let r = unsafeCoerce <$> t1 r in R r

-- r4 :: R (R Int, Int)
-- r4 = let r = first unsafeCoerce (t4 r) in R r



-- type family Res a :: Type
-- type instance Res () = ()
-- type instance Res (() -> () -> a) = () -> Res a


-- Maybe if I wrap the args in a polykinded thing, I can default

-- forall a. (W a, Int) -> (W Void, Int)



-- This is a poly-kinded, non-empty wrapper constructor.
-- If (a :: *) then MkStar a ~ a and it wraps a * type.
-- If (forall k. a :: k) then MkStar does not evaluate and it's a unit type!
-- data W (a :: k) = Wk | W { getW :: MkStar a }

-- instance Show (W a) where
--   show  Wk   = "Wk"
--   show (W _) = "W_"

-- type family MkStar (a :: k) :: *
-- type instance MkStar (a :: *) = a
-- type instance MkStar (d :: Determination) = Determination

-- data Det = Detd | UnDetd

-- data W2 (c :: k) (a :: k) where
--   Wk2 :: W2 UnDetd a
--   Wx2 :: MkStar a -> W2 Detd a

-- type family Default (a :: *) where
--   Default (W2 Detd a) = W a
--   Default          a  = W Void



-- data Determination = Determined | V deriving (Eq, Ord, Show)

-- type family ToStar (a :: k) :: *
-- type instance ToStar (a :: *) = a
-- type instance ToStar (d :: Determination) = Determination




-- type MaybeK a = MaybeDefaultingK Determined a

-- type MaybeDK d a = MaybeDefaultingK d a

-- data MaybeDefaultingK (d :: k0) (a :: k) where
--   NothingK ::             MaybeDefaultingK d  a
--   JustK    :: ToStar a -> MaybeDefaultingK Determined a

-- instance Show (ToStar a) => Show (MaybeDefaultingK d a) where
--   show (NothingK  ) = "NothingK"
--   show (JustK    x) = "JustK "   ++ show x

-- nothingK ::      MaybeK a
-- nothingK = NothingK

-- justK :: a -> MaybeK a
-- justK = JustK . tt

-- tt :: a -> ToStar a
-- tt = castWith Refl

-- toStar :: (a ~ ToStar a) => MaybeK a -> MaybeK (ToStar a)
-- toStar = id

-- instance Functor (MaybeDefaultingK d) where
--   fmap :: (a -> b) -> MaybeDK d a -> MaybeDK d b
--   fmap f (NothingK  ) = NothingK
--   fmap f (JustK    x) = JustK    (f x)

-- instance Applicative (MaybeDefaultingK d) where
--   pure :: a -> MaybeDK d a
--   pure = unsafeCoerce . JustK

--   (<*>) :: MaybeDK d (a -> b) -> MaybeDK d a -> MaybeDK d b
--   JustK f <*> x = fmap f x
--   _       <*> _ = NothingK

--   JustK _ *> y = y
--   _       *> _ = NothingK

-- instance Monad (MaybeDefaultingK d) where
--   return = pure
--   (JustK x) >>= k = k x
--   _         >>= _ = NothingK

--   fail _ = NothingK



-- type family MaybeDefaulting (a :: *) where
--   MaybeDefaulting (MaybeDefaultingK Determined a) = MaybeDefaultingK Determined a
--   MaybeDefaulting  x                              = MaybeDefaultingK V          V

-- type family Defaulting (a :: *) :: *
-- type instance Defaulting (MaybeDefaultingK d a) = MaybeDefaulting (MaybeDefaultingK d a)
-- type instance Defaulting () = ()
-- type instance Defaulting Int = Int
-- type instance Defaulting (a, b) = (Defaulting a, Defaulting b)
-- type instance Defaulting (a -> b) = Defaulting a -> Defaulting b

-- class Defaulting (a :: *) where
--   type DefaultingTo a :: *
--   defaulting     ::              a -> DefaultingTo a
--   defaultingFrom :: DefaultingTo a -> a
--   -- justK          ::              a -> MaybeDefaultingK Determined (DefaultingTo a)
--   -- justK = JustK . defaulting

-- instance Defaulting (MaybeDefaultingK d a) where
--   type DefaultingTo (MaybeDefaultingK d a) = MaybeDefaulting (MaybeDefaultingK d a)
--   defaulting     = unsafeCoerce
--   defaultingFrom = unsafeCoerce
--   -- justK = error "Cannot nest MaybeDefaultingK"

-- instance Defaulting () where
--   type DefaultingTo () = ()
--   defaulting     = id
--   defaultingFrom = id

-- instance Defaulting Int where
--   type DefaultingTo Int = Int
--   defaulting     = id
--   defaultingFrom = id

-- instance (Defaulting a, Defaulting b) => Defaulting (a, b) where
--   type DefaultingTo (a, b) = (DefaultingTo a, DefaultingTo b)
--   defaulting     (x, y) = (defaulting     x, defaulting     y)
--   defaultingFrom (x, y) = (defaultingFrom x, defaultingFrom y)

-- instance (Defaulting a, Defaulting b) => Defaulting (a -> b) where
--   type DefaultingTo (a -> b) = DefaultingTo a -> DefaultingTo b
--   defaulting     f = defaulting     . f . defaultingFrom
--   defaultingFrom f = defaultingFrom . f . defaulting

-- instance Defaulting a => Defaulting [a] where
--   type DefaultingTo [a] = [DefaultingTo a]
--   defaulting     = map defaulting
--   defaultingFrom = map defaultingFrom





-- data Wrap (d :: Determination) (a :: k) where
--   WrapK ::             Wrap Determined a
--   WrapX :: MkStar a -> Wrap Determined a


-- -- Use DefaultWrap to make instances, with MkStar to do the evaluation




-- instance Show (MkStar a) => Show (Wrap Determined a) where
--   show (WrapK  ) = "WrapK"
--   show (WrapX x) = "WrapX" ++ show x

-- instance (Eq (MkStar a)) => Eq (Wrap Determined a) where
--   WrapK   == WrapK   = True
--   WrapX x == WrapX y = x == y
--   _       == _       = False







-- -- instance Alternative (Wrap d) where
-- --   empty = WrapK
-- --   WrapK <|> y = y
-- --   x     <|> _ = x

-- type family DefaultWrap (a :: *) where
--   DefaultWrap (Wrap Determined a) = Wrap Determined a
--   DefaultWrap  w                  = Wrap V V




-- Darn impredicative types
-- ------------------------
-- data W3 (n :: *) (a :: k) where
--   Wk3 :: W3 Integer a
--   Wx3 :: Num n => MkStar a -> W3 (Num n => n) a
--
-- type family Default2 (a :: *) where
--   Default2 (W3 Integer a) = W Void
--   Default2 (W3 n a) = W a
--
-- type instance Default (W2 (d :: Det) a) = W2 Detd a
-- type instance Default (W2 (d :: k  ) a) = W2 UnDetd Detd


-- type family MkStar2 (a :: k) :: k
-- type instance MkStar2 (a :: *) = a

-- w1 :: W (Num Int)
-- w1 = Wk

-- w2 :: W Int
-- w2 = W 0

-- w3 :: W (Proxy (~))
-- w3 = W Proxy

-- w4 :: W ()
-- w4 = W ()

-- data Void





-- type family Def (a :: k) :: Type
-- type instance Def (W () -> W a) = W () -> Def (W a)
-- type instance Def (W a) = Defd a

-- type family Defd (a :: k) where
--   Defd (a :: k) = W ()

-- type instance (a :: k) == (b :: k) = EqPoly a b

-- type family EqPoly (a :: k) (b :: k) where
--  EqPoly a a = True
--  EqPoly a b = False

-- class T a where
--   tot :: Nf a -> (a, Int)

-- instance T () where
--   tot _ = (undefined, 0)

-- instance T a => T (() -> a) where
--   tot x = first unsafeCoerce (tot (($()) <$> x))

-- class Red a where
--   type Rd a :: *
--   red :: a -> Rd a
--   red = unsafeCoerce

-- fst' :: (a, Int) -> a
-- fst' = fst

-- snd' :: (R Int, a) -> a
-- snd' = snd

-- instance Red (R (((forall a. a, Int), forall b. b), forall b1. b1)) where
--   type Rd (R (((a, Int), b), b1)) = R (((R Int, Int), Int), Int)


-- type family Red a :: *
-- type instance Red (R (((a, Int), b), b1)) = R (((R Int, Int), Int), Int)

-- red :: a -> Red a
-- red = unsafeCoerce

-- let n = ((), N n) in n :: Refl ((), Refl (a -> N a))?
    -- \r -> ((), N r)

-- so far, we have homogenous infinite types and inhomogeneous infinite types
-- http://stackoverflow.com/questions/9566683/deliberately-defining-infinite-type-in-haskell
-- For homogeneous infinite types, should be able to help the type checker since it's like modular equality
-- For inhomogeneous infinite types, no easy ways out. Consider the following:



-- class C a where
--   type F a :: *
--   doC :: a -> F a

-- let t = (0 :: Int, doC <$> t) in t

-- Then t has the type:
--   (Int, (F Int, (F (F Int), (F (F (F Int)), ...)))) :: (Int, Refl (a -> (Int, F a)))

-- Actually, in this case, we can still properly compare equality to a finite type, we just need that C a => C (F a).
-- However, we have no hope to compare eqality with an infinite type.

-- (a0 , (a1   , (a2       , .. (an         , X))))
-- (Int, (F Int, (F (F Int), .. (F (F..(Int)), _))))

-- Here, we just zip the types, killing the final hole with a ($> someConstant).





-- (Int, (Int, (Int, ...))) -> Refl (Int, Refl X)

-- class Reflexive a where
--   type Reflex a :: *
--   refl_ :: Refl (Reflex a) -> a

-- instance Reflexive (Int, Refl X) where
--   type Reflex (Int, Refl X) = (Int, Refl X)
--   refl_ :: Refl (Int, Refl X) -> (Int, Refl X)
--   refl_ = getRefl

-- instance Reflexive (Int, (Int, Refl X)) where
--   type Reflex (Int, (Int, Refl X)) = (Int, Refl X)
--   refl_ :: Refl (Int, Refl X) -> (Int, (Int, Refl X))
--   refl_ (Refl (x, _) r) = (x, unsafeCoerce r)

-- instance Reflexive (Int, (Int, (Int, Refl X))) where
--   type Reflex (Int, (Int, (Int, Refl X))) = (Int, Refl X)
--   refl_ :: Refl (Int, Refl X) -> (Int, (Int, (Int, Refl X)))
--   refl_ r@(Refl (x, _) _) = (x, refl_ r)

-- instance Reflexive (Int, (Int, (Int, (Int, Refl X)))) where
--   type Reflex (Int, (Int, (Int, (Int, Refl X)))) = (Int, Refl X)
--   refl_ :: Refl (Int, Refl X) -> (Int, (Int, (Int, (Int, Refl X)
--   refl_ r@(Refl (x, _) _) = (x, refl_ r)

-- instance Reflexive (Int, r) => Reflexive (Int, (Int, r)) where
--   type Reflex (Int, (Int, r)) = Reflex (Int, r)
--   refl_ :: Refl (Reflex (Int, r)) -> (Int, (Int, r))

