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
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE TypeInType #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Reflexive2 where

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

-- | The class of pseudo-equirecursive types.
class Recursive (r :: *) where
  -- | This should be the most general type that can be used to construct @r@
  type ForallRec r (t :: *) :: *
  -- type ForallRecFrom r (t :: *) :: *
  -- type ForallRecTo r (t :: *) :: *

  -- | Hide the equi-recursion in a `Rec` wrapper
  rec :: (forall t. ForallRec r t) -> Rec r

  -- | Perform a single unfolding of an equi-recursive type
  -- pull :: Rec r -> XMap (Rec r) r

  -- | Perform a single folding a of an equi-recursive type
  push :: XMap (Rec r) r -> Rec r

-- class Push (r :: *) where
--   push :: XMap (Rec r) r -> Rec r




class Pull (r :: *) where
  pull :: Rec r -> XMap (Rec r) r

instance Pull (X X) where
  pull = id

instance Pull a => Pull (V1 a) where
  -- | Note that both of these types are uninhabited.
  -- Also note that @forall a b. `XMap` a (`V1` b) ~ `V1` c@
  -- for some @c@.
  pull :: Rec (V1 a) -> XMap (Rec (V1 a)) (V1 a)
  pull (Rec v) = unsafeCoerce v

instance Pull a => Pull (U1 a) where
  -- | Note that, similar to `V1`, `U1`s parameter is
  -- phantom and @forall a b. `XMap` a (`U1` b) ~ `U1` c@
  -- for some @c@
  pull :: Rec (U1 a) -> XMap (Rec (U1 a)) (U1 a)
  pull (Rec U1) = unsafeCoerce U1

-- Par1 { unPar1 :: p } :: Par1 p
instance Pull a => Pull (Par1 a) where
  -- | Justification goes here
  pull :: Rec (Par1 a) -> XMap (Rec (Par1 a)) (Par1 a)
  pull (Rec (Par1 x)) = Par1 . unsafeCoerce . pull . Rec $ x

instance (Pull (f a), XMap1 f (Rec (f a)) a, Functor f) => Pull (Rec1 f a) where
  pull :: Rec (Rec1 f a) -> XMap (Rec (Rec1 f a)) (Rec1 f a)
  pull (Rec (Rec1 x)) = Rec1 . fmap unsafeCoerce . pull . Rec $ x

-- K1 { unK1 :: c } :: K1 i c p
instance Pull a => Pull (K1 i a p) where
  pull :: Rec (K1 i a p) -> XMap (Rec (K1 i a p)) (K1 i a p)
  pull (Rec (K1 x)) = K1 . unsafeCoerce . pull . Rec $ x

-- M1 { unM1 :: f p } :: M1 i c f p
instance (Pull (f a), XMap1 f (Rec (f a)) a, Functor f) => Pull (M1 i c f a) where
  pull :: Rec (M1 i c f a) -> XMap (Rec (M1 i c f a)) (M1 i c f a)
  pull (Rec (M1 x)) = M1 . fmap unsafeCoerce . pull . Rec $ x

-- L1 (f p) | R1 (g p) :: (f :+: g) p
instance (Pull (f a), Pull (g a)) => Pull ((f :+: g) a) where
  pull :: Rec ((:+:) f g a) -> XMap (Rec ((:+:) f g a)) ((:+:) f g a)
  pull (Rec (L1 x)) = undefined
  pull (Rec (R1 y)) = undefined

-- | This is the naive implementation, which I believe should work given the kind of `(:*:)`:
--   (:*:) :: (* -> *) -> (* -> *) -> * -> *
-- Then, the remaining concerns will likely be with the base-case instances.
-- (f p) :*: (g p) :: (f :*: g) p
instance (Pull (f a), Pull (g a)) => Pull ((f :*: g) a) where
  pull :: Rec ((:*:) f g a) -> XMap (Rec ((:*:) f g a)) ((:*:) f g a)
  pull (Rec (x :*: y)) = undefined
  -- easy: pull x, pull y, apply (:*:)? pushing is safer..

-- Comp1 { unComp1 :: f (g a) } :: (f :.: g) a
instance (Functor f, Functor g, Pull (g a), XMap1 g (Rec (g a)) a) => Pull ((f :.: g) a) where
  pull :: Rec ((:.:) f g a) -> XMap (Rec ((:.:) f g a)) ((:.:) f g a)
  pull (Rec (Comp1 x)) = Comp1 . fmap unsafeCoerce $ (pull . Rec) <$> x


pull :: Rec (I, XX) -> (I, Rec (I, XX))
pull r@(Rec (i, x)) = (i, Rec x >- r)

pull :: Rec [(I, XX)] -> [(I, Rec [(I, XX)])]
pull r@(Rec ixs) = fmap ((>- r) . Rec) <$> ixs

pull :: Rec (Maybe XX) -> Maybe (Rec (Maybe XX))
pull r@(Rec (Just x)) = Just (Rec x >- r)
pull   (Rec Nothing ) = Nothing

pull :: Rec (Maybe (I, XX)) -> Maybe (I, Rec (Maybe (I, XX)))
pull r@(Rec (Just (i, x))) = Just (i, Rec x >- r)
pull   (Rec Nothing      ) = Nothing

pull :: Rec (Maybe (I, XX, XX)) -> Maybe (I, Rec (Maybe (I,XX,XX)), Rec (Maybe (I,XX,XX)))
pull r@(Rec (Just (i, x1, x2))) = Just (i, Rec x1 >- r, Rec x2 >- r)
pull   (Rec Nothing           ) = Nothing

`pull` always works the same: pull r@(Rec x) = xMap ((>- r) . Rec) x
  where
    xMap :: (XX -> a) ....

xmap is a lens to all XX's in the type!!!!

it's hard to hear, but it looks like i needed a lens the whole time...
Thankfully, (X (a :: k -> *)) is nice and unique, allowing a generic lens in no time!

`push` is a bit different. push really needs a Rec accessor......
But a Rec accessor.... Is just an (X (a :: *)) accessor in disguise.........

now, if we can guarantee that forall (Rec a) `elem` t, a ~ a0, we can safely coerce all Rec a's to Rec a0's


instance Pull a => Pull (Maybe a) where
  pull :: Rec (Maybe a) -> XMap (Rec (Maybe a)) (Maybe a)
  -- pull :: Rec (Maybe a) -> Maybe (XMap (Rec (Maybe a)) a)
  pull (Rec x) = undefined
  -- pull (Rec (Nothing)) = Nothing

-- XMap (Rec a) a -> Maybe (XMap (Rec (Maybe a)) a)

------------------------------------------------------------------------------


-- | The fundamental fixed-point of `XMap`
instance Recursive (X (X :: * -> *)) where
  type ForallRec (X (X :: * -> *)) t = t -> X X
  rec _ = Rec xx
  push = id

-- -- | I'm not sure this one is a good idea...
-- instance Recursive (X (X a)) where
--   type ForallRec (X (X a)) t = t -> X a
--   rec f = let t = f t in Rec (return t)
--   pull = undefined
--   push = undefined

-- | Void is Void is Void
coerceV1 :: V1 a -> V1 b
coerceV1 = unsafeCoerce

-- void :: V1 a
-- void = error "(void :: V1 a) accessed. Where'd you get a (V1 a)? Huh?"
instance Recursive a => Recursive (V1 a) where
  type ForallRec     (V1 a) t = t -> V1 a
  rec x = Rec (c $ x)
    where
      c :: forall c. (forall t. ForallRec (V1 a) t) -> V1 c
      c = unsafeCoerce

  -- | Just like with the `pull` definition, both
  -- types are uninhabited.
  push :: XMap (Rec (V1 a)) (V1 a) -> Rec (V1 a)
  push v = Rec (unsafeCoerce v)


instance Recursive a => Recursive (U1 a) where
  type ForallRec (U1 a) t = t -> U1 a
  rec _ = Rec (U1 :: U1 a)

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

  push :: XMap (Rec (Rec1 f a)) (Rec1 f a) -> Rec (Rec1 f a)
  push (Rec1 x) = Rec . Rec1 . getRec . (push :: XMap (Rec (f a)) (f a) -> Rec (f a)) $ (unsafeCoerce x :: XMap (Rec (f a)) (f a))

  -- xmap1 :: XMap (Rec (f a)) (f a) -> f (XMap (Rec (f a)) a)



-- K1 { unK1 :: c } :: K1 i c p
instance Recursive a => Recursive (K1 i a p) where
  type ForallRec (K1 i a p) t = LiftFunc Par1 (ForallRec a t)

  rec f = Rec . K1 . getRec . rec $ (unsafeCoerce f :: forall t. ForallRec a t)

  push :: XMap (Rec (K1 i a p)) (K1 i a p) -> Rec (K1 i a p)
  push (K1 x) = Rec . K1 . getRec . (push :: XMap (Rec a) a -> Rec a) $ (unsafeCoerce x :: XMap (Rec a) a)


type family LiftM1 i c f a b where
  LiftM1 i c f a (f a) = M1 i c f a
  LiftM1 i c f a (b -> d) = LiftM1 i c f a b -> LiftM1 i c f a d

-- M1 { unM1 :: f p } :: M1 i c f p
instance (Recursive (f a), XMap1 f (Rec (f a)) a, Functor f) => Recursive (M1 i c f a) where
  type ForallRec (M1 i c f a) t = LiftM1 i c f a (ForallRec (f a) t)

  rec f = Rec . M1 . getRec . rec $ (unsafeCoerce f :: forall t. ForallRec (f a) t)

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

  push :: XMap (Rec ((:.:) f g a)) ((:.:) f g a)   -> Rec ((:.:) f g a)
  -- push :: ((:.:) f g) (XMap (Rec ((:.:) f g a)) a) -> Rec ((:.:) f g a)
  push = Rec . Comp1 . fmap (getRec . push) . fmap (fmap unsafeCoerce) . unComp1

-- -- class Recursive (r :: *) where
-- --   type ForallRec r (t :: *) :: *
-- --   rec :: (forall t. ForallRec r t) -> Rec r
-- --   pull :: Rec r -> XMap (Rec r) r
-- --   push :: XMap (Rec r) r -> Rec r

-- type family LiftMaybe a where
--   LiftMaybe (a -> b) = Maybe a -> Maybe b

-- instance Recursive a => Recursive (Maybe a) where
--   type ForallRec (Maybe a) t = LiftMaybe (ForallRec a t)













-- | @`XMap` a b@ replaces all instances of @`X` t@ in @b@ with @a@
type family XMap (a :: *) (b :: *) = (c :: *)

xmap :: Rec a -> XMap (Rec a) (Rec a)
xmap = error "xmap is onlt defined to debug the UnXMap type family"

-- | Unary constructors, or those that are treated as such by `XMap`,
-- can always be pushed forward one step of `XMap`s evaluation.
-- It'd be great if the constraint could be
-- @forall a b. `XMap a (f b) ~ f (`XMap` a b)@, but GHC won't take it.
class (XMap a (f b) ~ f (XMap a b)) => XMap1 f a b where
  xmap1 :: XMap a (f b) -> f (XMap a b)
  xmap1 = id

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
type instance XMap a (Maybe b) = Maybe (XMap a b) --- probably superflous

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
instance XMap1 Maybe a b
