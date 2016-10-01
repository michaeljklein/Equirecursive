{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.X.Map.Internal where

import Prelude hiding ((.))
import Control.Category
import Control.Lens.Setter
import Control.Lens.Internal.Setter
import Control.Arrow
import GHC.Generics
import Data.Functor.Product
import Data.Kind
import Data.Bifunctor

-- -- | Map over a data structure and expand an `XX` into some `Rec`.
-- -- Alternatively, compress some `Rec` into some `XX`.
-- -- class XMap s t a b | s -> a, t -> b, s b -> t, t a -> s where
-- -- class XMap s t a b | s b -> t, t a -> s where
-- class XMap s t a b | s b -> t, t a -> s where
--   -- | `xmap` is the general setter, where @a@ must be `XX`
--   xmap :: Setter s t a b
--   -- | `ymap` is the complement to `xmap`, reversing its effects.
--   ymap :: Setter t s a b


-- -- | Extend `xmap` to an arbitrary `Functor`
-- xmapFunctor :: (Functor f, XMap s t a b) => Setter (f s) (f t) a b
-- xmapFunctor f = pure . fmap (untainted . xmap f)

-- -- | Extend `ymap` to an arbitrary `Functor`
-- ymapFunctor :: (Functor f, XMap s t a b) => Setter (f t) (f s) a b
-- ymapFunctor f = pure . fmap (untainted . ymap f)


-- -- | Extend `xmap` to a anything wrapping a `Functor`, given a wrapper and unwrapper
-- xmapWithFunctor :: forall t s (f :: * -> *) s0 t0 a b.  (XMap s0 t0 a b, Functor f) => (f t0 -> t) -> (s -> f s0) -> Setter s t a b
-- xmapWithFunctor to' from' f = pure . to' . fmap (untainted . xmap f) . from'

-- -- | Extend `ymap` to a anything wrapping a `Functor`, given a wrapper and unwrapper
-- ymapWithFunctor :: forall t s (f :: * -> *) t0 s0 a b.  (XMap s0 t0 a b, Functor f) => (f s0 -> t) -> (s -> f t0) -> Setter s t a b
-- ymapWithFunctor to' from' f = pure . to' . fmap (untainted . ymap f) . from'


-- -- | Extend `xmap` to an arbitrary `BiFunctor`
-- xmapBifunctor :: (Bifunctor p, XMap s0 t0 a b, XMap s1 t1 a b) => Setter (p s0 s1) (p t0 t1) a b
-- xmapBifunctor f = pure . bimap (untainted . xmap f) (untainted . xmap f)

-- -- | Extend `ymap` to an arbitrary `BiFunctor`
-- ymapBifunctor :: (Bifunctor p, XMap s0 t0 a b, XMap s1 t1 a b) => Setter (p t0 t1) (p s0 s1) a b
-- ymapBifunctor f = pure . bimap (untainted . ymap f) (untainted . ymap f)


-- -- | `xmapBifunctor` generalized to any `Bifunctor`-like type
-- xmapWithDualMap :: forall b a (f :: * -> *) c a1 a2 c1 a3 (f1 :: * -> *) b1.
--                    (XMap a3 c1 a2 b1, XMap a1 c a2 b1, Settable f1, Applicative f) =>
--                    ((a1 -> c) -> (a3 -> c1) -> a -> b) -> (a2 -> f1 b1) -> a -> f b
-- xmapWithDualMap m f = pure . m (untainted . xmap f) (untainted . xmap f)

-- -- | `ymapBifunctor` generalized to any `Bifunctor`-like type
-- ymapWithDualMap :: forall b a (f :: * -> *) c a1 a2 c1 a3 (f1 :: * -> *) b1.
--                    (XMap c1 a3 a2 b1, XMap c a1 a2 b1, Settable f1, Applicative f) =>
--                    ((a1 -> c) -> (a3 -> c1) -> a -> b) -> (a2 -> f1 b1) -> a -> f b
-- ymapWithDualMap m f = pure . m (untainted . ymap f) (untainted . ymap f)


-- -- | Extend `xmap` to an arbitrary `Arrow`
-- xmapArrow :: (Arrow ar, XMap s0 t0 a b, XMap s1 t1 a b) => Setter (ar s0 s1) (ar t0 t1) a b
-- xmapArrow f ar = pure $ arr (untainted . xmap f) . ar . arr (untainted . ymap f)

-- -- | Extend `ymap` to an arbitrary `Arrow`
-- ymapArrow :: (Arrow ar, XMap s0 t0 a b, XMap s1 t1 a b) => Setter (ar t0 t1) (ar s0 s1) a b
-- ymapArrow f ar = pure $ arr (untainted . ymap f) . ar . arr (untainted . xmap f)


-- -- | (These don't really belong here. Maybe in Utils?)
-- -- Map over a generic sum type.
-- sumMap  :: (f a -> f b) -> (g a -> g b) -> (f :+: g) a -> (f :+: g) b
-- sumMap f _ (L1 x) = L1 (f x)
-- sumMap _ g (R1 x) = R1 (g x)

-- -- | Map over a generic product type
-- prodMap :: (f a -> f b) -> (g a -> g b) -> (f :*: g) a -> (f :*: g) b
-- prodMap f g (x :*: y) = f x :*: g y

-- -- | Map over the product type from "Data.Functor.Product"
-- pairMap :: (f a -> f b) -> (g a -> g b) -> Product f g a -> Product f g b
-- pairMap f g (Pair x y) = Pair (f x) (g y)


