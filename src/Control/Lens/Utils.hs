module Control.Lens.Utils where

import Control.Lens
import Control.Lens.Iso

fmapSetter :: Functor f => Setter s t a b -> Setter (f s) (f t) a b
fmapSetter s0 f = pure . fmap (untainted . s0 f)

isoSetter :: Iso s t s0 t0 -> Setter s0 t0 a b -> Setter s t a b
isoSetter i s f = pure . (Control.Lens.Iso.from i `under` (untainted . s f))

compIso :: Iso ((f :.: g) a) ((f :.: g) b) (f (g a)) (f (g b))
compIso = coerced

par1Iso :: Iso (Par1 a) (Par1 b) a b
par1Iso = coerced

rec1Iso :: Iso (Rec1 f a) (Rec1 f b) (f a) (f b)
rec1Iso = coerced

m1Iso :: Iso (M1 i c f a) (M1 i c f b) (f a) (f b)
m1Iso = coerced

coercedSetter :: (Coercible s s0, Coercible t t0) => Setter s0 t0 a b -> Setter s t a b
coercedSetter = isoSetter coerced


type family BiLensLike f s t a b where
  BiLensLike f s t (a0, a1) (b0, b1) = (a0 -> f b0) -> (a1 -> f b1) -> s -> f t

type BiLens s t a b = forall f. Functor f => BiLensLike f s t a b

type BiTraversal s t a b = forall f. Applicative f => BiLensLike f s t a b

type BiSetter s t a b = forall f. Settable f => BiLensLike f s t a b

biSwap :: BiLensLike f s t (a0, a1) (b0, b1) -> BiLensLike f s t (a1, a0) (b1, b0)
biSwap = flip

bim :: Monad m => LensLike m s t0 a0 b0 -> LensLike m t0 t a1 b1 -> BiLensLike m s t (a0, a1) (b0, b1)
bim f g h i x = join (g i <$> f h x)

biset :: Setter s t0 a0 b0 -> Setter t0 t a1 b1 -> BiSetter s t (a0, a1) (b0, b1)
biset f g h i = g i . untainted . f h

biSet :: Bifunctor p => BiSetter (p a0 a1) (p b0 b1) (a0, a1) (b0, b1)
biSet f g = pure . bimap (untainted . f) (untainted . g)

biSum :: BiLens ((f :+: g) a) ((f :+: g) b) (f a, g a) (f b, g b)
biSum f _ (L1 x) = L1 <$> f x
biSum _ g (R1 y) = R1 <$> g y

biEither :: BiLens (Either a0 a1) (Either b0 b1) (a0, a1) (b0, b1)
biEither f _ (Left  x) = Left  <$> f x
biEither _ g (Right y) = Right <$> g y

biProd :: BiTraversal ((f :*: g) a) ((f :*: g) b) (f a, g a) (f b, g b)
biProd f g (x :*: y) = liftA2 (:*:) (f x) (g y)

biTup :: BiTraversal (a0, a1) (b0, b1) (a0, a1) (b0, b1)
biTup f g (x, y) = liftA2 (,) (f x) (g y)

diSet :: Profunctor p => BiSetter (p a0 a1) (p b0 b1) (b0, a1) (a0, b1)
diSet f g = pure . dimap (untainted . f) (untainted . g)

biArrow :: Arrow arr => BiSetter (arr a0 a1) (arr b0 b1) (b0, a1) (a0, b1)
biArrow f g ar = pure $ arr (untainted . g) . ar . arr (untainted . f)

biSetter :: BiSetter s t (s0, s1) (t0, t1) -> Setter s0 t0 a b -> Setter s1 t1 a b -> Setter s t a b
biSetter f g h i = g i `f` h i

-- | (These don't really belong here. Maybe in Utils?)
-- Map over a generic sum type.
sumMap  :: (f a -> f b) -> (g a -> g b) -> (f :+: g) a -> (f :+: g) b
sumMap f _ (L1 x) = L1 (f x)
sumMap _ g (R1 x) = R1 (g x)

-- | Map over a generic product type
prodMap :: (f a -> f b) -> (g a -> g b) -> (f :*: g) a -> (f :*: g) b
prodMap f g (x :*: y) = f x :*: g y

-- | Map over the product type from "Data.Functor.Product"
pairMap :: (f a -> f b) -> (g a -> g b) -> Product f g a -> Product f g b
pairMap f g (Pair x y) = Pair (f x) (g y)


