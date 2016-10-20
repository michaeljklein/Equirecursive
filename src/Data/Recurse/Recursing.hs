-- {-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeInType #-}

module Data.Recurse.Recursing where

import Data.Recurse
import Prelude hiding (id, (.))
import Data.Kind
import Data.X
import Data.X.Map
import Unsafe.Coerce
import Control.Category (Category(..))
import Data.Function (fix)
import Control.Comonad
import Data.Type.Equality
import Data.Proxy


-- TODO: Need a get type family that, given a constructor @c@,
-- either gets the unique @c a@ such that @c a `Elem` input@
-- or fails.
--
-- This will allow for
-- 1) a version of pull that works inside nested types,
-- 2) a real push function
-- 3) pushN and pullN functions.
--
-- Remember, also need to make a few fun generic things such as
-- - stateful infinite types
-- - MonoFunctor, etc, for infinite types (such as RecurseL (Int, XY))
-- - several infinite-type combinators
-- - Isos, Prisms
-- - Comonad instances for infinite types
-- - Infinite type transformers (Either a XY, anyone?)
-- - A nice family of infinite-type newtypes
-- - lenses for various infinite types. possibly even a generic destructor?
-- - I think there was an infinite-type solution to something with Conduits, like nested conduit tricks or something
--
-- Don't forget to start making theorem functions once all the classes are done.
-- I think GetX, Pull, Push, and rec' (given f, derive f' for RecurseL f') are the
-- last of them.

-- | This is a mix of `lock` and `asTypeOf`
-- Technically save since there is to exported way to open an `XY`
lockA :: forall a b. (a -> b) -> (RecurseU a -> XY)
lockA _ x = X (unsafeCoerce (extract x))

-- | Unlock an `XY` to a `RecurseV`.
-- Technically safe externally since there's no exported way to open @`Recurse` `Locked`@
unlockV :: XY -> RecurseV
unlockV (X x) = lock . return . unsafeCoerce $ x

-- | Do not export, this is rather unsafe
pullCoerce :: RecurseL a -> XY -> RecurseL a
pullCoerce _ (X x) = lock . return . unsafeCoerce $ x

type family RecType (a :: *) :: * where
  RecType a = MapT XY RecurseV a -> MapT XY (RecurseU (MapT XY RecurseV a)) a

amap :: a -> MapT XY RecurseV a
amap = unsafeCoerce

bmap :: MapT XY (RecurseU (MapT XY RecurseV a)) a -> a
bmap = unsafeCoerce

rec :: RecType a -> RecurseL a
rec f = lock . return . fix $ bmap . f . amap

pullG :: RecurseL a -> MapT XY (RecurseL a) a
pullG = unsafeCoerce



-- push



-- TODO: MapT from to a ~ b <=> _
-- fst $ pullT (undefined :: RecurseL (a, b))
  -- :: UnX
  --      (FoldX
  --         (X (,)
  --          .: MapTX
  --               (X X .: ((X X .: VoidX) .: VoidX))
  --               (X Recurse
  --                .: ((X 'Locked .: VoidX)
  --                    .: ((X (,)
  --                         .: (UnfoldXL VoidX (X a1) .: (UnfoldXL VoidX (X b1) .: VoidX)))
  --                        .: VoidX)))
  --               (UnfoldXL VoidX (X a1) .: (UnfoldXL VoidX (X b1) .: VoidX))))
  --    ~
  --    (a, b)
-- Distributivity of type equality over MapT from to
-- ((MapT XY () (a, b)) ~ (MapT XY () (c, d))) == (MapT XY () a ~ MapT XY () c, MapT XY () b ~ MapT XY () d)
--
-- One approach is to use a defaulting to a datatype in MapT. So then, we can pattern match on unevaluated 'type families'.
-- Another approach, is to apply the constraint distribution _before_ applying the other type families.
-- Still another approach, is to combine the type family with a GADT and do a little unsafe magic given that it always contains the type family
--   This approach appears to be untenable
--     type family F (a :: *) where
--       F (unx a) = a
--     (undefined :: F (UnX a)) = F (UnX a)

type family DistEq (c :: Constraint) :: Constraint

type instance DistEq ((a, b) ~ (c, d)) = (a ~ c, b ~ d)

type family (=~) (a :: k) (b :: k) :: Bool


type family (=~=) (a :: k) (b :: k) :: Constraint

type instance (=~=) (a, b) (c, d) = (a ~ c, b ~ d)

(~=~) :: forall (a :: k) (b :: k). a =~= b => Proxy a -> Proxy b -> a :~: b
(~=~) = unsafeCoerce Refl





-- r1 = rec ((\x -> (0, return x)) :: (Int, RecurseV) -> (Int, RecurseU (Int, RecurseV)))
-- λ> fst (pull r1)
-- 0
-- λ> fst . pull . snd . pull $ r1
-- 0
--
-- let r2 = rec ((\(x, (y, xy)) -> (0, (1, return (x+1, (y+1, xy))))) :: (Int, (Int, RecurseV)) -> (Int, (Int, (RecurseU (Int, (Int, RecurseV))))))
--
-- let r3 = rec ((\(x, (y, (z, xy))) -> (0, (1, (2, return (x, (y, (z, xy))))))) :: (Int, (Int, (Int, RecurseV))) -> (Int, (Int, (Int, RecurseU (Int, (Int, (Int, RecurseV)))))))
--
-- λ> req r3 r2
-- Just Refl
--
-- λ> fst . pull . snd . snd . pull $ r3'
-- 2
-- *Main Data.Lifted Data.Recurse Data.Recurse.Equality Data.Recurse.Recursing Data.Recurse.TH Data.Recurse.TH.Test Data.X Data.X.Folding Data.X.Map Data.X.Map.TH Control.Monad.Trans.Maybe Data.Bifunctor
-- λ> let r3' = rcast r3 :: RecurseL (Int, (Int, X X))
--
-- λ> :t rec ((\f x -> return (fmap (+x) . f)) :: (Int -> (RecurseV, Int)) -> (Int -> RecurseU (Int -> (RecurseV, Int))))
--
-- <interactive>:1:1: error:
--     • Couldn't match type ‘XMapF
--                              (YMapF
--                                 (Int -> RecurseU (Int -> (RecurseV, Int)))
--                                 Y
--                                 'Unlocked
--                                 (Int -> (RecurseV, Int)))
--                              Y
--                              'Locked
--                              Data.Void.Void’
--                      with ‘Int -> (RecurseV, Int)’
--         arising from a use of ‘rec’
--     • In the expression:
--         rec
--           ((\ f x -> return (fmap (+ x) . f)) ::
--              (Int -> (RecurseV, Int))
--              -> (Int -> RecurseU (Int -> (RecurseV, Int))))
--
-- (Int -> (RecurseV, Int)) -> Int -> RecurseU (Int -> (RecurseV, Int))
--
-- \f x -> return (fmap (+x) . f)



