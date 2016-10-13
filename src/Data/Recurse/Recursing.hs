{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Recurse.Recursing where

import Data.Recurse
import Prelude hiding (id, (.))
import Data.Kind
import Data.X
import Data.X.Map hiding (XMap)
import Unsafe.Coerce
import Control.Category (Category(..))
import Data.Function (fix)
import Control.Lens.Setter ((%~))
import Data.Void
import Control.Comonad
import Data.Type.Equality
import Data.Proxy

-- | Since @`Recurse` `Locked`@ isn't exported, this should effectively
-- be equivalent to @forall t. t@
type RecurseV = Recurse 'Locked Void

-- xmapn :: (XY -> RecurseL b)
-- ymapn :: (RecurseU b -> XY)

-- | This is a mix of `lock` and `asTypeOf`
lockA :: forall a b. (a -> b) -> (RecurseU a -> XY)
lockA _ x = X (unsafeCoerce (extract x))

-- | Unlock an `XY` to a `RecurseV`.
-- Technically safe externally since there's no way to open @`Recurse` `Locked`@
unlockV :: XY -> RecurseV
unlockV (X x) = lock . return . unsafeCoerce $ x

-- | This is where the magic happens.
class ( XMapC (YMapF s Y 'Unlocked t) Y 'Locked Void
      , XMapF (YMapF s Y 'Unlocked t) Y 'Locked Void ~ t
      , XMapN (YMapF s Y 'Unlocked t)
      , XMapN s
      , YMapC s Y 'Unlocked t
      ) => Recursing t s where
        -- | Takes a function of form: @c `RecurseV` -> c2 (`RecurseU` (c `RecurseV`))@
        rec :: (t -> s) -> RecurseL (YMapF s Y 'Unlocked t)
        rec = (lock . return) . fix . (. (xmapn %~ unlockV)) . ((.) =<< (ymapn %~) . lockA)

-- | This instance is simply to allow abbreviation, no other instances should exist (they'd overlap anyway)
instance ( XMapC (YMapF s Y 'Unlocked t) Y 'Locked Void
         , XMapF (YMapF s Y 'Unlocked t) Y 'Locked Void ~ t
         , XMapN (YMapF s Y 'Unlocked t)
         , XMapN s
         , YMapC s Y 'Unlocked t
         ) => Recursing t s where
        -- rec :: (t -> s) -> RecurseL (YMapF s Y 'Unlocked t)
        -- rec = RecurseLocked . fix . (. (xmapn %~ unlockV)) . ((.) =<< (ymapn %~) . lockA)

-- | Do not export
pullCoerce :: RecurseL a -> XY -> RecurseL a
pullCoerce _ (X x) = lock . return . unsafeCoerce $ x

-- | Pull a layer out of a `RecurseL`
pull :: ( XMapC s Y 'Locked s
        , XMapN s
        ) => RecurseL s -> XMapF s Y 'Locked s
pull x@(Recurse y) = (xmapn %~ pullCoerce x) y


type family RecTypeA (a :: *) :: * where
  RecTypeA a = MapT XY RecurseV a

type family RecTypeB (a :: *) :: * where
  RecTypeB a = MapT XY (RecurseU (MapT XY RecurseV a)) a

type family RecType (a :: *) :: * where
  RecType a = MapT XY RecurseV a -> MapT XY (RecurseU (MapT XY RecurseV a)) a


amap :: a -> MapT XY RecurseV a
amap = unsafeCoerce

bmap :: MapT XY (RecurseU (MapT XY RecurseV a)) a -> a
bmap = unsafeCoerce

recT :: RecType a -> RecurseL a
recT f = lock . return . fix $ bmap . f . amap

pullT :: RecurseL a -> MapT XY (RecurseL a) a
pullT = unsafeCoerce

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

-- distEq :: (c => a) -> Maybe (DistEq c)
-- distEq = undefined

-- λ> :t fst $ pullT (undefined :: (a, RecurseL (b, c)))
--
-- <interactive>:1:14: error:
--     • Couldn't match type ‘(a1, RecurseL (b1, c0))’
--                      with ‘Recurse 'Locked a0’
--       Expected type: RecurseL a0
--         Actual type: (a1, RecurseL (b1, c0))
--     • In the first argument of ‘pullT’, namely
--         ‘(undefined :: (a, RecurseL (b, c)))’
--       In the second argument of ‘($)’, namely
--         ‘pullT (undefined :: (a, RecurseL (b, c)))’
--       In the expression: fst $ pullT (undefined :: (a, RecurseL (b, c)))





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



