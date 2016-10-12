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

-- (a -> b) -> (t -> a) -> (b -> t) -> (t -> t)

-- RecTypeA (Int, XY) = (Int, RecurseV)
-- RecTypeB (Int, XY) = (Int, (RecurseU (Int, RecurseV)))
-- RecType  (Int, XY) = RecTypeA (Int, XY) -> RecTypeB (Int, XY)


-- r1 = rec ((\x -> (0, return x)) :: (Int, RecurseV) -> (Int, RecurseU (Int, RecurseV)))
-- λ> fst (pull r1)
-- 0
-- λ> fst . pull . snd . pull $ r1
-- 0

-- let r2 = rec ((\(x, (y, xy)) -> (0, (1, return (x+1, (y+1, xy))))) :: (Int, (Int, RecurseV)) -> (Int, (Int, (RecurseU (Int, (Int, RecurseV))))))

-- let r3 = rec ((\(x, (y, (z, xy))) -> (0, (1, (2, return (x, (y, (z, xy))))))) :: (Int, (Int, (Int, RecurseV))) -> (Int, (Int, (Int, RecurseU (Int, (Int, (Int, RecurseV)))))))

-- λ> req r3 r2
-- Just Refl

-- λ> fst . pull . snd . snd . pull $ r3'
-- 2
-- *Main Data.Lifted Data.Recurse Data.Recurse.Equality Data.Recurse.Recursing Data.Recurse.TH Data.Recurse.TH.Test Data.X Data.X.Folding Data.X.Map Data.X.Map.TH Control.Monad.Trans.Maybe Data.Bifunctor
-- λ> let r3' = rcast r3 :: RecurseL (Int, (Int, X X))

-- λ> :t rec ((\f x -> return (fmap (+x) . f)) :: (Int -> (RecurseV, Int)) -> (Int -> RecurseU (Int -> (RecurseV, Int))))

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

-- (Int -> (RecurseV, Int)) -> Int -> RecurseU (Int -> (RecurseV, Int))

-- \f x -> return (fmap (+x) . f)



