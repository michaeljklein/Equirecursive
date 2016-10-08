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

-- | Since @`Recurse` `Locked`@ isn't exported, this should effectively
-- be equivalent to @forall t. t@
type RecurseV = Recurse 'Locked Void

-- | This is a mix of `lock` and `asTypeOf`
lockA :: forall a b. (a -> b) -> (RecurseU a -> XY)
lockA _ (RecurseUnlocked x) = X (unsafeCoerce x)

-- | Unlock an `XY` to a `RecurseV`.
-- Technically safe externally since there's no way to open @`Recurse` `Locked`@
unlockV :: XY -> RecurseV
unlockV (X x) = RecurseLocked (unsafeCoerce x)

-- | This is where the magic happens.
class ( XMapC (YMapF s Y 'Unlocked t) Y 'Locked Void
      , XMapF (YMapF s Y 'Unlocked t) Y 'Locked Void ~ t
      , XMapN (YMapF s Y 'Unlocked t)
      , XMapN s
      , YMapC s Y 'Unlocked t
      ) => Recursing t s where
        -- | Takes a function of form: @c `RecurseV` -> c2 (`RecurseU` (c `RecurseV`))@
        rec :: (t -> s) -> RecurseL (YMapF s Y 'Unlocked t)
        rec = RecurseLocked . fix . (. (xmapn %~ unlockV)) . ((.) =<< (ymapn %~) . lockA)

-- | This instance is simply to allow abbreviation, no other instances should exist (they'd overlap anyway)
instance ( XMapC (YMapF s Y 'Unlocked t) Y 'Locked Void
         , XMapF (YMapF s Y 'Unlocked t) Y 'Locked Void ~ t
         , XMapN (YMapF s Y 'Unlocked t)
         , XMapN s
         , YMapC s Y 'Unlocked t
         ) => Recursing t s where
        rec :: (t -> s) -> RecurseL (YMapF s Y 'Unlocked t)
        rec = RecurseLocked . fix . (. (xmapn %~ unlockV)) . ((.) =<< (ymapn %~) . lockA)

-- | Do not export
pullCoerce :: RecurseL a -> XY -> RecurseL a
pullCoerce _ (X x) = RecurseLocked (unsafeCoerce x)

-- | Pull a layer out of a `RecurseL`
pull :: ( XMapC s Y 'Locked s
        , XMapN s
        ) => RecurseL s -> XMapF s Y 'Locked s
pull x@(RecurseLocked y) = (xmapn %~ pullCoerce x) y

