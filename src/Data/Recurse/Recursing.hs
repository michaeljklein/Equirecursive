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

-- | Rename to Data.Recurse.Recursing
module Data.Recurse.Recursing where

import Data.Recurse
-- import Data.X
-- import Data.X.Map
-- import Control.Lens.Setter
import Prelude hiding (id, (.))
import Data.Kind
import Data.X
import Data.X.Map hiding (XMap)
import Unsafe.Coerce
import Control.Category (Category(..))
import Data.Function (fix)
import Control.Lens.Setter ((%~))
import Data.Void

-- class Recursing (a :: *) where
--   type RecursingBuilder a t :: *
  -- rec :: (forall t. RecursingBuilder a t) -> Recurse 'Locked a

type Y = (X :: * -> *)
type XY = X Y
type RecurseV = Recurse 'Locked Void

lockA :: forall a b. (a -> b) -> (RecurseU a -> XY)
lockA _ (RecurseUnlocked x) = X (unsafeCoerce x)

unlockV :: XY -> RecurseV
unlockV (X x) = RecurseLocked (unsafeCoerce x)


class ( XMapC (YMapF s Y 'Unlocked t) Y 'Locked Void
      , XMapF (YMapF s Y 'Unlocked t) Y 'Locked Void ~ t
      , XMapN (YMapF s Y 'Unlocked t)
      , XMapN s
      , YMapC s Y 'Unlocked t
      ) => Recursing t s where
        rec :: (t -> s) -> RecurseL (YMapF s Y 'Unlocked t)
        rec = RecurseLocked . fix . (. (xmapn %~ unlockV)) . ((.) =<< (ymapn %~) . lockA)

instance ( XMapC (YMapF s Y 'Unlocked t) Y 'Locked Void
         , XMapF (YMapF s Y 'Unlocked t) Y 'Locked Void ~ t
         , XMapN (YMapF s Y 'Unlocked t)
         , XMapN s
         , YMapC s Y 'Unlocked t
         ) => Recursing t s where
        rec :: (t -> s) -> RecurseL (YMapF s Y 'Unlocked t)
        rec = RecurseLocked . fix . (. (xmapn %~ unlockV)) . ((.) =<< (ymapn %~) . lockA)


