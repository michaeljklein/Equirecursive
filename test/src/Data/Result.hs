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

{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Result where

import Control.Lens
import Unsafe.Coerce
import Data.Default
import Data.Functor.Contravariant
import Control.Lens.Internal.Setter
import GHC.Types
import Control.Monad
import Control.Monad.Trans.Class

-- A number of these things should be remade through the Refl module

-- -- | Push a limbo function to its result
-- -- push :: (a -> b -> C (a -> b -> C V m c) m c) -> C (a -> b -> C V m c) m c
-- push :: (Result s, Res s ~ C v m a) => s -> C v m a
-- push = (^. getRes)

-- -- | Pull a limbo function from its result
-- -- pull :: C (a -> b -> C V m c) m (C V m c) -> (a -> b -> C (a -> b -> C V m c) m c)
-- pull :: C v m b -> UnRes m (C v m b) v
-- pull (Func f _) = unsafeCoerce f


-- | The result of 'a'
type family Res a :: *
type instance Res CInt = CInt
type instance Res (CInt -> b) = Res b

type instance Res (C v m a) = C v m a
type instance Res (C V m a -> b) = Res b

type instance Res (Maybe a) = Maybe a
type instance Res (Maybe a -> b) = Res b

-- | Replace the result of 'a' with 'r', wrapping with (C V m) unless
-- `()` is supplied for the first argument
type family UnRes (m :: k0) (r :: k1) (a :: k2) :: *

type instance UnRes (m :: * -> *) r CInt = C V m r
type instance UnRes (m :: * -> *) r (CInt -> a) = C V m CInt -> UnRes m r a

type instance UnRes () r CInt = r
type instance UnRes () r (CInt -> a) = CInt -> UnRes () r a


-- | `Setter`s and `Getter`s for the result of a function.
-- I.e. given a function with type @(a0 -> a1 -> .. -> r)@, get or set @r@
--
-- Note: When getting the result, the type's default is applied until the
-- result is reached. For example: @((+) :: Int -> Int -> Int) ^. getRes == 0@.
-- This can be used to recover applied arguments from partial application, if the
-- function is adapted to support it.
class Result s where
  setRes :: Setter s (UnRes () b s) (Res s) b
  getRes :: Getter s                (Res s)

instance Result CInt where
  setRes = id
  getRes = id

instance Result s => Result (CInt -> s) where
  setRes f g = pure $ \x -> untainted (setRes f (g x))
  getRes f g = ($ def) >$< getRes f (g def)




