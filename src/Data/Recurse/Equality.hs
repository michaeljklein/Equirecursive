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
-- {-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE InstanceSigs #-}

{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Recurse.Equality where

import Data.Type.Equality ()
import Data.Kind
import GHC.Generics


data Nat = Z | S Nat deriving (Eq, Ord, Show)


NOTE!! The current instance type doesn't account for the possibility that `b` is X (X :: k -> *)!
This needs to be added so that it won't fail if the second is more shallow than the first.


-- data V1 p
-- data U1 p
-- newtype Par1 p = Par1 p
-- newtype Rec1 f p = Rec1 (f p)
-- data (f :+: g) p = L1 (f p) | R1 (g p)
-- data (f :*: g) p = (f p) :*: (g p)
-- newtype (f :.: g) p = Comp1 (f (g p))

infixr 0 :$
type family (:$) (f :: ka -> kb) (x :: ka) where
  f :$ x = f x

infixr 3 :&&
type family (:&&) (a :: Bool) (b :: Bool) where
  'True :&& 'True = 'True
  a     :&& b     = 'False

-- | Recurse, checking equality until `XY` is reached.
-- Then, pass the lower depth to the second stage.
type family Req (a :: *) (b :: *) (d :: Nat) :: Bool

-------------------------------------------------------

-- | This instance is the atomic instance type of
-- `Req`. It means we've reached finitary equality
-- for this path.
type family ReqUnit (b :: *) where
  ReqUnit () = 'True
  ReqUnit b  = 'False

type instance Req () b d = ReqUnit b

-----------------------------------------------------------------------------------

-- | This is the `Functor` instance type of `Req`.
-- It's for the simplest type of zip-equality.
type family ReqV1 (a :: *) (b :: *) (d :: Nat) where
  ReqV1 a (V1 b) d = Req a b ('S d)
  ReqV1 a     b  d = 'False

type instance Req (V1 a) b d = ReqV1 a b d

-----------------------------------------------------------------------------------

type family ReqU1 (a :: *) (b :: *) (d :: Nat) where
  ReqU1 a (U1 b) d = Req a b ('S d)
  ReqU1 a     b  d = 'False

type instance Req (U1 a) b d = ReqU1 a b d

-----------------------------------------------------------------------------------

type family ReqPar1 (a :: *) (b :: *) (d :: Nat) where
  ReqPar1 a (Par1 b) d = Req a b ('S d)
  ReqPar1 a       b  d = 'False

type instance Req (Par1 a) b d = ReqPar1 a b d

-----------------------------------------------------------------------------------

type family ReqRec1 (f :: * -> *) (a :: *) (b :: *) (d :: Nat) where
  ReqRec1 f a (f b) d = Req a b ('S d)
  ReqRec1 f a    b  d = 'False

type instance Req (Rec1 f a) b d = ReqRec1 f a b d

-----------------------------------------------------------------------------------

type family ReqGSum (f :: * -> *) (g :: * -> *) (a :: *) (b :: *) (d :: Nat) where
  ReqGSum f g a ((f :+: g) b) d = Req (f a) (f b) ('S d) :&& Req (g a) (g b) ('S d)
  ReqGSum g f a            b  d = 'False

type instance Req ((f :+: g) a) b d = ReqGSum f g a b d

-----------------------------------------------------------------------------------

type family ReqGProd (f :: * -> *) (g :: * -> *) (a :: *) (b :: *) (d :: Nat) where
  ReqGProd f g a ((f :*: g) b) d = Req (f a) (f b) ('S d) :&& Req (g a) (g b) ('S d)
  ReqGProd f g a            b  d = 'False

type instance Req ((f :*: g) a) b d = ReqGProd f g a b d

-----------------------------------------------------------------------------------


-- newtype (f :.: g) p = Comp1 (f (g p))

-- depth1 * depth2
-- depthx
-- depthy

-- x to xy depth2 times
-- y to xy depth1 times

-- traverse with equality until one xy is reached
--   that one becomes x
--   current depth is depthx
-- traverse with equality until y's xy is reached
--   decrement depthx
--   repeat until depthx == Z
-- if still all eq, x == y

-- sym       :: (  a :~:   b) ->  b :~: a
-- trans     :: (  a :~:   b) -> (b :~: c) ->   a :~:   c
-- apply     :: (f   :~: g  ) -> (a :~: b) -> f a :~: g b
-- inner     :: (f a :~: g b) ->  a :~: b
-- outer     :: (f a :~: g b) ->  f :~: g

-- (Int, XY)
-- (Int, (Int, XY))
-- (Int, (Int, (Int, XY)))


