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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}

{-# LANGUAGE AllowAmbiguousTypes #-}


module Data.Lifted where


data Nat = Z | S Nat deriving (Eq, Ord, Show)


-- NOTE!! The current instance type doesn't account for the possibility that `b` is X (X :: k -> *)!
-- This needs to be added so that it won't fail if the second is more shallow than the first.


infixr 0 :$
type family (:$) (f :: ka -> kb) (x :: ka) where
  f :$ x = f x

infixr 3 :&&
type family (:&&) (a :: Bool) (b :: Bool) where
  'True :&& 'True = 'True
  a     :&& b     = 'False


