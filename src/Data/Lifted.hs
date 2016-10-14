{-# LANGUAGE AllowAmbiguousTypes #-}


module Data.Lifted where

import GHC.TypeLits( ErrorMessage(..), TypeError )
import Data.Kind


-- | Type-level `$`
infixr 0 :$
type family (:$) (f :: ka -> kb) (x :: ka) where
  f :$ x = f x

type family (+?+) (a :: Constraint) (b :: Constraint) where
  () +?+ b  = b
  a  +?+ () = a
  a  +?+ b  = (a, b)

type family (?&?) (a :: * -> Constraint) (b :: * -> Constraint) :: * -> Constraint where
  (?&?) a b = TypeError ('Text "Not implemented")

