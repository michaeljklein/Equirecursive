{-# LANGUAGE AllowAmbiguousTypes #-}


module Data.Lifted where

import GHC.TypeLits( ErrorMessage(..), TypeError )
import Data.Kind


-- | Type-level `$`
infixr 0 :$
type family (:$) (f :: ka -> kb) (x :: ka) where
  f :$ x = f x

-- | Type-level `&&`
infixr 3 :&&
type family (:&&) (a :: Bool) (b :: Bool) where
  'True :&& 'True = 'True
  a     :&& b     = 'False


type family Not (a :: Bool) :: Bool where
  Not 'True  = 'False
  Not 'False = 'True

type family (:||) (a :: Bool) (b :: Bool) :: Bool where
  'False :|| 'False = 'False
  a      :|| b      = 'True

type family (+?+) (a :: Constraint) (b :: Constraint) where
  () +?+ b  = b
  a  +?+ () = a
  a  +?+ b  = (a, b)

type family (?&?) (a :: * -> Constraint) (b :: * -> Constraint) :: * -> Constraint where
  (?&?) a b = TypeError ('Text "Not implemented")

