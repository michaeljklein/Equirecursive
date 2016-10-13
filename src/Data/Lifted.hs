-- {-# LANGUAGE GADTs #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE PolyKinds #-}
-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE TypeInType #-}
-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE NoMonomorphismRestriction #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE InstanceSigs #-}

{-# LANGUAGE AllowAmbiguousTypes #-}


module Data.Lifted where

-- | Type-level natural numbers. Nothing special here.
-- data Nat = Z | S Nat deriving (Eq, Ord, Show)

-- | Type-level `$`
infixr 0 :$
type family (:$) (f :: ka -> kb) (x :: ka) where
  f :$ x = f x

-- | Type-level `&&`
infixr 3 :&&
type family (:&&) (a :: Bool) (b :: Bool) where
  'True :&& 'True = 'True
  a     :&& b     = 'False


