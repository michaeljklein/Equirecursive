{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Exists where

import Data.Kind

data Exists (c :: * -> Constraint) = forall a. c a => Exists { getExists :: a }

toExists :: c a => a -> Exists c
toExists = Exists
