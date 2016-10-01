{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Test where

data W (a :: *) where
  WCon :: W (Num n => n)

type family F (a :: *) where
  F (W Integer) = ()
  F (W n) = ()

