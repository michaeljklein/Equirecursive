{-# LANGUAGE TemplateHaskell #-}


module Test.QuickCheck.Types.Test where

import Test.QuickCheck.Types
import Language.Haskell.TH
import Test.QuickCheck.Types.TH
import GHC.Types

-- $(return . return $ dropped_Reflexive (TypeK (ConT ''Int) (Just (ConT ''GHC.Types.Type))) (mkName "ttt"))
-- ttt = undefined
