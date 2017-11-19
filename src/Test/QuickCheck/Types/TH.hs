{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoMonoLocalBinds #-}

module Test.QuickCheck.Types.TH where

-- import Control.Monad
-- import Data.TyResult
-- import Language.Haskell.TH hiding (Type)
-- import Language.Haskell.TH.Syntax hiding (Type)
-- import Language.Haskell.TH.Utils
-- import Test.QuickCheck hiding (Result(..))
-- import Test.QuickCheck.Gen.Unsafe
-- import Test.QuickCheck.Property


-- | TODO: Explode this into a quickcheck-like structure and fill in the deets.


-- | TODO: as it stands, it's practically impossible to add types to the Arbitrary instances for TypeK.
-- Possible solutions include:
--  use TH to collect Lift instances, including things like (Proxy k) as lifting k.
--  hmmm... otherwise collect types and allow extras to be added as args to the tester (should be done anyway for PromotedT types/kinds)
--


-- We have function of type TypeK k0 -> TypeK k1 -> TypeK k2 -> ExprQ, where Expr is of type
-- Proxy ('TySuccess e) or
-- Proxy ('TyFails e)
-- 1) detect success, continue
-- 2) detect failure, shrink
-- 3) shrink, if not, return failing constraint
--






