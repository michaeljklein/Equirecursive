{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoMonoLocalBinds #-}

module Test.QuickCheck.Types.TH where

import Control.Monad
import Data.TyResult
import Language.Haskell.TH hiding (Type)
import Language.Haskell.TH.Syntax hiding (Type)
import Language.Haskell.TH.Utils
import Test.QuickCheck hiding (Result(..))
import Test.QuickCheck.Gen.Unsafe
import Test.QuickCheck.Property

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


newtype ResultQ = ResultQ { getResultQ :: TExpQ (TyResult, Result) }

-- | Discard this case
discardQ :: ResultQ
discardQ = undefined


mapTotalResultQ :: TestableQ prop => (ResultQ -> ResultQ) -> prop -> PropertyQ
mapTotalResultQ f = mapRoseResultQ (fmap f)

-- f here mustn't throw an exception (rose tree invariant).
mapRoseResultQ :: TestableQ prop => (RoseQ ResultQ -> RoseQ ResultQ) -> prop -> PropertyQ
mapRoseResultQ f = mapPropQ (\(MkPropQ t) -> MkPropQ (f t))

mapPropQ :: TestableQ prop => (PropQ -> PropQ) -> prop -> PropertyQ
mapPropQ f = MkPropertyQ . fmap f . unPropertyQ . propertyQ


-- | Make a `ResultQ` from a `TyResult`.
-- Internally, it uses `success` to find whether the
-- `TyResult` is a success and `showErrorMessage` to
-- give the failing message.
resultQ :: TyResultQ -> ResultQ
resultQ tr@(TyResultQ res) = do
  let res' = unTypeQ res
  ResultQ (TExp <$> [| if success $res'
     then ($res', succeeded)
     else ($res', failed {reason = $(unTypeQ (showErrorMessage tr)) }) |])



data RoseQ a = MkRoseQ a [RoseQ a] | QRose (Q (RoseQ a))


joinRoseQ :: RoseQ (RoseQ a) -> RoseQ a
joinRoseQ (QRose rs) = QRose (fmap joinRoseQ rs)
joinRoseQ (MkRoseQ (QRose rm) rs) = QRose $ do r <- rm; return (joinRoseQ (MkRoseQ r rs))
joinRoseQ (MkRoseQ (MkRoseQ x ts) tts) = MkRoseQ x (map joinRoseQ tts ++ ts)

-- | Execute the "QRose" bits of a rose tree, returning a tree
-- constructed by MkRoseQ.
reduceRoseQ :: RoseQ ResultQ -> Q (RoseQ ResultQ)
reduceRoseQ r@(MkRoseQ _ _) = return r
reduceRoseQ (QRose m) = m >>= reduceRoseQ

-- | Apply a function to the outermost MkRose constructor of a rose tree.
-- The function must be total!
onRoseQ :: (a -> [RoseQ a] -> RoseQ a) -> RoseQ a -> RoseQ a
onRoseQ f (MkRoseQ x rs) = f x rs
onRoseQ f (QRose m) = QRose (fmap (onRoseQ f) m)


instance Functor RoseQ where
  fmap f (MkRoseQ x rqs) = MkRoseQ (f x) (fmap f <$> rqs)

instance Applicative RoseQ where
  pure = return
  (<*>) = liftM2 ($)

instance Monad RoseQ where
  return x = MkRoseQ x []
  m >>= k = joinRoseQ (fmap k m)


newtype PropQ = MkPropQ { unPropQ :: RoseQ ResultQ }

newtype PropertyQ = MkPropertyQ { unPropertyQ :: Gen PropQ }

class TestableQ prop where
  propertyQ :: prop -> PropertyQ

instance TestableQ PropertyQ where
  propertyQ = id

instance TestableQ ResultQ where
  propertyQ = MkPropertyQ . return . MkPropQ . return

instance TestableQ PropQ where
  propertyQ = MkPropertyQ . return

instance TestableQ Discard where
  propertyQ _ = propertyQ discardQ

instance TestableQ prop => TestableQ (Gen prop) where
  propertyQ = undefined

instance (Arbitrary a, Show a, TestableQ prop) => TestableQ (a -> prop) where
  propertyQ f = forAllShrinkQ arbitrary shrink f


forAllShrinkQ :: (Show a, TestableQ prop)
             => Gen a -> (a -> [a]) -> (a -> prop) -> PropertyQ
forAllShrinkQ gen shrinker pf = undefined
  -- again $
  -- MkProperty $
  -- gen >>= \x ->
  --   unProperty $
  --   shrinking shrinker x $ \x' ->
  --     counterexample (show x') (pf x')


-- | Undoes the effect of 'once'.
againQ :: TestableQ prop => prop -> PropertyQ
againQ = mapTotalResultQ undefined --(\res -> res{ abort = False })


-- | Shrinks the argument to property if it fails. Shrinking is done
-- automatically for most types. This is only needed when you want to
-- override the default behavior.
shrinkingQ :: TestableQ prop =>
             (a -> [a])  -- ^ 'shrink'-like function.
          -> a           -- ^ The original argument
          -> (a -> prop) -> PropertyQ
shrinkingQ shrinker x0 pf = MkPropertyQ (fmap (MkPropQ . joinRoseQ . fmap unPropQ) (promote (props x0)))
 where
  props x =
    MkRoseQ (unPropertyQ (propertyQ (pf x))) [ props x' | x' <- shrinker x ]


-- | Adds the given string to the counterexample.
counterexampleQ :: TestableQ prop => String -> prop -> PropertyQ
counterexampleQ s = undefined
  -- callback $ PostFinalFailure Counterexample $ \st _res -> do
  --   res <- tryEvaluateIO (putLine (terminal st) s)
  --   case res of
  --     Left err ->
  --       putLine (terminal st) (formatException "Exception thrown while printing test case" err)
  --     Right () ->
  --       return ()




