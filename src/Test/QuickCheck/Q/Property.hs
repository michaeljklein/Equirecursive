{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveLift #-}

module Test.QuickCheck.Q.Property where

import Control.Monad
import Data.TyResult
import Language.Haskell.TH hiding (Type)
import Language.Haskell.TH.Syntax hiding (Type)
import Language.Haskell.TH.Utils
import Test.QuickCheck hiding (Result(..))
import Test.QuickCheck.Gen.Unsafe
import Test.QuickCheck.Property
import Data.Proxy
import GHC.TypeLits (ErrorMessage(..))
import Test.QuickCheck.Text (putLine)
import Test.QuickCheck.State
import Test.QuickCheck.Exception (tryEvaluateIO)


newtype ResultQ = ResultQ { getResultQ :: TExpQ Result }

instance TestableQ ResultQ where
  propertyQ = MkPropertyQ . return . MkPropQ . return


mapTotalResultQ :: TestableQ prop => (ResultQ -> ResultQ) -> prop -> PropertyQ
mapTotalResultQ f = mapRoseResultQ (fmap f)


-- | f here mustn't throw an exception (rose tree invariant).
mapRoseResultQ :: TestableQ prop => (RoseQ ResultQ -> RoseQ ResultQ) -> prop -> PropertyQ
mapRoseResultQ f = mapPropQ (\(MkPropQ t) -> MkPropQ (f t))


mapPropQ :: TestableQ prop => (PropQ -> PropQ) -> prop -> PropertyQ
mapPropQ f = MkPropertyQ . fmap f . unPropertyQ . propertyQ

-- | Execute the "QRose" bits of a rose tree, returning a tree
-- constructed by MkRoseQ.
reduceRoseQ :: RoseQ ResultQ -> Q (RoseQ ResultQ)
reduceRoseQ r@(MkRoseQ _ _) = return r
reduceRoseQ (QRose m) = m >>= reduceRoseQ


-- | Discard this case
discardQ :: ResultQ
discardQ = ResultQ $ TExp <$> [| (Proxy :: Proxy (TySuccess ('Text "discardQ")), discard) |]



data RoseQ a = MkRoseQ a [RoseQ a] | QRose (Q (RoseQ a))


instance Functor RoseQ where
  fmap f (MkRoseQ x rqs) = MkRoseQ (f x) (fmap f <$> rqs)

instance Applicative RoseQ where
  pure = return
  (<*>) = liftM2 ($)

instance Monad RoseQ where
  return x = MkRoseQ x []
  m >>= k = joinRoseQ (fmap k m)


joinRoseQ :: RoseQ (RoseQ a) -> RoseQ a
joinRoseQ (QRose rs) = QRose (fmap joinRoseQ rs)
joinRoseQ (MkRoseQ (QRose rm) rs) = QRose $ do r <- rm; return (joinRoseQ (MkRoseQ r rs))
joinRoseQ (MkRoseQ (MkRoseQ x ts) tts) = MkRoseQ x (map joinRoseQ tts ++ ts)


-- | Apply a function to the outermost MkRose constructor of a rose tree.
-- The function must be total!
onRoseQ :: (a -> [RoseQ a] -> RoseQ a) -> RoseQ a -> RoseQ a
onRoseQ f (MkRoseQ x rs) = f x rs
onRoseQ f (QRose m) = QRose (fmap (onRoseQ f) m)


newtype PropQ = MkPropQ { unPropQ :: RoseQ ResultQ }

newtype PropertyQ = MkPropertyQ { unPropertyQ :: Gen PropQ }

class TestableQ prop where
  propertyQ :: prop -> PropertyQ

instance TestableQ TyResult where
  propertyQ _ = undefined

instance TestableQ PropertyQ where
  propertyQ = id

instance TestableQ PropQ where
  propertyQ = MkPropertyQ . return

instance TestableQ Discard where
  propertyQ _ = propertyQ discardQ

instance TestableQ prop => TestableQ (Gen prop) where
  propertyQ mp = MkPropertyQ $ do p <- mp; unPropertyQ (againQ p)

instance (Arbitrary a, Show a, TestableQ prop) => TestableQ (a -> prop) where
  propertyQ f = forAllShrinkQ arbitrary shrink f


forAllShrinkQ :: (Show a, TestableQ prop)
             => Gen a -> (a -> [a]) -> (a -> prop) -> PropertyQ
forAllShrinkQ gen shrinker pf =
  againQ $
  MkPropertyQ $
  gen >>= \x ->
    unPropertyQ $
    shrinkingQ shrinker x $ \x' ->
      counterexampleQ (show x') (pf x')

-- | Modifies a `PropertyQ` so that it only will be tested once.
onceQ :: TestableQ prop => prop -> PropertyQ
onceQ = mapTotalResultQ (\(ResultQ texp) -> ResultQ $ TExp <$> [| (\res -> res{ abort = True }) <$> $(unTypeQ texp) |])

-- | Undoes the effect of `onceQ`.
againQ :: TestableQ prop => prop -> PropertyQ
againQ = mapTotalResultQ (\(ResultQ texp) -> ResultQ $ TExp <$> [| (\res -> res{ abort = False }) <$> $(unTypeQ texp) |])


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


counterexampleQ :: TestableQ prop => String -> prop -> PropertyQ
counterexampleQ s =
  mapTotalResultQ (\(ResultQ texp) -> ResultQ $
    TExp <$> [|
    (\res -> res{ callbacks = (PostFinalFailure Counterexample $
      \st _res -> do
        res <- tryEvaluateIO (putLine (terminal st) s)
        case res of
          Left err -> putLine (terminal st) (formatException "Exception thrown while printint test case" err)
          Right () -> return ())
          : callbacks res }) <$> $(unTypeQ texp) |])


