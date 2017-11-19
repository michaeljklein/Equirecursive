{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

module Test.QuickCheck.Q.Test where

import Control.Exception (Exception(..), SomeException(..))
import Control.Monad (join)
import Data.Proxy
import Data.TyResult
import Data.Typeable
import GHC.TypeLits
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Random.TF.Gen (TFGen(..))
import Test.QuickCheck.Gen
import Test.QuickCheck.Property hiding ( Result( reason, theException, labels ) )
import Test.QuickCheck.Q.Property
import Test.QuickCheck.Random
import Test.QuickCheck.State hiding (labels)
import Test.QuickCheck.Test
import Test.QuickCheck.Text
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Test.QuickCheck.Property as P
import qualified Test.QuickCheck.State as S
import System.Random(split)

-- | Just a wrapper around `String` so we can make an `Exception` instance
newtype StringException = StringException { getStringException :: String } deriving (Show, Typeable)

instance Exception StringException

-- | So this and the next `Lift` instance use a trick: going to and from strings.
-- I doubt it's fast, but it /should/ work.
instance Lift SomeException where
  lift :: SomeException -> ExpQ
  lift (SomeException e) = let es = show e in [| toException (StringException es) |]

instance Lift TFGen where
  lift tfGen = let str = show tfGen in [| (read str ::  TFGen) |]

deriving instance Lift QCGen

-- | All the above is just so we can `lift` `Result`
deriving instance Lift Result


-- | Make a `ResultQ` from a `TyResult`.
-- Internally, it uses `success` to find whether the
-- `TyResult` is a success and `showErrorMessage` to
-- give the failing message.
resultQ :: TyResultQ -> ResultQ
resultQ tr@(TyResultQ res) = do
  let res' = unTypeQ res
  ResultQ (TExp <$> [| if successTy $res'
     then ($res', succeeded)
     else ($res', failed {reason = $(unTypeQ (showErrorMessage tr)) }) |])


-- | Make a result into a `ResultQ`, including a trivial `TySuccess`
resultQ_ :: Result -> ResultQ
resultQ_ r = ResultQ $ TExp <$> [| (Proxy :: Proxy ('TySuccess ('Text "Gave up")), r) |]


-- | Push `IO` into the `Q` monad, within a `ResultQ`
joinResultQIO :: IO ResultQ -> ResultQ
joinResultQIO = ResultQ . join . qRunIO . fmap getResultQ

-- | `join` `Q` across the newtype declaration
joinResultQ :: Q ResultQ -> ResultQ
joinResultQ = ResultQ . join . fmap getResultQ

-- data RoseQ a = MkRoseQ a [RoseQ a] | QRose (Q (RoseQ a))
-- newtype ResultQ = ResultQ { getResultQ :: TExpQ (TyResult, Result) }



-- | Tests a property, using test arguments, produces a test result, and prints the results to 'stdout'.
quickCheckWithResultQ :: TestableQ prop => Args -> prop -> IO ResultQ
quickCheckWithResultQ a p = (if chatty a then withStdioTerminal else withNullTerminal) $ \tm -> do
     rnd <- case replay a of
              Nothing      -> newQCGen
              Just (rnd,_) -> return rnd
     testQ MkState{ terminal                  = tm
                  , maxSuccessTests           = maxSuccess a
                  , maxDiscardedTests         = maxDiscardRatio a * maxSuccess a
                  , computeSize               = case replay a of
                                                  Nothing    -> computeSize'
                                                  Just (_,s) -> computeSize' `at0` s
                  , numSuccessTests           = 0
                  , numDiscardedTests         = 0
                  , numRecentlyDiscardedTests = 0
                  , S.labels                  = Map.empty
                  , collected                 = []
                  , expectedFailure           = False
                  , randomSeed                = rnd
                  , numSuccessShrinks         = 0
                  , numTryShrinks             = 0
                  , numTotTryShrinks          = 0
                  } (unGen (unPropertyQ (propertyQ p)))
  where computeSize' n d
          -- e.g. with maxSuccess = 250, maxSize = 100, goes like this:
          -- 0, 1, 2, ..., 99, 0, 1, 2, ..., 99, 0, 2, 4, ..., 98.
          | n `roundTo` maxSize a + maxSize a <= maxSuccess a ||
            n >= maxSuccess a ||
            maxSuccess a `mod` maxSize a == 0 = (n `mod` maxSize a + d `div` 10) `min` maxSize a
          | otherwise =
            ((n `mod` maxSize a) * maxSize a `div` (maxSuccess a `mod` maxSize a) + d `div` 10) `min` maxSize a
        n `roundTo` m = (n `div` m) * m
        at0 f s 0 0 = s
        at0 f s n d = f n d


testQ :: State -> (QCGen -> Int -> PropQ) -> IO ResultQ
testQ st f
  | numSuccessTests st   >= maxSuccessTests st   = doneTestingQ st f
  | numDiscardedTests st >= maxDiscardedTests st = giveUpQ st f
  | otherwise                                    = runATestQ st f



doneTestingQ :: State -> (QCGen -> Int -> PropQ) -> IO ResultQ
doneTestingQ st _f
  | not (expectedFailure st) = do
      putPart (terminal st)
        ( bold ("*** Failed!")
       ++ " Passed "
       ++ show (numSuccessTests st)
       ++ " tests (expected failure)"
        )
      finished NoExpectedFailure
  | insufficientCoverage st = do
      putPart (terminal st)
        ( bold ("*** Insufficient coverage after ")
       ++ show (numSuccessTests st)
       ++ " tests"
        )
      finished InsufficientCoverage
  | otherwise = do
      putPart (terminal st)
        ( "+++ OK, passed "
       ++ show (numSuccessTests st)
       ++ " tests"
        )
      finished Success
  where
    finished :: (Int -> [(String, Int)] -> String -> Result) -> IO ResultQ
    finished k = do
      success st
      theOutput <- terminalOutput (terminal st)
      return . resultQ_ $ (k (numSuccessTests st) (summary st) theOutput)

giveUpQ :: State -> (QCGen -> Int -> PropQ) -> IO ResultQ
giveUpQ st _f =
  do -- CALLBACK gave_up?
     putPart (terminal st)
       ( bold ("*** Gave up!")
      ++ " Passed only "
      ++ show (numSuccessTests st)
      ++ " tests"
       )
     success st
     theOutput <- terminalOutput (terminal st)
     return . resultQ_ $ GaveUp{ numTests = numSuccessTests st
                          , labels   = summary st
                          , output   = theOutput
                          }


runATestQ :: State -> (QCGen -> Int -> PropQ) -> IO ResultQ
runATestQ = undefined

q = qRunIO

prepRunATestQ :: State -> Q Int
prepRunATestQ st = do
  q $ putTemp (terminal st)
     ( "("
    ++ number (numSuccessTests st) "test"
    ++ concat [ "; " ++ show (numDiscardedTests st) ++ " discarded"
              | numDiscardedTests st > 0
              ]
    ++ ")"
     )
  let size = computeSize st (numSuccessTests st) (numRecentlyDiscardedTests st)
  return size



runATestQ' :: State -> (QCGen -> Int -> PropQ) -> Q ResultQ
runATestQ' st f =
  do -- CALLBACK before_test
     size <- prepRunATestQ st
     MkRoseQ res ts <- reduceRoseQ (unPropQ (f rnd1 size))
     res <- callbackPostTestQ st res :: Q ResultQ

     let continue break st' | undefined abort res = break st'
                            | otherwise = testQ st'
         cons x xs
           | Set.null x = xs
           | otherwise = x:xs

     case res of
       a -> (undefined :: Q ResultQ)
 where
  (rnd1,rnd2) = split (randomSeed st)


scons :: Set.Set a -> [Set.Set a] -> [Set.Set a]
scons x xs | Set.null x =   xs
           | otherwise  = x:xs


-- MkResult
-- ok :: Maybe Bool
-- expect :: Bool
-- reason :: String
-- theException :: Maybe AnException
-- abort :: Bool
-- labels :: Map String Int
-- stamp :: Set String
-- callbacks :: [Callback]

-- newtype ResultQ = ResultQ { getResultQ :: TExpQ Result }



case1 :: P.Result -> ((State -> (QCGen -> Int -> PropQ) -> IO ResultQ) -> State -> t -> Q ResultQ) -> State -> QCGen -> P.Result -> t -> Q ResultQ
case1 MkResult{ok = Just True, stamp = stamp, expect = expect} continue st rnd2 res f = -- successful test
         do continue doneTestingQ
              st{ numSuccessTests           = numSuccessTests st + 1
                , numRecentlyDiscardedTests = 0
                , randomSeed                = rnd2
                , S.labels                  = Map.unionWith max (S.labels st) (P.labels res)
                , collected                 = stamp `scons` collected st
                , expectedFailure           = expect
                } f

case2 :: P.Result -> ((State -> (QCGen -> Int -> Prop) -> IO Result) -> State -> t -> Q ResultQ) -> State -> QCGen -> P.Result -> t -> Q ResultQ
case2 MkResult{ok = Nothing, expect = expect} continue st rnd2 res f = -- discarded test
         do continue giveUp
              st{ numDiscardedTests         = numDiscardedTests st + 1
                , numRecentlyDiscardedTests = numRecentlyDiscardedTests st + 1
                , randomSeed                = rnd2
                , S.labels                  = Map.unionWith max (S.labels st) (P.labels res)
                , expectedFailure           = expect
                } f


case3 :: P.Result -> t -> State -> t1 -> P.Result -> t2 -> [Rose P.Result] -> Int -> IO Result
case3 MkResult{ok = Just False} continue st rnd2 res f ts size = -- failed test
         do if expect res
              then putPart (terminal st) (bold "*** Failed! ")
              else putPart (terminal st) "+++ OK, failed as expected. "
            (numShrinks, totFailed, lastFailed) <- foundFailure st res ts
            theOutput <- terminalOutput (terminal st)
            if not (expect res) then
              return Success{ labels = summary st,
                              numTests = numSuccessTests st+1,
                              output = theOutput }
             else
              return Failure{ usedSeed       = randomSeed st -- correct! (this will be split first)
                            , usedSize       = size
                            , numTests       = numSuccessTests st+1
                            , numShrinks     = numShrinks
                            , numShrinkTries = totFailed
                            , numShrinkFinal = lastFailed
                            , output         = theOutput
                            , reason         = P.reason res
                            , theException   = P.theException res
                            , labels         = summary st
                            }



callbackPostTestQ :: State -> ResultQ -> Q ResultQ
callbackPostTestQ st res = undefined
-- callbackPostTestQ st res = protect (exception "Exception running callback") $ do
--   sequence_ [ f st res | PostTest _ f <- callbacks res ]
--   return res

