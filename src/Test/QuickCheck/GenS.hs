{-# LANGUAGE TupleSections #-}

module Test.QuickCheck.GenS where

import Test.QuickCheck
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import System.Random

-- | Refactor/rename to remove the '
class ArbitraryS a where
  arbitraryS' :: GenS a

-- | `Gen` with `Int` state. Useful for limiting infinite
-- generation.
type GenS a = StateT Int Gen a

-- | Decrements the state before generating an
-- `arbitrary`.
arbitraryS :: Arbitrary a => GenS a
arbitraryS = decrementS >> lift arbitrary

-- | Decrement the state
decrementS :: GenS ()
decrementS = modify (+ (-1))

-- | See `oneof`. TODO: Use state to choose smaller if run out (==0)
oneofS :: [GenS a] -> GenS a
oneofS xs = lift (choose (0, length xs - 1)) >>= (xs !!)

-- | See `elements`
elementS :: [a] -> GenS a
elementS = lift . elements

-- | See `choose`, pick lower bound if run out
chooseS :: Random a => (a, a) -> GenS a
chooseS = lift . choose

-- | See `generate`, also requires initial state.
generateS :: Int -> GenS a -> IO a
generateS n = generate . fmap fst . ($ n) . runStateT

-- | See `sample'`, also requires initial state.
sampleS' :: Int -> GenS a -> IO [a]
sampleS' n = sample' . fmap fst . ($ n) . runStateT

-- | See `sample`, also requires initial state.
sampleS :: Show a => Int -> GenS a -> IO ()
sampleS n = sample . fmap fst . ($ n) . runStateT

-- | See `sublistOf`
sublistOfS :: [a] -> GenS [a]
sublistOfS = lift . sublistOf

-- | See `shuffle`
shuffleS :: [a] -> GenS [a]
shuffleS = lift . shuffle

-- | See `growingElements`
growingElementsS :: [a] -> GenS a
growingElementsS = lift . growingElements

