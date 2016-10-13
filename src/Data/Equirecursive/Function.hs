module Data.Equirecursive.Function where

import Data.Exists
import Data.Equirecursive

-- - a $ function for RecurseL (a -> c XY), which is much more limited than an automatic push
-- - convert a function into one that allows reporting its arguments
-- - an infinite-type based printf
-- - generic infinite-type functions

data Result c a = Result { args :: [Exists c], result :: a, function :: a } -- fix function

-- (a0 -> .. -> an) -> (a0 -> .. -> Result c an)
