{-# LANGUAGE EmptyDataDecls,
             MultiParamTypeClasses,
             ScopedTypeVariables,
             FunctionalDependencies,
             OverlappingInstances,
             FlexibleInstances,
             UndecidableInstances #-}

module Main where

import Prelude hiding (print)

class Print a where
    print :: a -> IO ()

{- the following does not work:
instance Show a => Print a where
    print x = putStrLn (show x)
instance        Print a where
    print x = putStrLn "No show method"

error:
    Duplicate instance declarations:
      instance (Show a) => Print a -- Defined at /tmp/wiki.hs:7:0
      instance Print a -- Defined at /tmp/wiki.hs:9:0
-}

class Print' flag a where
    print' :: flag -> a -> IO ()

instance (ShowPred a flag, Print' flag a) => Print a where
    print = print' (undefined::flag)


-- overlapping instances are used only for ShowPred
class ShowPred a flag | a ->flag where {}

                                  -- Used only if the other
                                  -- instances don't apply
instance TypeCast flag HFalse => ShowPred a flag

instance ShowPred Int  HTrue   -- These instances should be
instance ShowPred Bool HTrue   -- the same as Show's
instance ShowPred a flag => ShowPred [a] flag
--  ...etc...


data HTrue    -- Just two
data HFalse   -- distinct types

instance Show a => Print' HTrue a where
   print' _ x = putStrLn (show x)
instance Print' HFalse a where
   print' _ x = putStrLn "No show method"

test1 = print [True,False] -- [True,False]
test2 = print id           -- No show method




-- see http://okmij.org/ftp/Haskell/typecast.html
class TypeCast   a b   | a -> b, b -> a   where typeCast   :: a -> b
class TypeCast'  t a b | t a -> b, t b -> a where typeCast'  :: t->a->b
class TypeCast'' t a b | t a -> b, t b -> a where typeCast'' :: t->a->b
instance TypeCast'  () a b => TypeCast a b where typeCast x = typeCast' () x
instance TypeCast'' t a b => TypeCast' t a b where typeCast' = typeCast''
instance TypeCast'' () a a where typeCast'' _ x  = x
