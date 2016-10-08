{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
module Data.Recurse.TH.Test where

import Data.X
import Data.Recurse.Equality
import Data.Recurse.TH

import GHC.Generics
import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadPrec (ReadPrec)
import Data.Monoid (Last, First, Product, Sum, Dual)
import Control.Applicative (ZipList)
import GHC.Conc (STM)
import Data.Complex (Complex)
import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup (Option, Max, Min)
import Data.Functor.Identity
import Control.Monad.ST
import Data.Proxy (Proxy)
import Language.Haskell.TH (tupleTypeName)

$(deqInstances ''Bool)
$(deqInstances ''Complex)
$(deqInstances ''Dual)
$(deqInstances ''Either)
$(deqInstances ''First)
$(deqInstances ''IO)
$(deqInstances ''Identity)
$(deqInstances ''Int)
$(deqInstances ''K1)
$(deqInstances ''Last)
$(deqInstances ''Max)
$(deqInstances ''Maybe)
$(deqInstances ''Min)
$(deqInstances ''NonEmpty)
$(deqInstances ''Option)
$(deqInstances ''Par1)
$(deqInstances ''Product)
$(deqInstances ''Proxy)
$(deqInstances ''ReadP)
$(deqInstances ''ReadPrec)
$(deqInstances ''ST)
$(deqInstances ''STM)
$(deqInstances ''Sum)
$(deqInstances ''U1)
$(deqInstances ''V1)
$(deqInstances ''ZipList)
$(deqInstances ''[])
$(fmap concat . mapM (deqInstances . tupleTypeName) $ 0 : [2..2]) -- [2..62]
-- $(deqInstances ''(->))


