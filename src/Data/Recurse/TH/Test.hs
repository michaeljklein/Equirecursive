{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
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


-- $(deqInstances ''[])

$(deqInstances ''Maybe)

$(deqInstances ''IO)

$(deqInstances ''V1)

$(deqInstances ''U1)

$(deqInstances ''Par1)

$(deqInstances ''ReadP)

$(deqInstances ''ReadPrec)

$(deqInstances ''Last)

$(deqInstances ''First)

$(deqInstances ''Product)

$(deqInstances ''Sum)

$(deqInstances ''Dual)

$(deqInstances ''STM)

$(deqInstances ''ZipList)

$(deqInstances ''Complex)

$(deqInstances ''NonEmpty)

$(deqInstances ''Option)

$(deqInstances ''Max)

$(deqInstances ''Min)

$(deqInstances ''Identity)

-- $(deqInstances ''(->))

$(deqInstances ''Either)

-- $(deqInstances ''(,))

-- $(deqInstances ''(,,))

-- $(deqInstances ''(,,,))

-- $(deqInstances ''(,,,,))

-- $(deqInstances ''(,,,,,))

$(deqInstances ''ST)

$(deqInstances ''Proxy)

$(deqInstances ''K1)



-- Rec1
-- (URec Char)
-- (URec Double)
-- (URec Float)
-- (URec Int)
-- (URec Word)
-- (URec (Ptr ()))
-- ArrowMonad
-- WrappedMonad
-- Arg
-- (:+:)
-- (:*:)
-- (:.:)
-- (Alt * f)
-- (Const * m)
-- (WrappedArrow a b)
-- (M1 i c f)
-- (Product * f g)
-- (Sum * f g)
-- (Compose * * f g)



-- cc = $(((stringL . show) <$> (runMaybeT $ reifyCon ''Either)) >>= litE)
