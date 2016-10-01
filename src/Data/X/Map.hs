{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}

{-# LANGUAGE UndecidableInstances #-}


module Data.X.Map (XMap(..)) where

import Control.Applicative (ZipList, WrappedMonad, Const)
import Control.Arrow (Arrow, ArrowMonad, Kleisli, arr)
import Control.Category (Category(..))
import Control.Exception (Handler, AsyncException, ArrayException, MaskingState, IOException, ErrorCall, ArithException)
import Control.Lens.Internal.Setter (Settable(..))
import Control.Lens.Setter
import Control.Monad.ST (ST)
import Data.Bifunctor (Bifunctor(bimap))
import Data.Char (GeneralCategory)
import Data.Data (Constr, ConstrRep, DataRep, TyCon, TypeRep)
import Data.Functor.Compose
import Data.Functor.Constant
import Data.Functor.Identity ()
import Data.Functor.Product
import Data.Functor.Reverse
import Data.Int
import Data.Kind
import Data.Monoid (First, Last, All, Any, Alt(..))
import Data.Ord (Down(..))
import Data.Proxy
import Data.Unique
import Data.Version (Version)
import Data.Void
import Data.Word
import Data.X
import Foreign.C.Error (Errno)
import Foreign.C.Types
import Foreign.Ptr (Ptr, WordPtr, IntPtr)
import GHC.Conc (STM, ThreadId, ThreadStatus, BlockReason)
import GHC.Exts (SpecConstrAnnotation)
import GHC.Fingerprint.Type
import GHC.Generics
import GHC.IO.Buffer (BufferState)
import GHC.IO.Device (IODeviceType, SeekMode)
import GHC.IO.Encoding.Types (CodingProgress)
import GHC.IO.Handle (Handle, BufferMode, NewlineMode, HandlePosn)
import GHC.TypeLits (SomeNat, SomeSymbol)
import Language.Haskell.TH (conT, varT)
import Numeric.Natural
import Prelude hiding (id, (.))
import System.Console.GetOpt (ArgOrder, OptDescr, ArgDescr)
import System.Exit (ExitCode)
import System.IO (IOMode, Newline)
import System.IO.Error (IOErrorType)
import System.Posix.Types
import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadPrec (ReadPrec)
import Text.Read.Lex (Lexeme, Number)

import Data.X.Map.TH {-# SOURCE #-} (XMaps(..), baseInstances, functorInstances, bifunctorInstances, constrainedFunctorInstances)
import Data.Recurse (Locking(..), Recurse(..))
import Data.X.Map.Builders {-# SOURCE #-} ( xmapWithFunctor
                                          , xmapWithDualMap
                                          , xmapArrow
                                          , ymapWithFunctor
                                          , ymapWithDualMap
                                          , ymapArrow)


-- | Map over a data structure and expand an `XX` into some `Rec`.
-- Alternatively, compress some `Rec` into some `XX`.
--
-- Compare the functional dependencies to what we'd have if all
-- subexpressions were guaranteed to contain an `XX`:
-- @
-- class `XMap` s t a b | s -> a, t -> b, s b -> t, t a -> s where
-- @
-- However, these additonal constraints fail for cases like the following:
-- @
-- `Either` `Int` `XX`
-- @
-- The classes recurse over `Either` and we have two cases: one with @Int@
-- at the bottom and one trivial case. Since `xmap` should not modify the
-- `Int`, the `Int` instance needs to be of the form:
-- @
-- instance XMap `Int` `Int` a b
-- @
-- However, `Int` along with @a, b@ does not determine @b, a@, respectively.
class XMap s t a b | s b -> t, t a -> s where
  -- | `xmap` is the general setter, where @a@ must be `XX`
  xmap :: Setter s t a b
  -- | `ymap` is the complement to `xmap`, reversing its effects.
  ymap :: Setter t s a b


-- Ok. If I never recurse on Rec, and always replace XX -> Rec _, this will always be reversable, which will also allow for generalized pulls.
-- | Base instance, this is what's replaced
instance XMap (XX k) (Rec t) (XX k) (Rec t) where
  xmap = id
  ymap _ (Rec x) = pure $ return x >- xX


$(baseInstances'
  (XMaps ''XMap 'xmap 'ymap 'xmapFunctor 'ymapFunctor 'xmapBifunctor 'ymapBifunctor)
  [  ''Bool
   , ''Char
   , ''Double
   , ''Float
   , ''Int
   , ''Int8
   , ''Int16
   , ''Int32
   , ''Int64
   , ''Integer
   , ''Ordering
   , ''Word
   , ''Word8
   , ''Word16
   , ''Word32
   , ''Word64
   , ''TypeRep
   , ''()
   , ''Number
   , ''Lexeme
   , ''GeneralCategory
   , ''Fingerprint
   , ''TyCon
   , ''Associativity
   , ''Any
   , ''All
   , ''ArithException
   , ''ErrorCall
   , ''IOException
   , ''MaskingState
   , ''CUIntMax
   , ''CIntMax
   , ''CUIntPtr
   , ''CIntPtr
   , ''CSUSeconds
   , ''CUSeconds
   , ''CTime
   , ''CClock
   , ''CSigAtomic
   , ''CWchar
   , ''CSize
   , ''CPtrdiff
   , ''CDouble
   , ''CFloat
   , ''CULLong
   , ''CLLong
   , ''CULong
   , ''CLong
   , ''CUInt
   , ''CInt
   , ''CUShort
   , ''CShort
   , ''CUChar
   , ''CSChar
   , ''CChar
   , ''IntPtr
   , ''WordPtr
   , ''BufferState
   , ''CodingProgress
   , ''SeekMode
   , ''IODeviceType
   , ''NewlineMode
   , ''Newline
   , ''BufferMode
   , ''Handle
   , ''IOErrorType
   , ''ExitCode
   , ''ArrayException
   , ''AsyncException
   , ''Errno
   , ''Fd
   , ''CRLim
   , ''CTcflag
   , ''CSpeed
   , ''CCc
   , ''CUid
   , ''CNlink
   , ''CGid
   , ''CSsize
   , ''CPid
   , ''COff
   , ''CMode
   , ''CIno
   , ''CDev
   , ''ThreadStatus
   , ''BlockReason
   , ''ThreadId
   , ''IOMode
   , ''HandlePosn
   , ''Version
   , ''ConstrRep
   , ''DataRep
   , ''Constr
   , ''Natural
   , ''SomeSymbol
   , ''SomeNat
   , ''SpecConstrAnnotation
   , ''Unique
   , ''Void
   , ''R
   , ''D
   , ''C
   , ''S
   , ''Fixity
   , ''FixityI
   , ''SourceUnpackedness
   , ''SourceStrictness
   , ''DecidedStrictness
   , ''Meta
   ])

$(functorInstances'
  (XMaps ''XMap 'xmap 'ymap 'xmapFunctor 'ymapFunctor 'xmapBifunctor 'ymapBifunctor)
  [ conT ''[]
  , conT ''IO
  , conT ''Maybe
  , conT ''ReadP
  , conT ''ReadPrec
  , conT ''Last
  , conT ''First
  , conT ''STM
  , conT ''Handler
  , conT ''ZipList
  , conT ''Identity
  , conT ''ArgDescr
  , conT ''OptDescr
  , conT ''ArgOrder
  , conT ''Proxy
  , conT ''V1
  , conT ''U1
  , conT ''Par1
  , conT ''ST   ~>  varT "st"
  , conT ''URec ~>  conT ''Char
  , conT ''URec ~>  conT ''Double
  , conT ''URec ~>  conT ''Float
  , conT ''URec ~>  conT ''Int
  , conT ''URec ~>  conT ''Word
  , conT ''URec ~> (conT ''Ptr ~> conT ''())
  ])

$(constrainedFunctorInstances'
  (XMaps ''XMap 'xmap 'ymap 'xmapFunctor 'ymapFunctor 'xmapBifunctor 'ymapBifunctor)
  [ (conT ''Arrow   ~> varT "ar", conT ''ArrowMonad   ~> varT "ar"                        )
  , (conT ''Monad   ~> varT "m" , conT ''WrappedMonad ~> varT "m"                         )
  , (conT ''Functor ~> varT "f" , conT ''Rec1         ~> varT "f"                         )
  , (conT ''Functor ~> varT "f" , conT ''M1           ~> varT "a"  ~> varT "c" ~> varT "f")
  ])

$(bifunctorInstances'
  (XMaps ''XMap 'xmap 'ymap 'xmapFunctor 'ymapFunctor 'xmapBifunctor 'ymapBifunctor)
  [ conT ''(,)
  , conT ''Either
  , conT ''Const
  , conT ''Constant
  ])


-- | This one is special, so not worth adding TH for it
instance XMap s t a b => XMap (Down s) (Down t) a b where
  xmap f (Down x) = pure . Down . untainted . xmap f $ x
  ymap f (Down x) = pure . Down . untainted . ymap f $ x


-- | TH for these would be a pain, but I think I might attempt it
instance (Functor f, XMap (g s) (g t) a b) => XMap ((f :.: g) s) ((f :.: g) t) a b where
  xmap = xmapWithFunctor Comp1 unComp1
  ymap = ymapWithFunctor Comp1 unComp1

instance (Functor f, XMap (g s) (g t) a b) => XMap (Compose f g s) (Compose f g t) a b where
  xmap = xmapWithFunctor Compose getCompose
  ymap = ymapWithFunctor Compose getCompose

instance (Functor f, XMap s t a b) => XMap (Reverse f s) (Reverse f t) a b where
  xmap = xmapWithFunctor Reverse getReverse
  ymap = ymapWithFunctor Reverse getReverse

instance (Functor f, XMap s t a b) => XMap (Alt f s) (Alt f t) a b where
  xmap = xmapWithFunctor Alt getAlt
  ymap = ymapWithFunctor Alt getAlt


-- | (These don't really belong here. Maybe in Utils?)
-- Map over a generic sum type.
sumMap  :: (f a -> f b) -> (g a -> g b) -> (f :+: g) a -> (f :+: g) b
sumMap f _ (L1 x) = L1 (f x)
sumMap _ g (R1 x) = R1 (g x)

-- | Map over a generic product type
prodMap :: (f a -> f b) -> (g a -> g b) -> (f :*: g) a -> (f :*: g) b
prodMap f g (x :*: y) = f x :*: g y

-- | Map over the product type from "Data.Functor.Product"
pairMap :: (f a -> f b) -> (g a -> g b) -> Product f g a -> Product f g b
pairMap f g (Pair x y) = Pair (f x) (g y)

-- | TH for these is pretty straightforward
instance (XMap (f s) (f t) a b, XMap (g s) (g t) a b) => XMap ((f :+: g) s) ((f :+: g) t) a b where
  xmap = xmapWithDualMap sumMap
  ymap = ymapWithDualMap sumMap

instance (XMap (f s) (f t) a b, XMap (g s) (g t) a b) => XMap ((f :*: g) s) ((f :*: g) t) a b where
  xmap = xmapWithDualMap prodMap
  ymap = ymapWithDualMap prodMap

instance (XMap (f s) (f t) a b, XMap (g s) (g t) a b) => XMap (Product f g s) (Product f g t) a b where
  xmap = xmapWithDualMap pairMap
  ymap = ymapWithDualMap pairMap


-- | TH for these is also pretty straightforward
instance (XMap s0 t0 a b, XMap s1 t1 a b) => XMap (s0 -> s1) (t0 -> t1) a b where
  xmap = xmapArrow
  ymap = ymapArrow

instance (Monad m, XMap s0 t0 a b, XMap s1 t1 a b) => XMap (Kleisli m s0 s1) (Kleisli m t0 t1) a b where
  xmap = xmapArrow
  ymap = ymapArrow

