{-# LANGUAGE GADTs #-}
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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# LANGUAGE UndecidableInstances #-}


module Data.X.Map where

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
import Language.Haskell.TH (conT, varT, promotedT)
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

import Data.X.Map.TH (XMaps(..), baseInstances', functorInstances', bifunctorInstances', constrainedFunctorInstances', (~>))
import Data.Recurse (Locking(..), Recurse(..), lock, unlock, RecurseU, RecurseL)
import Data.Type.Equality
import Unsafe.Coerce

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

-- s b -> t
-- t a -> s
-- s -> a, unless atom: s a -> atom
-- t -> b, unless atom: t b -> atom
--                atom => s ~ t

-- --------------------- atom ~ (Int, Bool, Etc)
-- s b -> t
-- t a -> s
-- s atom -> a
-- t atom -> b
-- s a b -> atom
-- t a b -> atom

-- These are the minimum determining sets:
--     s | t | a | b | at
--   [ 1 | 1 | 0 | 0 | 1 ]
--   [ 1 | 0 | 1 | 1 | 0 ]
--   [ 1 | 0 | 0 | 1 | 1 ]
--   [ 0 | 1 | 1 | 1 | 0 ]
--   [ 0 | 1 | 1 | 0 | 1 ]


-- class (Contains a s ~ True, Contains b t ~ 'True) => XMapContains s t a b | s -> a, t -> b, s b -> t, t a -> s where
--   xmapCont :: Setter s t a b
--   ymapCont :: Setter t s a b

class XMap2 (s :: *) (t :: *) k (l :: Locking) (b :: *) | s -> k, t -> l, t -> b, s l b -> t, t k -> s where
  xmap2 :: Setter s t (XX k) (Recurse l b)
  ymap2 :: Setter t s (XX k) (Recurse l b)

instance XMap2 (XX k) (Recurse l b) k l b where
  xmap2 = undefined
  ymap2 = undefined

class XMapContains s t k lk b (l :: Bool) (r :: Bool) | s -> k, t -> lk, t -> b, s lk b -> t, t k -> s where -- | s -> k, t -> lk, t -> b, s -> l, t -> r where
  bixmap :: Setter s t (XX k) (Recurse lk b)
  biymap :: Setter t s (XX k) (Recurse lk b)


class XMapN s where
  type XMapF s (a :: k0) (l :: Locking) b :: *
  type XMapC s (a :: k0) (l :: Locking) b :: Constraint
  type YMapF s (a :: k0) (l :: Locking) b :: *
  type YMapC s (a :: k0) (l :: Locking) b :: Constraint
  xmapc :: forall (a :: k0) (l :: Locking) (b :: *). XMapC s a l b => Setter s (XMapF s a l b) (X       a  ) (Recurse l b)
  ymapc :: forall (a :: k0) (l :: Locking) (b :: *). YMapC s a l b => Setter s (YMapF s a l b) (Recurse l b) (X       a  )

-- -- | Need to decide whether the @a ~ a0@ constraint is desirable, or to shove through the inequality.
-- instance (a ~ a0) => XMapCX (a :: k -> *) (a0 :: k -> *) (l :: Locking) (b :: *) where
--   type XMapFX a (a0 :: k -> *) l b = Recurse l b
--   xmapcX = (id :: (X a -> f (Recurse l b)) -> X a -> f (Recurse l b))

-- instance XMapCX (a :: k -> *) (a0 :: *) (l :: Locking) (b :: *) where
--   type XMapFX a (a0 :: *) l b = X a
--   xmapcX = (const pure :: Applicative f => (X a0 -> f (Recurse l b)) -> X a -> f (X a))


type family (+?+) (a :: Constraint) (b :: Constraint) where
  () +?+ b  = b
  a  +?+ () = a
  a  +?+ b  = (a, b)

class XMapNX a a0 l b where
  type XMapFX a a0 l b :: *
  xmapnX :: Setter (X a) (XMapFX a a0 l b) (X a0) (Recurse l b)

instance (a ~ a0) => XMapNX a (a0 :: k -> *) l b where
  type XMapFX a a0 l b = Recurse l b
  xmapnX = id

instance XMapNX a (a0 :: *) l b where
  type XMapFX a a0 l b = X a
  xmapnX = const pure

instance XMapN (X (a :: k -> *)) where
  type XMapF (X a) a0 l b = XMapFX a a0 l b
  type XMapC (X a) a0 l b = XMapNX a a0 l b
  type YMapF (X a) a0 l b = X a
  type YMapC (X a) a0 l b = ()
  xmapc = xmapnX
  ymapc = const pure

instance XMapN (Recurse l b) where
  type XMapF (Recurse l b) a l0 b0 = Recurse l b
  type XMapC (Recurse l b) a l0 b0 = ()
  type YMapF (Recurse l b) a l0 b0 = () -- ((l0 == l) :&& (b0 == b)) :? (X a, Recurse l b)
  type YMapC (Recurse l b) a l0 b0 = ()
  xmapc = const pure
  ymapc = undefined

instance XMapN Int where
  type XMapF Int a l b = Int
  type XMapC Int a l b = ()
  type YMapF Int a l b = Int
  type YMapC Int a l b = ()
  xmapc = undefined
  ymapc = undefined

instance (XMapN s0, XMapN s1) => XMapN (s0, s1) where
  type XMapF (s0, s1) a l b = (XMapF s0 a l b, XMapF s1 a l b)
  type XMapC (s0, s1) a l b = XMapC s0 a l b +?+ XMapC s1 a l b
  type YMapF (s0, s1) a l b = (YMapF s0 a l b, YMapF s1 a l b)
  type YMapC (s0, s1) a l b = YMapC s0 a l b +?+ YMapC s1 a l b
  xmapc = undefined
  ymapc = undefined


-- f :: a -> b
-- f :: (RecurseL Void, Int) -> (RecurseU (RecurseL Void, Int), Int)
-- (. ymapc (XX -> RecurseU a)       f :: (RecurseL Void, Int) -> (XX, Int)
-- (  ymapc (XX -> RecurseL Void) .) _ :: (XX, Int) -> (XX, Int)


-- f :: (RecurseU t, Int) -> (RecurseU (RecurseU t, Int), Int)
-- (. ymapc (XX -> (RecurseU t, Int))) f :: (RecurseU t, Int) -> (RecurseU XX, Int)
-- (. ymapc (XX -> RecurseU XX)) _ :: (RecurseU t, Int) -> (XX, Int)
-- (ymapc (XX -> RecurseU t) .) _ :: (XX, Int) -> (XX, Int)

-- type family XMapF s (l :: Locking) b :: *
-- type instance XMapF (XX k) l b = Recurse l b
-- type instance XMapF (Recurse l0 b0) l b = Recurse l0 b0
-- type instance XMapF Int l b = Int
-- type instance XMapF (s0, s1) l b = (XMapF s0 l b, XMapF s1 l b)

-- type family YMapF s (l :: Locking) b :: *
-- type instance YMapF (XX k) l b =

-- type family Contains (a :: *) (b :: *) :: Bool
-- type instance Contains a Int = 'False
-- type instance Contains (XX k0) (XX k1) = 'True
-- type instance Contains a (b, c) = Contains a b :|: Contains a c

type family (a :: Bool) :|: (b :: Bool) where
  'True  :|: 'True   = 'True
  'True  :|: 'False  = 'True
  'False :|: 'True   = 'True
  'False :|: 'False  = 'False


-- Ok. If I never recurse on Rec, and always replace XX -> Rec _, this will always be reversable, which will also allow for generalized pulls.
-- | Base instance, this is what's replaced
-- instance XMap (XX k) (Recurse 'Locked t) (XX k) (Recurse 'Locked t) where
instance XMap (XX k) (Recurse 'Locked t) (XX k) (Recurse 'Locked t) where
  xmap = id
  ymap _ (RecurseLocked x) = pure $ return x >- xX


-- | This gives us an iso between the two. This means that we can lock a type and unlock it later.
--
-- However! It also means that this is extremely dangerous to use. `xmap` and `ymap` should not be exported,
-- but instead locked in with a few key argument types.
--
-- ALSO, in this form, `xmap` locks and `ymap` unlocks. This is parallel to the base instance.
-- As long as there's no way to manipulate `Locked` or `XX` from the outside, exporting `xmap`
-- should be completely safe?
instance XMap (Recurse 'Unlocked t) (Recurse 'Locked t) (Recurse 'Unlocked a) (Recurse 'Locked a) where
  xmap _ = pure . lock
  ymap _ = pure . unlock


-- instance XMap s t a b => XMap (Recurse 'Unlocked s) (Recurse 'Unlocked t) a b where
--   xmap = xmapFunctor
--   ymap = ymapFunctor


-- | Extend `xmap` to an arbitrary `Functor`
xmapFunctor :: (Functor f, XMap s t a b) => Setter (f s) (f t) a b
xmapFunctor f = pure . fmap (untainted . xmap f)

-- | Extend `ymap` to an arbitrary `Functor`
ymapFunctor :: (Functor f, XMap s t a b) => Setter (f t) (f s) a b
ymapFunctor f = pure . fmap (untainted . ymap f)


-- | Extend `xmap` to a anything wrapping a `Functor`, given a wrapper and unwrapper
xmapWithFunctor :: forall t s (f :: * -> *) s0 t0 a b.  (XMap s0 t0 a b, Functor f) => (f t0 -> t) -> (s -> f s0) -> Setter s t a b
xmapWithFunctor to' from' f = pure . to' . fmap (untainted . xmap f) . from'

-- | Extend `ymap` to a anything wrapping a `Functor`, given a wrapper and unwrapper
ymapWithFunctor :: forall t s (f :: * -> *) t0 s0 a b.  (XMap s0 t0 a b, Functor f) => (f s0 -> t) -> (s -> f t0) -> Setter s t a b
ymapWithFunctor to' from' f = pure . to' . fmap (untainted . ymap f) . from'


-- | Extend `xmap` to an arbitrary `BiFunctor`
xmapBifunctor :: (Bifunctor p, XMap s0 t0 a b, XMap s1 t1 a b) => Setter (p s0 s1) (p t0 t1) a b
xmapBifunctor f = pure . bimap (untainted . xmap f) (untainted . xmap f)

-- | Extend `ymap` to an arbitrary `BiFunctor`
ymapBifunctor :: (Bifunctor p, XMap s0 t0 a b, XMap s1 t1 a b) => Setter (p t0 t1) (p s0 s1) a b
ymapBifunctor f = pure . bimap (untainted . ymap f) (untainted . ymap f)


-- | `xmapBifunctor` generalized to any `Bifunctor`-like type
xmapWithDualMap :: forall b a (f :: * -> *) c a1 a2 c1 a3 (f1 :: * -> *) b1.
                   (XMap a3 c1 a2 b1, XMap a1 c a2 b1, Settable f1, Applicative f) =>
                   ((a1 -> c) -> (a3 -> c1) -> a -> b) -> (a2 -> f1 b1) -> a -> f b
xmapWithDualMap m f = pure . m (untainted . xmap f) (untainted . xmap f)

-- | `ymapBifunctor` generalized to any `Bifunctor`-like type
ymapWithDualMap :: forall b a (f :: * -> *) c a1 a2 c1 a3 (f1 :: * -> *) b1.
                   (XMap c1 a3 a2 b1, XMap c a1 a2 b1, Settable f1, Applicative f) =>
                   ((a1 -> c) -> (a3 -> c1) -> a -> b) -> (a2 -> f1 b1) -> a -> f b
ymapWithDualMap m f = pure . m (untainted . ymap f) (untainted . ymap f)


-- | Extend `xmap` to an arbitrary `Arrow`
xmapArrow :: (Arrow ar, XMap s0 t0 a b, XMap s1 t1 a b) => Setter (ar s0 s1) (ar t0 t1) a b
xmapArrow f ar = pure $ arr (untainted . xmap f) . ar . arr (untainted . ymap f)

-- | Extend `ymap` to an arbitrary `Arrow`
ymapArrow :: (Arrow ar, XMap s0 t0 a b, XMap s1 t1 a b) => Setter (ar t0 t1) (ar s0 s1) a b
ymapArrow f ar = pure $ arr (untainted . ymap f) . ar . arr (untainted . xmap f)

instance XMap Int Int a b where
  xmap _ = pure
  ymap _ = pure

$(baseInstances'
  (XMaps ''XMap 'xmap 'ymap 'xmapFunctor 'ymapFunctor 'xmapBifunctor 'ymapBifunctor)
  [  ''Bool
   , ''Char
   , ''Double
   , ''Float
   -- , ''Int
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

