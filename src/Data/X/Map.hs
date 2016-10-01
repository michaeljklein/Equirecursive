{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}

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
import Data.Rec
import Data.Unique
import Data.Version (Version)
import Data.Void
import Data.Word
import Data.X
import Data.X.Map.TH
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
import Language.Haskell.TH (Name, DecsQ)
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


-- type Setter s t a b = forall f. Settable f => (a -> f b) -> s -> f t

-- | Convenience alias
type XX k = X (X :: k -> *)

-- | Map over a data structure and expand an `XX` into some `Rec`.
-- Alternatively, compress some `Rec` into some `XX`.
-- class XMap s t a b | s -> a, t -> b, s b -> t, t a -> s where
-- class XMap s t a b | s b -> t, t a -> s where
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


$(baseInstances' (XMaps ''XMap 'xmap 'ymap 'xmapFunctor 'ymapFunctor 'xmapBifunctor 'ymapBifunctor) [''Bool])

-- instance XMap Bool Bool a b where
--   xmap _ = pure
--   ymap _ = pure

instance XMap Char Char a b where
  xmap _ = pure
  ymap _ = pure

instance XMap Double Double a b where
  xmap _ = pure
  ymap _ = pure

instance XMap Float Float a b where
  xmap _ = pure
  ymap _ = pure

instance XMap Int Int a b where
  xmap _ = pure
  ymap _ = pure

instance XMap Int8 Int8 a b where
  xmap _ = pure
  ymap _ = pure

instance XMap Int16 Int16 a b where
  xmap _ = pure
  ymap _ = pure

instance XMap Int32 Int32 a b where
  xmap _ = pure
  ymap _ = pure

instance XMap Int64 Int64 a b where
  xmap _ = pure
  ymap _ = pure

instance XMap Integer Integer a b where
  xmap _ = pure
  ymap _ = pure

instance XMap Ordering Ordering a b where
  xmap _ = pure
  ymap _ = pure

instance XMap Word Word a b where
  xmap _ = pure
  ymap _ = pure

instance XMap Word8 Word8 a b where
  xmap _ = pure
  ymap _ = pure

instance XMap Word16 Word16 a b where
  xmap _ = pure
  ymap _ = pure

instance XMap Word32 Word32 a b where
  xmap _ = pure
  ymap _ = pure

instance XMap Word64 Word64 a b where
  xmap _ = pure
  ymap _ = pure

instance XMap TypeRep TypeRep a b where
  xmap _ = pure
  ymap _ = pure

instance XMap () () a b where
  xmap _ = pure
  ymap _ = pure

instance XMap Number Number a b where
  xmap _ = pure
  ymap _ = pure

instance XMap Lexeme Lexeme a b where
  xmap _ = pure
  ymap _ = pure

instance XMap GeneralCategory GeneralCategory a b where
  xmap _ = pure
  ymap _ = pure

instance XMap Fingerprint Fingerprint a b where
  xmap _ = pure
  ymap _ = pure

instance XMap TyCon TyCon a b where
  xmap _ = pure
  ymap _ = pure

instance XMap Associativity Associativity a b where
  xmap _ = pure
  ymap _ = pure

instance XMap Any Any a b where
  xmap _ = pure
  ymap _ = pure

instance XMap All All a b where
  xmap _ = pure
  ymap _ = pure

instance XMap ArithException ArithException a b where
  xmap _ = pure
  ymap _ = pure

instance XMap ErrorCall ErrorCall a b where
  xmap _ = pure
  ymap _ = pure

instance XMap IOException IOException a b where
  xmap _ = pure
  ymap _ = pure

instance XMap MaskingState MaskingState a b where
  xmap _ = pure
  ymap _ = pure

instance XMap CUIntMax CUIntMax a b where
  xmap _ = pure
  ymap _ = pure

instance XMap CIntMax CIntMax a b where
  xmap _ = pure
  ymap _ = pure

instance XMap CUIntPtr CUIntPtr a b where
  xmap _ = pure
  ymap _ = pure

instance XMap CIntPtr CIntPtr a b where
  xmap _ = pure
  ymap _ = pure

instance XMap CSUSeconds CSUSeconds a b where
  xmap _ = pure
  ymap _ = pure

instance XMap CUSeconds CUSeconds a b where
  xmap _ = pure
  ymap _ = pure

instance XMap CTime CTime a b where
  xmap _ = pure
  ymap _ = pure

instance XMap CClock CClock a b where
  xmap _ = pure
  ymap _ = pure

instance XMap CSigAtomic CSigAtomic a b where
  xmap _ = pure
  ymap _ = pure

instance XMap CWchar CWchar a b where
  xmap _ = pure
  ymap _ = pure

instance XMap CSize CSize a b where
  xmap _ = pure
  ymap _ = pure

instance XMap CPtrdiff CPtrdiff a b where
  xmap _ = pure
  ymap _ = pure

instance XMap CDouble CDouble a b where
  xmap _ = pure
  ymap _ = pure

instance XMap CFloat CFloat a b where
  xmap _ = pure
  ymap _ = pure

instance XMap CULLong CULLong a b where
  xmap _ = pure
  ymap _ = pure

instance XMap CLLong CLLong a b where
  xmap _ = pure
  ymap _ = pure

instance XMap CULong CULong a b where
  xmap _ = pure
  ymap _ = pure

instance XMap CLong CLong a b where
  xmap _ = pure
  ymap _ = pure

instance XMap CUInt CUInt a b where
  xmap _ = pure
  ymap _ = pure

instance XMap CInt CInt a b where
  xmap _ = pure
  ymap _ = pure

instance XMap CUShort CUShort a b where
  xmap _ = pure
  ymap _ = pure

instance XMap CShort CShort a b where
  xmap _ = pure
  ymap _ = pure

instance XMap CUChar CUChar a b where
  xmap _ = pure
  ymap _ = pure

instance XMap CSChar CSChar a b where
  xmap _ = pure
  ymap _ = pure

instance XMap CChar CChar a b where
  xmap _ = pure
  ymap _ = pure

instance XMap IntPtr IntPtr a b where
  xmap _ = pure
  ymap _ = pure

instance XMap WordPtr WordPtr a b where
  xmap _ = pure
  ymap _ = pure

instance XMap BufferState BufferState a b where
  xmap _ = pure
  ymap _ = pure

instance XMap CodingProgress CodingProgress a b where
  xmap _ = pure
  ymap _ = pure

instance XMap SeekMode SeekMode a b where
  xmap _ = pure
  ymap _ = pure

instance XMap IODeviceType IODeviceType a b where
  xmap _ = pure
  ymap _ = pure

instance XMap NewlineMode NewlineMode a b where
  xmap _ = pure
  ymap _ = pure

instance XMap Newline Newline a b where
  xmap _ = pure
  ymap _ = pure

instance XMap BufferMode BufferMode a b where
  xmap _ = pure
  ymap _ = pure

instance XMap Handle Handle a b where
  xmap _ = pure
  ymap _ = pure

instance XMap IOErrorType IOErrorType a b where
  xmap _ = pure
  ymap _ = pure

instance XMap ExitCode ExitCode a b where
  xmap _ = pure
  ymap _ = pure

instance XMap ArrayException ArrayException a b where
  xmap _ = pure
  ymap _ = pure

instance XMap AsyncException AsyncException a b where
  xmap _ = pure
  ymap _ = pure

instance XMap Errno Errno a b where
  xmap _ = pure
  ymap _ = pure

instance XMap Fd Fd a b where
  xmap _ = pure
  ymap _ = pure

instance XMap CRLim CRLim a b where
  xmap _ = pure
  ymap _ = pure

instance XMap CTcflag CTcflag a b where
  xmap _ = pure
  ymap _ = pure

instance XMap CSpeed CSpeed a b where
  xmap _ = pure
  ymap _ = pure

instance XMap CCc CCc a b where
  xmap _ = pure
  ymap _ = pure

instance XMap CUid CUid a b where
  xmap _ = pure
  ymap _ = pure

instance XMap CNlink CNlink a b where
  xmap _ = pure
  ymap _ = pure

instance XMap CGid CGid a b where
  xmap _ = pure
  ymap _ = pure

instance XMap CSsize CSsize a b where
  xmap _ = pure
  ymap _ = pure

instance XMap CPid CPid a b where
  xmap _ = pure
  ymap _ = pure

instance XMap COff COff a b where
  xmap _ = pure
  ymap _ = pure

instance XMap CMode CMode a b where
  xmap _ = pure
  ymap _ = pure

instance XMap CIno CIno a b where
  xmap _ = pure
  ymap _ = pure

instance XMap CDev CDev a b where
  xmap _ = pure
  ymap _ = pure

instance XMap ThreadStatus ThreadStatus a b where
  xmap _ = pure
  ymap _ = pure

instance XMap BlockReason BlockReason a b where
  xmap _ = pure
  ymap _ = pure

instance XMap ThreadId ThreadId a b where
  xmap _ = pure
  ymap _ = pure

instance XMap IOMode IOMode a b where
  xmap _ = pure
  ymap _ = pure

instance XMap HandlePosn HandlePosn a b where
  xmap _ = pure
  ymap _ = pure

instance XMap Version Version a b where
  xmap _ = pure
  ymap _ = pure

instance XMap ConstrRep ConstrRep a b where
  xmap _ = pure
  ymap _ = pure

instance XMap DataRep DataRep a b where
  xmap _ = pure
  ymap _ = pure

instance XMap Constr Constr a b where
  xmap _ = pure
  ymap _ = pure

instance XMap Natural Natural a b where
  xmap _ = pure
  ymap _ = pure

instance XMap SomeSymbol SomeSymbol a b where
  xmap _ = pure
  ymap _ = pure

instance XMap SomeNat SomeNat a b where
  xmap _ = pure
  ymap _ = pure

instance XMap SpecConstrAnnotation SpecConstrAnnotation a b where
  xmap _ = pure
  ymap _ = pure

instance XMap Unique Unique a b where
  xmap _ = pure
  ymap _ = pure

instance XMap Void Void a b where
  xmap _ = pure
  ymap _ = pure

instance XMap R R a b where
  xmap _ = pure
  ymap _ = pure

instance XMap D D a b where
  xmap _ = pure
  ymap _ = pure

instance XMap C C a b where
  xmap _ = pure
  ymap _ = pure

instance XMap S S a b where
  xmap _ = pure
  ymap _ = pure

instance XMap Fixity Fixity a b where
  xmap _ = pure
  ymap _ = pure

instance XMap FixityI FixityI a b where
  xmap _ = pure
  ymap _ = pure

instance XMap SourceUnpackedness SourceUnpackedness a b where
  xmap _ = pure
  ymap _ = pure

instance XMap SourceStrictness SourceStrictness a b where
  xmap _ = pure
  ymap _ = pure

instance XMap DecidedStrictness DecidedStrictness a b where
  xmap _ = pure
  ymap _ = pure

instance XMap Meta Meta a b where
  xmap _ = pure
  ymap _ = pure


instance XMap s t a b => XMap [s] [t] a b where
  xmap :: Setter [s] [t] a b
  xmap = xmapFunctor
  ymap = ymapFunctor

instance XMap s t a b => XMap (IO s) (IO t) a b where
  xmap = xmapFunctor
  ymap = ymapFunctor

instance XMap s t a b => XMap (Maybe s) (Maybe t) a b where
  xmap = xmapFunctor
  ymap = ymapFunctor

instance XMap s t a b => XMap (ReadP s) (ReadP t) a b where
  xmap = xmapFunctor
  ymap = ymapFunctor

instance XMap s t a b => XMap (ReadPrec s) (ReadPrec t) a b where
  xmap = xmapFunctor
  ymap = ymapFunctor

instance XMap s t a b => XMap (Last s) (Last t) a b where
  xmap = xmapFunctor
  ymap = ymapFunctor

instance XMap s t a b => XMap (First s) (First t) a b where
  xmap = xmapFunctor
  ymap = ymapFunctor

instance XMap s t a b => XMap (STM s) (STM t) a b where
  xmap = xmapFunctor
  ymap = ymapFunctor

instance XMap s t a b => XMap (Handler s) (Handler t) a b where
  xmap = xmapFunctor
  ymap = ymapFunctor

instance XMap s t a b => XMap (ZipList s) (ZipList t) a b where
  xmap = xmapFunctor
  ymap = ymapFunctor

instance XMap s t a b => XMap (Identity s) (Identity t) a b where
  xmap = xmapFunctor
  ymap = ymapFunctor

instance XMap s t a b => XMap (ArgDescr s) (ArgDescr t) a b where
  xmap = xmapFunctor
  ymap = ymapFunctor

instance XMap s t a b => XMap (OptDescr s) (OptDescr t) a b where
  xmap = xmapFunctor
  ymap = ymapFunctor

instance XMap s t a b => XMap (ArgOrder s) (ArgOrder t) a b where
  xmap = xmapFunctor
  ymap = ymapFunctor

instance XMap s t a b => XMap (Proxy s) (Proxy t) a b where
  xmap = xmapFunctor
  ymap = ymapFunctor

instance XMap s t a b => XMap (ST st s) (ST st t) a b where
  xmap = xmapFunctor
  ymap = ymapFunctor

instance (Arrow ar, XMap s t a b) => XMap (ArrowMonad ar s) (ArrowMonad ar t) a b where
  xmap = xmapFunctor
  ymap = ymapFunctor

instance (Monad m, XMap s t a b) => XMap (WrappedMonad m s) (WrappedMonad m t) a b where
  xmap = xmapFunctor
  ymap = ymapFunctor

instance XMap s t a b => XMap (V1 s) (V1 t) a b where
  xmap = xmapFunctor
  ymap = ymapFunctor

instance XMap s t a b => XMap (U1 s) (U1 t) a b where
  xmap = xmapFunctor
  ymap = ymapFunctor

instance XMap s t a b => XMap (Par1 s) (Par1 t) a b where
  xmap = xmapFunctor
  ymap = ymapFunctor

instance (Functor f, XMap s t a b) => XMap (Rec1 f s) (Rec1 f t) a b where
  xmap = xmapFunctor
  ymap = ymapFunctor

instance XMap s t a b => XMap (URec Char s) (URec Char t) a b where
  xmap = xmapFunctor
  ymap = ymapFunctor

instance XMap s t a b => XMap (URec Double s) (URec Double t) a b where
  xmap = xmapFunctor
  ymap = ymapFunctor

instance XMap s t a b => XMap (URec Float s) (URec Float t) a b where
  xmap = xmapFunctor
  ymap = ymapFunctor

instance XMap s t a b => XMap (URec Int s) (URec Int t) a b where
  xmap = xmapFunctor
  ymap = ymapFunctor

instance XMap s t a b => XMap (URec Word s) (URec Word t) a b where
  xmap = xmapFunctor
  ymap = ymapFunctor

instance XMap s t a b => XMap (URec (Ptr ()) s) (URec (Ptr ()) t) a b where
  xmap = xmapFunctor
  ymap = ymapFunctor

instance (Functor f, XMap s t a b) => XMap (M1 i c f s) (M1 i c f t) a b where
  xmap = xmapFunctor
  ymap = ymapFunctor

instance XMap s t a b => XMap (Down s) (Down t) a b where
  xmap f (Down x) = pure . Down . untainted . xmap f $ x
  ymap f (Down x) = pure . Down . untainted . ymap f $ x


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


instance (XMap s0 t0 a b, XMap s1 t1 a b) => XMap (s0, s1) (t0, t1) a b where
  xmap = xmapBifunctor
  ymap = ymapBifunctor

instance (XMap s0 t0 a b, XMap s1 t1 a b) => XMap (Either s0 s1) (Either t0 t1) a b where
  xmap = xmapBifunctor
  ymap = ymapBifunctor

instance (XMap s0 t0 a b, XMap s1 t1 a b) => XMap (Const s0 s1) (Const t0 t1) a b where
  xmap = xmapBifunctor
  ymap = ymapBifunctor

instance (XMap s0 t0 a b, XMap s1 t1 a b) => XMap (Constant s0 s1) (Constant t0 t1) a b where
  xmap = xmapBifunctor
  ymap = ymapBifunctor


instance (XMap (f s) (f t) a b, XMap (g s) (g t) a b) => XMap ((f :+: g) s) ((f :+: g) t) a b where
  xmap = xmapWithDualMap sumMap
  ymap = ymapWithDualMap sumMap

instance (XMap (f s) (f t) a b, XMap (g s) (g t) a b) => XMap ((f :*: g) s) ((f :*: g) t) a b where
  xmap = xmapWithDualMap prodMap
  ymap = ymapWithDualMap prodMap

instance (XMap (f s) (f t) a b, XMap (g s) (g t) a b) => XMap (Product f g s) (Product f g t) a b where
  xmap = xmapWithDualMap pairMap
  ymap = ymapWithDualMap pairMap


instance (XMap s0 t0 a b, XMap s1 t1 a b) => XMap (s0 -> s1) (t0 -> t1) a b where
  xmap = xmapArrow
  ymap = ymapArrow

instance (Monad m, XMap s0 t0 a b, XMap s1 t1 a b) => XMap (Kleisli m s0 s1) (Kleisli m t0 t1) a b where
  xmap = xmapArrow
  ymap = ymapArrow

