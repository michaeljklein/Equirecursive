{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.QuickCheck.Types.TH where

import Language.Haskell.TH hiding (Type)
import Test.QuickCheck
import Control.Applicative         (Const)
import Control.Applicative         (WrappedArrow)
import Control.Applicative         (WrappedMonad)
import Control.Applicative         (ZipList)
import Control.Arrow               (ArrowMonad)
import Control.Arrow               (Kleisli)
import Control.Exception           (AllocationLimitExceeded)
import Control.Exception           (AssertionFailed)
import Control.Exception           (BlockedIndefinitelyOnMVar)
import Control.Exception           (BlockedIndefinitelyOnSTM)
import Control.Exception           (Deadlock)
import Control.Exception           (ErrorCall)
import Control.Exception           (Handler)
import Control.Exception           (NestedAtomically)
import Control.Exception           (NoMethodError)
import Control.Exception           (NonTermination)
import Control.Exception           (PatternMatchFail)
import Control.Exception           (RecConError)
import Control.Exception           (RecSelError)
import Control.Exception           (RecUpdError)
import Control.Exception           (SomeAsyncException)
import Control.Exception           (SomeException)
import Control.Exception           (TypeError)
import Data.Complex                (Complex)
import Data.Fixed                  (Fixed)
import Data.Functor.Compose        (Compose)
import Data.Functor.Const          (Const)
import Data.Functor.Identity       (Identity)
import Data.Functor.Product        (Product)
import Data.List.NonEmpty          (NonEmpty)
import Data.Monoid                 (All)
import Data.Monoid                 (Alt)
import Data.Monoid                 (Any)
import Data.Monoid                 (Dual)
import Data.Monoid                 (Endo)
import Data.Monoid                 (First)
import Data.Monoid                 (Last)
import Data.Monoid                 (Product)
import Data.Monoid                 (Sum)
import Data.Ord                    (Down)
import Data.Proxy                  (KProxy)
import Data.Proxy                  (Proxy)
import Data.Semigroup              (Arg)
import Data.Semigroup              (Max)
import Data.Semigroup              (Min)
import Data.Semigroup              (Option)
import Data.Semigroup              (WrappedMonoid)
import Data.Semigroup              (First)
import Data.Semigroup              (Last)
import Data.Version                (Version)
import Foreign.C.Error             (Errno)
import Foreign.C.Types             (CChar)
import Foreign.C.Types             (CClock)
import Foreign.C.Types             (CDouble)
import Foreign.C.Types             (CFloat)
import Foreign.C.Types             (CInt)
import Foreign.C.Types             (CIntMax)
import Foreign.C.Types             (CIntPtr)
import Foreign.C.Types             (CLLong)
import Foreign.C.Types             (CLong)
import Foreign.C.Types             (CPtrdiff)
import Foreign.C.Types             (CSChar)
import Foreign.C.Types             (CSUSeconds)
import Foreign.C.Types             (CShort)
import Foreign.C.Types             (CSigAtomic)
import Foreign.C.Types             (CSize)
import Foreign.C.Types             (CTime)
import Foreign.C.Types             (CUChar)
import Foreign.C.Types             (CUInt)
import Foreign.C.Types             (CUIntMax)
import Foreign.C.Types             (CUIntPtr)
import Foreign.C.Types             (CULLong)
import Foreign.C.Types             (CULong)
import Foreign.C.Types             (CUSeconds)
import Foreign.C.Types             (CUShort)
import Foreign.C.Types             (CWchar)
import GHC.Conc                    (STM)
import GHC.Conc                    (TVar)
import GHC.Conc                    (ThreadId)
import GHC.Conc.Sync               (STM)
import GHC.Conc.Sync               (TVar)
import GHC.Conc.Sync               (ThreadId)
import GHC.ExecutionStack          (Location)
import GHC.ExecutionStack          (SrcLoc)
import GHC.ExecutionStack.Internal (Location)
import GHC.ExecutionStack.Internal (SrcLoc)
import GHC.Exts                    (Char)
import GHC.Exts                    (Double)
import GHC.Exts                    (Down)
import GHC.Exts                    (Float)
import GHC.Exts                    (FunPtr)
import GHC.Exts                    (Int)
import GHC.Exts                    (Ptr)
import GHC.Exts                    (Word)
import GHC.Exts                    (Any)
import GHC.Fingerprint             (Fingerprint)
import GHC.Generics                ((:+:))
import GHC.Generics                ((:.:))
import GHC.Generics                ((:*:))
import GHC.Generics                (K1)
import GHC.Generics                (M1)
import GHC.Generics                (Par1)
import GHC.Generics                (Rec1)
import GHC.Generics                (U1)
import GHC.Generics                (Meta)
import GHC.IO.Buffer               (Buffer)
import GHC.IO.Encoding             (BufferCodec)
import GHC.IO.Encoding             (TextEncoding)
import GHC.IO.Encoding.Types       (BufferCodec)
import GHC.IO.Encoding.Types       (TextEncoding)
import GHC.IO.Handle               (HandlePosn)
import GHC.IO.Handle               (NewlineMode)
import GHC.RTS.Flags               (CCFlags)
import GHC.RTS.Flags               (ConcFlags)
import GHC.RTS.Flags               (DebugFlags)
import GHC.RTS.Flags               (GCFlags)
import GHC.RTS.Flags               (MiscFlags)
import GHC.RTS.Flags               (ProfFlags)
import GHC.RTS.Flags               (RTSFlags)
import GHC.RTS.Flags               (TickyFlags)
import GHC.RTS.Flags               (TraceFlags)
import GHC.Stack                   (SrcLoc)
import GHC.StaticPtr               (StaticPtrInfo)
import GHC.Stats                   (GCStats)
import GHC.TypeLits                (SomeNat)
import GHC.TypeLits                (SomeSymbol)
import System.Console.GetOpt       (OptDescr)
import System.IO                   (NewlineMode)
import System.Posix.Types          (CCc)
import System.Posix.Types          (CDev)
import System.Posix.Types          (CGid)
import System.Posix.Types          (CIno)
import System.Posix.Types          (CMode)
import System.Posix.Types          (CNlink)
import System.Posix.Types          (COff)
import System.Posix.Types          (CPid)
import System.Posix.Types          (CRLim)
import System.Posix.Types          (CSpeed)
import System.Posix.Types          (CSsize)
import System.Posix.Types          (CTcflag)
import System.Posix.Types          (CUid)
import System.Posix.Types          (Fd)
import Text.Printf                 (FieldFormat)
import Text.Printf                 (FormatParse)

import Data.X
import qualified Language.Haskell.TH as TH
import Data.Kind
import Data.Default
import Text.PrettyPrint
import Control.Monad

-- | A `Type` with kind @k@
data TypeK k = TypeK { getTypeK :: TH.Type, typeKind :: Kind } deriving (Eq, Ord, Show)

instance Ppr (TypeK k) where
  ppr (TypeK ty kind) = liftM2 (<>) (liftM2 (<>) (liftM2 (<>) (return "TypeK ") $ ppr ty) $ return " ") $ ppr kind

type TypeKQ k = Q (TypeK k)


-- | Apply one `TypeK` to another
kApp :: TypeK (k0 -> k1) -> TypeK k0 -> TypeK k1
kApp = undefined

-- | Make a `TypeKQ`
typeK :: ExpQ -> X k -> Name -> TypeKQ k
typeK e _ n = do
  e' <- e
  case e' of
    SigE (VarE _) (AppT _ kind) -> return $ TypeK (ConT n) kind
    other              -> fail . show $ other

-- | Make a bunch of `TypeKQ`'s
typeKs :: (ExpQ, X k, [Name]) -> [TypeKQ k]
typeKs (e, x, ns) = typeK e x <$> ns


-- | List of included types should go here.
type3type3 :: [TypeKQ ((Type -> Type -> Type) -> Type -> Type -> Type)]
type3type3 = typeKs ([| def :: X( (Type -> Type -> Type) -> Type -> Type -> Type ) |],
                        def :: X( (Type -> Type -> Type) -> Type -> Type -> Type )   ,
  [ ''WrappedArrow -- :: (* -> * -> *) -> * -> * -> *
  ])

type3type2 :: [TypeKQ ((Type -> Type -> Type) -> Type -> Type)]
type3type2 = typeKs ([| def :: X( (Type -> Type -> Type) -> Type -> Type ) |],
                        def :: X( (Type -> Type -> Type) -> Type -> Type )   ,
  [ ''ArrowMonad -- :: (* -> * -> *) -> * -> *
  ])

type2type2type2 :: [TypeKQ ((Type -> Type) -> (Type -> Type) -> Type -> Type)]
type2type2type2 = typeKs ([| def :: X( (Type -> Type) -> (Type -> Type) -> Type -> Type ) |],
                             def :: X( (Type -> Type) -> (Type -> Type) -> Type -> Type )   ,
  [ ''(:*:) -- :: (* -> *) -> (* -> *) -> * -> *
  , ''(:+:) -- :: (* -> *) -> (* -> *) -> * -> *
  , ''(:.:) -- :: (* -> *) -> (* -> *) -> * -> *
  ])

type2type3 :: [TypeKQ ((Type -> Type) -> Type -> Type -> Type)]
type2type3 = typeKs ([| def :: X( (Type -> Type) -> Type -> Type -> Type ) |],
                        def :: X( (Type -> Type) -> Type -> Type -> Type )   ,
  [ ''Kleisli -- :: (* -> *) -> * -> * -> *
  ])

type2type2 :: [TypeKQ ((Type -> Type) -> Type -> Type)]
type2type2 = typeKs ([| def :: X( (Type -> Type) -> Type -> Type ) |],
                        def :: X( (Type -> Type) -> Type -> Type )   ,
  [ ''Rec1 -- :: (* -> *) -> * -> *
  , ''WrappedMonad -- :: (* -> *) -> * -> *
  ])

wrappolywrap :: [TypeKQ ((k -> Type) -> (k1 -> k) -> k1 -> Type)]
wrappolywrap = typeKs ([| def :: X( (k -> Type) -> (k1 -> k) -> k1 -> Type ) |],
                          def :: X( (k -> Type) -> (k1 -> k) -> k1 -> Type )   ,
  [ ''Compose -- :: (k -> *) -> (k1 -> k) -> k1 -> *
  ])

meta :: [TypeKQ (Type -> Meta -> (Type -> Type) -> Type -> Type)]
meta = typeKs ([| def :: X( Type -> Meta -> (Type -> Type) -> Type -> Type ) |],
                  def :: X( Type -> Meta -> (Type -> Type) -> Type -> Type )   ,
  [ ''M1 -- :: * -> Meta -> (* -> *) -> * -> *
  ])

wrapwrap :: [TypeKQ ((k -> Type) -> k -> Type)]
wrapwrap = typeKs ([| def :: X( (k -> Type) -> k -> Type ) |],
                      def :: X( (k -> Type) -> k -> Type )   ,
  [ ''Alt -- :: (k -> *) -> k -> *
  ])

typewrap :: [TypeKQ (Type -> k -> Type)]
typewrap = typeKs ([| def :: X( Type -> k -> Type ) |],
                      def :: X( Type -> k -> Type )   ,
  [ ''Const -- :: * -> k -> *
  ])

wrap :: [TypeKQ (k -> Type)]
wrap = typeKs ([| def :: X( k -> Type ) |],
                  def :: X( k -> Type )   ,
  [ ''Proxy -- :: k -> *
  ])

anyk :: [TypeKQ k]
anyk = typeKs ([| undefined :: X( k ) |],
                  undefined :: X( k )   ,
  [ ''GHC.Exts.Any -- :: k
  ])

type4 :: [TypeKQ (Type -> Type -> Type -> Type)]
type4 = typeKs ([| def :: X( Type -> Type -> Type -> Type ) |],
                   def :: X( Type -> Type -> Type -> Type )   ,
  [ ''BufferCodec --  * -> * -> * -> *
  , ''BufferCodec --  * -> * -> * -> *
  , ''K1 --  * -> * -> * -> *
  ])

type3 :: [TypeKQ (Type -> Type -> Type)]
type3 = typeKs ([| def :: X( Type -> Type -> Type ) |],
                   def :: X( Type -> Type -> Type )   ,
  [ ''Data.Semigroup.Arg -- :: * -> * -> *
  ])

type2 :: [TypeKQ (Type -> Type)]
type2 = typeKs ([| def :: X( Type -> Type ) |],
                   def :: X( Type -> Type )   ,
  [ ''Buffer -- :: * -> *
  , ''Complex -- :: * -> *
  , ''Data.Fixed.Fixed -- :: * -> *
  , ''Data.Monoid.First -- :: * -> *
  , ''Data.Monoid.Last -- :: * -> *
  , ''Data.Monoid.Product -- :: * -> *
  , ''Data.Monoid.Product -- :: * -> *
  , ''Data.Semigroup.First -- :: * -> *
  , ''Data.Semigroup.Last -- :: * -> *
  , ''Down -- :: * -> *
  , ''Down -- :: * -> *
  , ''Dual -- :: * -> *
  , ''Endo -- :: * -> *
  , ''FunPtr -- :: * -> *
  , ''Handler -- :: * -> *
  , ''Identity -- :: * -> *
  , ''KProxy -- :: * -> *
  , ''Max -- :: * -> *
  , ''Min -- :: * -> *
  , ''NonEmpty -- :: * -> *
  , ''OptDescr -- :: * -> *
  , ''Option -- :: * -> *
  , ''Par1 -- :: * -> *
  , ''Ptr -- :: * -> *
  , ''STM -- :: * -> *
  , ''STM -- :: * -> *
  , ''Sum -- :: * -> *
  , ''TVar -- :: * -> *
  , ''TVar -- :: * -> *
  , ''U1 -- :: * -> *
  , ''WrappedMonoid -- :: * -> *
  , ''ZipList -- :: * -> *
  ])

type1 :: [TypeKQ Type]
type1 = typeKs ([| def :: X( Type ) |],
                   def :: X( Type )   ,
  [ ''All -- :: *
  , ''AllocationLimitExceeded -- :: *
  , ''AssertionFailed -- :: *
  , ''BlockedIndefinitelyOnMVar -- :: *
  , ''BlockedIndefinitelyOnSTM -- :: *
  , ''CCFlags -- :: *
  , ''CCc -- :: *
  , ''CChar -- :: *
  , ''CClock -- :: *
  , ''CDev -- :: *
  , ''CDouble -- :: *
  , ''CFloat -- :: *
  , ''CGid -- :: *
  , ''CIno -- :: *
  , ''CInt -- :: *
  , ''CIntMax -- :: *
  , ''CIntPtr -- :: *
  , ''CLLong -- :: *
  , ''CLong -- :: *
  , ''CMode -- :: *
  , ''CNlink -- :: *
  , ''COff -- :: *
  , ''CPid -- :: *
  , ''CPtrdiff -- :: *
  , ''CRLim -- :: *
  , ''CSChar -- :: *
  , ''CSUSeconds -- :: *
  , ''CShort -- :: *
  , ''CSigAtomic -- :: *
  , ''CSize -- :: *
  , ''CSpeed -- :: *
  , ''CSsize -- :: *
  , ''CTcflag -- :: *
  , ''CTime -- :: *
  , ''CUChar -- :: *
  , ''CUInt -- :: *
  , ''CUIntMax -- :: *
  , ''CUIntPtr -- :: *
  , ''CULLong -- :: *
  , ''CULong -- :: *
  , ''CUSeconds -- :: *
  , ''CUShort -- :: *
  , ''CUid -- :: *
  , ''CWchar -- :: *
  , ''Char -- :: *
  , ''ConcFlags -- :: *
  , ''Control.Exception.TypeError -- :: *
  , ''Data.Monoid.Any -- :: *
  , ''Deadlock -- :: *
  , ''DebugFlags -- :: *
  , ''Double -- :: *
  , ''Errno -- :: *
  , ''ErrorCall -- :: *
  , ''Fd -- :: *
  , ''FieldFormat -- :: *
  , ''Fingerprint -- :: *
  , ''Float -- :: *
  , ''FormatParse -- :: *
  , ''GCFlags -- :: *
  , ''GCStats -- :: *
  , ''HandlePosn -- :: *
  , ''Int -- :: *
  , ''MiscFlags -- :: *
  , ''NestedAtomically -- :: *
  , ''NewlineMode -- :: *
  , ''NewlineMode -- :: *
  , ''NoMethodError -- :: *
  , ''NonTermination -- :: *
  , ''PatternMatchFail -- :: *
  , ''ProfFlags -- :: *
  , ''RTSFlags -- :: *
  , ''RecConError -- :: *
  , ''RecSelError -- :: *
  , ''RecUpdError -- :: *
  , ''SomeAsyncException -- :: *
  , ''SomeException -- :: *
  , ''SomeNat -- :: *
  , ''SomeSymbol -- :: *
  , ''StaticPtrInfo -- :: *
  , ''TextEncoding -- :: *
  , ''TextEncoding -- :: *
  , ''ThreadId -- :: *
  , ''ThreadId -- :: *
  , ''TickyFlags -- :: *
  , ''TraceFlags -- :: *
  , ''Version -- :: *
  , ''Word -- :: *
  ])




