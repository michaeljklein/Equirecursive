{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.TypeK where


import Control.Applicative         (Alternative(..))
import Control.Applicative         (Const)
import Control.Applicative         (WrappedArrow)
import Control.Applicative         (WrappedMonad)
import Control.Applicative         (ZipList)
import Control.Arrow               (ArrowMonad)
import Control.Arrow               (Kleisli)
import Control.Arrow               (first)
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
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Complex                (Complex)
import Data.Default
import Data.Fixed                  (Fixed)
import Data.Functor.Compose        (Compose)
import Data.Functor.Identity       (Identity)
import Data.Kind
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
import Data.Proxy                  (Proxy(..))
import Data.Semigroup              (Arg)
import Data.Semigroup              (First)
import Data.Semigroup              (Last)
import Data.Semigroup              (Max)
import Data.Semigroup              (Min)
import Data.Semigroup              (Option)
import Data.Semigroup              (WrappedMonoid)
import Data.Type.Equality
import Data.Version                (Version)
import Data.X
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
import GHC.Exts                    (Any)
import GHC.Exts                    (Char)
import GHC.Exts                    (Double)
import GHC.Exts                    (Float)
import GHC.Exts                    (FunPtr)
import GHC.Exts                    (Int)
import GHC.Exts                    (Ptr)
import GHC.Exts                    (Word)
import GHC.Fingerprint             (Fingerprint)
import GHC.Generics                ((:*:))
import GHC.Generics                ((:+:))
import GHC.Generics                ((:.:))
import GHC.Generics                (FixityI(..))
import GHC.Generics                (K1)
import GHC.Generics                (M1)
import GHC.Generics                (Meta(..))
import GHC.Generics                (Par1)
import GHC.Generics                (Rec1)
import GHC.Generics                (U1)
import GHC.IO.Buffer               (Buffer)
import GHC.IO.Encoding             (BufferCodec)
import GHC.IO.Encoding             (TextEncoding)
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
import GHC.StaticPtr               (StaticPtrInfo)
import GHC.Stats                   (GCStats)
import GHC.TypeLits
import GHC.TypeLits                (SomeNat)
import GHC.TypeLits                (SomeSymbol)
import Language.Haskell.TH hiding (Type)
import Language.Haskell.TH.Syntax hiding (Type)
import Language.Haskell.TH.Utils ()
import System.Console.GetOpt       (OptDescr)
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
import Test.QuickCheck hiding (Result(..))
import Test.QuickCheck.GenS
import Text.PrettyPrint
import Text.Printf                 (FieldFormat)
import Text.Printf                 (FormatParse)
import qualified Language.Haskell.TH as TH



-- TODO: Use TemplateHaskell to generate TypeK's and ArbitraryS instances.
-- Use GHC to make `dropFamily` function: TypeK k0 -> TypeK k1 -> StackGHC Result

-- TODO: Need to make: Arbitrary, CoArbitrary, using GenS to prevent blowup
-- Cleanup imports
-- Migrate most of this to its own module, here we should really just have
--  the type family -> TH -> test functions.
-- Remember to make quasiquoters for Limbo once this is all done.

-- TODO: GenS elements/oneOf need to decrement state when traversing!



-- | A TemplateHaskell `Type` with kind @k@.
-- Currently the `Kind` is mostly unused, because applying one `Kind` to another is a pain.
-- The `Kind` is there as a sort of witness to the kind of the type.
-- Add type role nominal to prevent coercions, or otherwise include a witness, possibly through existentials.
-- A witness might be a lot better than `typeKind`, but, on the other hand, it's practically meaningless for kinds
-- other than star. Actually, no it isn't. It requires that `k` is inhabited. Or does it? Typeable fails
-- on polymorphism, does this? I don't think so, consider simple example of k ~ t -> Maybe t. The witness is 'Just.
-- The remaining questions are: 1) does a witness work for everything? it seems to work fine for application.
-- 2) if having a witness works, is there any remaining use for `typeKind`? Hmm.. It seems that typeKind could be eliminated
-- by having a class to go from (Proxy k) -> TH.Kind. What about Proxy t -> TH.Type?
data TypeK k = TypeK { getTypeK :: TH.Type, typeKind :: Maybe Kind } deriving (Eq, Ord, Show, Lift)

-- data TypeK k = forall (a :: k). TypeK { getTypeK :: TH.Type, typeKind :: Maybe Kind, witness :: ToStar a }


-- | Convenience synonym
type TypeKQ k = Q (TypeK k)


-- | Unimplemented
instance Enum (TypeK Nat) where
  toEnum = undefined
  fromEnum = undefined
  pred = undefined

-- | Unimplemented
zeroTK :: TypeK Nat
zeroTK = undefined (TypeK (undefined 0) undefined)


-- | There has to be a better way to defined `ppr` for this
instance Ppr (TypeK k) where
  ppr (TypeK ty (Just kind)) = liftM2 (<>) (liftM2 (<>) (liftM2 (<>) (return "TypeK ") $ ppr ty) $ return " ") $ ppr kind
  ppr (TypeK ty (_        )) = liftM2 (<>) (liftM2 (<>) (return "TypeK ") $ ppr ty) $ return " "



-- TODO: Fail better
-- | Make a `TypeKQ`
typeK :: ExpQ -> X k -> Name -> TypeKQ k
typeK e _ n = do
  e' <- e
  case e' of
    SigE (VarE _) (AppT _ kind) -> return $ TypeK (ConT n) (Just kind)
    SigE (VarE _) (ForallT tys cxt (AppT _ kind)) -> return $ TypeK (ConT n) (Just $ ForallT tys cxt kind)
    other              -> fail . show $ other

-- | Make a bunch of `TypeKQ`'s
typeKs :: (ExpQ, X k, [Name]) -> [TypeKQ k]
typeKs (e, x, ns) = typeK e x <$> ns

-- | `fmap` is likely impossible. Here to have `Applicative`
instance Functor TypeK where fmap = error "TypeK's fmap is unimplemented."


-- | `pure` is likely impossible
instance Applicative TypeK where
  pure = error "TypeK's pure is unimplemented."

  (<*>) (TypeK tyf kf) (TypeK tyx kx) = TypeK (AppT tyf tyx) Nothing -- Nothing should eventually always be Just


-- | Valid applications return `Just`, others return `Nothing`
type family MaybeAppK (a :: Type) (b :: Type) :: Maybe Type where
  MaybeAppK (k0 -> k) k0 = 'Just k
  MaybeAppK  k0       k1 = 'Nothing

-- | This class allows applying arbitrary `TypeK`'s to each other.
-- When it's an invalid application, it returns @forall t. Maybe (TypeK t)@
class (MaybeAppK a b ~ m) => TypeKApp (a :: Type) (b :: Type) (m :: Maybe Type) where
  type DropMaybe t a b m :: Type
  maybeAppK' :: forall t. TypeK a -> TypeK b -> Maybe (TypeK (DropMaybe t a b m))

  maybeAppK  :: forall t. TypeKQ a -> TypeKQ b -> MaybeT Q (TypeK (DropMaybe t a b m))

-- | Return `Just` when the application is valid
instance (MaybeAppK a b ~ 'Just k) => TypeKApp a b ('Just k) where
  type DropMaybe t a b ('Just k) = k
  maybeAppK' :: TypeK a -> TypeK b -> Maybe (TypeK k)
  maybeAppK' (TypeK tyf kf) (TypeK tyx kx) = Just (TypeK (AppT tyf tyx) ky)
    where
      ky = (join $ liftM2 simpleApp kf kx) <|> (join $ liftM2 polyApp kf kx)

  maybeAppK x y = MaybeT (liftM2 maybeAppK' x y)

-- | Return `Nothing` when the application is invalid
instance (MaybeAppK a b ~ 'Nothing) => TypeKApp a b 'Nothing where
  type DropMaybe t a b 'Nothing = t
  maybeAppK' :: forall t. TypeK a -> TypeK b -> Maybe (TypeK t)
  maybeAppK' _ _ = Nothing

  maybeAppK x y = MaybeT (liftM2 maybeAppK' x y)

-- | Split a kind of the form @k0 -> k1@
splitK :: Kind -> Maybe (Kind, Kind)
splitK (AppT (AppT ArrowT k0) k1) = Just (k0, k1)
splitK  _                         = Nothing

-- | Apply a simple (non-polymorphic) kind to another
simpleApp :: Kind -> Kind -> Maybe Kind
simpleApp x y = case first (== y) <$> splitK x of
                  Just (True, z) -> Just z
                  _              -> Nothing

-- | Apply a polymorphic kind to another
polyApp :: Kind -> Kind -> Maybe Kind
polyApp _ _ = def


-- | A simple type function plus `asTypeOf`
arbitrarySProxy :: ArbitraryS (TypeKQ a) => Proxy a -> GenS (TypeKQ a)
arbitrarySProxy _ = arbitraryS'

-- | Lift `Proxy` to a `GenS` `TypeKQ`. Idempotent.
class LiftGenS (a :: Type) (b :: Type) | a -> b where
  liftGenS :: a -> GenS (TypeKQ b)

-- | Base case
instance ArbitraryS (TypeKQ a) => LiftGenS (Proxy a) a where
  liftGenS = arbitrarySProxy

-- | Idempotence
instance LiftGenS (GenS (TypeKQ a)) a where
  liftGenS = id

-- appTypeGenS :: (ArbitraryS (TypeKQ (a -> b)), ArbitraryS (TypeKQ a)) => Proxy (a -> b) -> Proxy a -> GenS (Q (TypeK b))
-- appTypeGenS p1 p2 = liftM2 (liftM2 (<*>)) (arbitrarySProxy p1) (arbitrarySProxy p2)

-- | Apply one `TypeKQ` `GenS` to another. See `LiftGenS`
appTypeGenS :: (LiftGenS f (a -> b), LiftGenS t a) => f -> t -> GenS (TypeKQ b)
appTypeGenS f x = liftM2 (liftM2 (<*>)) (liftGenS f) (liftGenS x)

infixl 1 $~>
-- | Infix version of `appTypeGenS`
($~>) :: (LiftGenS f (a -> b), LiftGenS t a) => f -> t -> GenS (TypeKQ b)
($~>) = appTypeGenS



instance ArbitraryS (TypeKQ Bool) where
  arbitraryS' = elementS bool

instance ArbitraryS (TypeKQ k) => ArbitraryS (TypeKQ (Maybe k)) where
  arbitraryS' = oneofS
    [ genS nothing
    , genS just $~> (Proxy :: Proxy k)
    ]

nothing :: TypeKQ (Maybe k)
nothing = fin $ typeKs ([| def :: X( Maybe k ) |],
                           def :: X( Maybe k )   ,
                           [ 'Nothing ])
  where
    fin = fmap (\(TypeK (ConT x) y) -> TypeK (PromotedT x) y) . head

just :: TypeKQ (k -> Maybe k)
just = fin $ typeKs ([| def :: X( k -> Maybe k ) |],
                        def :: X( k -> Maybe k )   ,
                        [ 'Just ])
  where
    fin = fmap (\(TypeK (ConT x) y) -> TypeK (PromotedT x) y) . head

instance ArbitraryS (TypeKQ FixityI) where
  arbitraryS' = undefined

instance ArbitraryS (TypeKQ SourceUnpackedness) where
  arbitraryS' = undefined

instance ArbitraryS (TypeKQ SourceStrictness) where
  arbitraryS' = undefined

instance ArbitraryS (TypeKQ DecidedStrictness) where
  arbitraryS' = undefined

-- | `return` with type constrained for use with `$~>`
genS :: a -> GenS a
genS = return


instance ArbitraryS (TypeKQ Meta) where
  arbitraryS' = oneofS
    [ genS metaData $~> (Proxy :: Proxy Symbol) $~> (Proxy :: Proxy Symbol) $~> (Proxy :: Proxy Symbol) $~> (Proxy :: Proxy Bool)
    , genS metaCons $~> (Proxy :: Proxy Symbol) $~> (Proxy :: Proxy FixityI) $~> (Proxy :: Proxy Bool)
    , genS metaSel $~> (Proxy :: Proxy (Maybe Symbol)) $~> (Proxy :: Proxy SourceUnpackedness) $~> (Proxy :: Proxy SourceStrictness) $~> (Proxy :: Proxy DecidedStrictness)
    ]

metaData :: TypeKQ (Symbol -> Symbol -> Symbol -> Bool -> Meta)
metaData = fin $ typeKs ([| def :: X( Symbol -> Symbol -> Symbol -> Bool -> Meta ) |],
                            def :: X( Symbol -> Symbol -> Symbol -> Bool -> Meta )   ,
                            [ 'MetaData ])
  where
    fin = fmap (\(TypeK (ConT x) y) -> TypeK (PromotedT x) y) . head

metaCons :: TypeKQ (Symbol -> FixityI -> Bool -> Meta)
metaCons = fin $ typeKs ([| def :: X( Symbol -> FixityI -> Bool -> Meta ) |],
                            def :: X( Symbol -> FixityI -> Bool -> Meta )   ,
                            [ 'MetaCons ])
  where
    fin = fmap (\(TypeK (ConT x) y) -> TypeK (PromotedT x) y) . head

metaSel :: TypeKQ (Maybe Symbol -> SourceUnpackedness -> SourceStrictness -> DecidedStrictness -> Meta)
metaSel = fin $ typeKs ([| def :: X( Maybe Symbol -> SourceUnpackedness -> SourceStrictness -> DecidedStrictness -> Meta) |],
                           def :: X( Maybe Symbol -> SourceUnpackedness -> SourceStrictness -> DecidedStrictness -> Meta)   ,
                           [ 'MetaSel ])
  where
    fin = fmap (\(TypeK (ConT x) y) -> TypeK (PromotedT x) y) . head

-- | Should be uninhabited?
instance Default Meta where
  def = MetaData undefined undefined undefined False


instance ArbitraryS (TypeKQ (Type -> Meta -> (Type -> Type) -> Type -> Type)) where
  arbitraryS' = elementS meta

instance ArbitraryS (TypeKQ Nat) where
  arbitraryS' = (return . flip TypeK def . LitT . NumTyLit . getNonNegative) <$> arbitraryS

instance ArbitraryS (TypeKQ Symbol) where
  arbitraryS' = (return . flip TypeK def . LitT . StrTyLit) <$> arbitraryS

instance ArbitraryS (TypeKQ Type) where
  arbitraryS' = oneofS
    [ elementS type1
    , (Proxy :: Proxy (Type -> Type)) $~> (Proxy :: Proxy Type)
    ]


class ((Type == k) ~ ty) => WrapArbitraryS (k :: Type) (ty :: Bool) where
  wrapArbitraryS :: GenS (TypeKQ (k -> Type))
instance WrapArbitraryS Type 'True where
  wrapArbitraryS = oneofS
    [  elementS type2
    ,  elementS wrap
    ,  (Proxy :: Proxy (Type -> Type -> Type)) $~> (Proxy :: Proxy Type)
    ,  (Proxy :: Proxy (Type -> Meta -> (Type -> Type) -> Type -> Type)) $~> (Proxy :: Proxy Type) $~> (Proxy :: Proxy Meta) $~> (Proxy :: Proxy (Type -> Type))
    ,  (Proxy :: Proxy ((Type -> Type) -> Type -> Type)) $~> (Proxy :: Proxy (Type -> Type))
    ,  (Proxy :: Proxy ((Type -> Type) -> Type -> Type -> Type)) $~> (Proxy :: Proxy (Type -> Type)) $~> (Proxy :: Proxy Type)
    ,  (Proxy :: Proxy ((Type -> Type -> Type) -> Type -> Type)) $~> (Proxy :: Proxy (Type -> Type -> Type))
    ]
instance ((Type == k) ~ 'False, ArbitraryS (TypeKQ ((Type -> Type) -> (k -> Type) -> k -> Type)), ArbitraryS (TypeKQ (k -> Type))) => WrapArbitraryS k 'False where
  wrapArbitraryS = oneofS
    [  elementS wrap
    ,  (Proxy :: Proxy ((k -> Type) -> k -> Type)) $~> (Proxy :: Proxy (k -> Type))
    ]
instance WrapArbitraryS k ty => ArbitraryS (TypeKQ (k -> Type)) where
  arbitraryS' = wrapArbitraryS



class ((Type == k) ~ ty) => WrapWrapArbitraryS (k :: Type) (ty :: Bool) where
  wrapWrapArbitraryS :: GenS (TypeKQ ((k -> Type) -> k -> Type))
instance WrapWrapArbitraryS Type 'True where
  wrapWrapArbitraryS = oneofS
    [  elementS wrapwrap
    , (Proxy :: Proxy ((Type -> Type) -> (Type -> Type) -> Type -> Type)) $~> (Proxy :: Proxy (Type -> Type))
    , (Proxy :: Proxy (Type -> Meta -> (Type -> Type) -> Type -> Type)) $~> (Proxy :: Proxy Type) $~> (Proxy :: Proxy Meta)
    ]
instance ((Type == k) ~ 'False) => WrapWrapArbitraryS k 'False where
  wrapWrapArbitraryS = oneofS
    [  elementS wrapwrap
    ,  (Proxy :: Proxy ((Type -> Type) -> (k -> Type) -> k -> Type)) $~> (Proxy :: Proxy (Type -> Type))
    ]
instance WrapWrapArbitraryS k ty => ArbitraryS (TypeKQ ((k -> Type) -> k -> Type)) where
  arbitraryS' = wrapWrapArbitraryS


class ((Type == k) ~ ty1, (Type == k1) ~ ty2) => WrapPolyWrapArbitraryS (k :: Type) (k1 :: Type) (ty1 :: Bool) (ty2 :: Bool) where
  wrapPolyWrapArbitraryS :: GenS (TypeKQ ((k -> Type) -> (k1 -> k) -> k1 -> Type))
instance WrapPolyWrapArbitraryS Type Type 'True 'True where
  wrapPolyWrapArbitraryS = oneofS . fmap elementS $ [type2type2type2, wrappolywrap]

instance ((Type == k1) ~ 'False) => WrapPolyWrapArbitraryS Type k1 'True 'False where
  wrapPolyWrapArbitraryS = elementS wrappolywrap

instance ((Type == k) ~ 'False) => WrapPolyWrapArbitraryS k Type 'False 'True where
  wrapPolyWrapArbitraryS = elementS wrappolywrap

instance ((Type == k) ~ 'False, (Type == k1) ~ 'False) => WrapPolyWrapArbitraryS k k1 'False 'False where
  wrapPolyWrapArbitraryS = elementS wrappolywrap

instance WrapPolyWrapArbitraryS k k1 ty1 ty2 => ArbitraryS (TypeKQ ((k -> Type) -> (k1 -> k) -> k1 -> Type)) where
  arbitraryS' = wrapPolyWrapArbitraryS


class ((Type == k) ~ ty) => TypeWrapArbitraryS (k :: Type) (ty :: Bool) where
  typeWrapArbitraryS :: GenS (TypeKQ (Type -> k -> Type))
instance TypeWrapArbitraryS Type 'True where
  typeWrapArbitraryS = oneofS
    [ elementS type3
    , elementS typewrap
    , appTypeGenS (Proxy :: Proxy (Type -> Type -> Type -> Type)) (Proxy :: Proxy Type)
    , appTypeGenS (Proxy :: Proxy ((Type -> Type) -> Type -> Type -> Type)) (Proxy :: Proxy (Type -> Type))
    , appTypeGenS (Proxy :: Proxy ((Type -> Type -> Type) -> Type -> Type -> Type)) (Proxy :: Proxy (Type -> Type -> Type))
    ]
instance ((Type == k) ~ 'False) => TypeWrapArbitraryS k 'False where
  typeWrapArbitraryS = elementS typewrap
instance TypeWrapArbitraryS k ty => ArbitraryS (TypeKQ (Type -> k -> Type)) where
  arbitraryS' = typeWrapArbitraryS




instance ArbitraryS (TypeKQ ((Type -> Type) -> Type -> Type -> Type)) where
  arbitraryS' = elementS type2type3

instance ArbitraryS (TypeKQ ((Type -> Type -> Type) -> Type -> Type)) where
  arbitraryS' = elementS type3type2

instance ArbitraryS (TypeKQ ((Type -> Type -> Type) -> Type -> Type -> Type)) where
  arbitraryS' = elementS type3type3

instance ArbitraryS (TypeKQ (Type -> Type -> Type -> Type)) where
  arbitraryS' = elementS type4


bool :: [TypeKQ Bool]
bool = (return . flip TypeK (return . ConT $ ''Bool) . PromotedT) <$> ['True, 'False]

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






