{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}

module Test.QuickCheck.Types.TH where

import Language.Haskell.TH hiding (Type)
import Test.QuickCheck
import Control.Applicative         (Const)
import Control.Applicative         (WrappedArrow)
import Control.Applicative         (WrappedMonad)
import Control.Applicative         (ZipList)
import Control.Applicative         (Alternative(..))
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
import Data.Proxy                  (Proxy(..))
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
import GHC.Generics                (Meta(..))
import GHC.Generics                (FixityI(..))
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
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import GHC.TypeLits -- (Nat, Symbol)
import Test.QuickCheck.GenS
import Data.Type.Equality
import Data.Typeable
import Language.Haskell.TH hiding (Type)
import Language.Haskell.TH.Syntax hiding (Type)
import Data.Maybe


-- | A TemplateHaskell `Type` with kind @k@
data TypeK k = TypeK { getTypeK :: TH.Type, typeKind :: Maybe Kind } deriving (Eq, Ord, Show, Lift)

deriving instance Lift AnnTarget
deriving instance Lift Bang
deriving instance Lift Body
deriving instance Lift Callconv
deriving instance Lift Clause
deriving instance Lift Con
deriving instance Lift Dec
deriving instance Lift Exp
deriving instance Lift FamilyResultSig
deriving instance Lift Fixity
deriving instance Lift FixityDirection
deriving instance Lift Foreign
deriving instance Lift FunDep
deriving instance Lift Guard
deriving instance Lift Info
deriving instance Lift InjectivityAnn
deriving instance Lift Inline
deriving instance Lift Lit
deriving instance Lift Loc
deriving instance Lift Match
deriving instance Lift ModName
deriving instance Lift Module
deriving instance Lift ModuleInfo
deriving instance Lift Name
deriving instance Lift NameFlavour
deriving instance Lift NameSpace
deriving instance Lift OccName
deriving instance Lift Overlap
deriving instance Lift Pat
deriving instance Lift Phases
deriving instance Lift PkgName
deriving instance Lift Pragma
deriving instance Lift Range
deriving instance Lift Role
deriving instance Lift RuleBndr
deriving instance Lift RuleMatch
deriving instance Lift Safety
deriving instance Lift SourceStrictness
deriving instance Lift SourceUnpackedness
deriving instance Lift Stmt
deriving instance Lift TH.Type
deriving instance Lift TyLit
deriving instance Lift TySynEqn
deriving instance Lift TyVarBndr
deriving instance Lift TypeFamilyHead

instance Lift a => Lift (Q a) where
  lift = join . fmap Language.Haskell.TH.Syntax.lift


-- | Convenience synonym
type TypeKQ k = Q (TypeK k)


-- Even simpler:

-- type family F (a :: Nat) :: TyResult

type family R (a :: Nat) :: Bool where
  R a = 'True

instance Enum (TypeK Nat) where
  toEnum = undefined
  fromEnum = undefined
  pred = undefined

zeroTK :: TypeK Nat
zeroTK = undefined (TypeK (undefined 0) undefined)


dropFamily :: Name -> DecsQ
dropFamily name = do
  info <- reify name
  case info of
    FamilyI (OpenTypeFamilyD tyHead)     _ -> dropFamilyHead name tyHead
    FamilyI (ClosedTypeFamilyD tyHead _) _ -> dropFamilyHead name tyHead
    other -> fail $ unwords [show name, "is not a type family, it is a:", show other]

dropFamilyHead :: Name -> TypeFamilyHead -> DecsQ
dropFamilyHead name' (TypeFamilyHead name tyVars resultSig _)
  | name /= name' = fail $ unwords ["Given name is:", show name', "while reify gave:", show name]
  | otherwise    = case (mapM bndrKind tyVars, familyResultKind resultSig) of
                     (Just args, Just res) -> dropFamily' name args res
                     other                 -> fail (show other)

-- newName :: String -> Q Name

funcT :: [TH.Type] -> TH.Type
funcT (x:y:zs) = AppT (AppT ArrowT x) (funcT (y:zs))
funcT (x:ys)   = x

proxyFT :: TH.Type -> [TH.Type] -> TH.Type
proxyFT f ts = AppT (ConT ''Proxy) (foldl AppT f ts)

-- appsT :: [TH.Type] -> TH.Type
-- appsT (x:y:zs) = (AppT x (appsT zs))
-- appsT (x:ys)

-- (AppT (ConT Data.Proxy.Proxy) (AppT (AppT (ConT Data.Either.Either) (ConT GHC.Types.Bool)) (ConT GHC.Integer.Type.Integer)))

-- | makes @tt = "given string"@. for debugging non-IO splices.
mtt :: String -> Dec
mtt s = ValD (VarP (mkName "tt")) (NormalB . LitE . StringL $ s) []

instance Default (Proxy (a :: k)) where
  def = Proxy


type family F (a :: k0) (b :: k1) :: k


dropFamily' :: Name -> [Kind] -> Kind -> DecsQ
dropFamily' name args res = undefined

-- | foldl :: (b -> a -> b) -> b -> [a] -> b
-- Why? So that the type of the function is not defaulted.
foldlE :: ExpQ -> ExpQ -> [ExpQ] -> ExpQ
foldlE f z []     = z
foldlE f z (x:xs) = foldlE f [| $f $z $x |] xs


-- | Given:
-- @
--  F (a0 :: k0) (a1 :: k1) .. (an :: kn) :: k
-- @
-- it outputs:
-- @
--  it :: TypeKQ k0 -> .. TypeKQ k1 -> QExp
--  it = \(TypeK t0 _) .. (TypeK tn _) -> [| $(dropF ..) (def :: Proxy $t0) .. (def :: Proxy $tn) |]
-- @
dropF :: Name -> [TyVarBndr] -> Kind -> ExpQ
dropF fam args res = sigE dropped (return droppedType)
  where
    droppedType = funcT $ fmap (AppT (ConT ''TypeKQ)) argKinds ++ [ConT ''ExpQ]
    dropped = lamE (fmap varP xs) applied
    applied = [| foldl ((. fmap (flip TypeK def)) . appTK)|] `appE` [| return droppedF |] `appE` ts
    (argKinds, droppedF) = dropF_ fam args res
    ts = listE . fmap (\x -> appE (varE 'fmap `appE` varE 'getTypeK) (varE x)) $ xs
    xs = (mkName . ('x':). show) <$> [1..length args]

-- There's a scope error, but I know what it it: $ts gets instantiated before the lambda vars are.
-- I think the solution is to rework appTK, so that ts doesn't need to get instantiated.
-- appTK currently has type: ExpQ -> TypeKQ k -> ExpQ. Hmmmmmm.... It can be lifted to ExpQ -> Type -> ExpQ
-- But we really need it to do something like: ExpQ function -> ExpQ args -> ExpQ result.
-- Remember, we need to have: Type -> Proxy $Type
-- Oh, well that would be nice: I think I've simply forgotten the dual scopes: dropF generates a function that uses appTK, it should not use it itself! I must be tired.


-- | Cool, it works. Given:
-- @
--  F (a0 :: k0) (a1 :: k1) .. (an :: kn) :: k
-- @
-- it outputs:
-- @
--  ( [k0 .. kn],
--     def :: forall k0 .. kn k (a0 :: k0) .. (an :: kn). Proxy (a0 :: k0) -> .. -> Proxy (an :: kn) -> Proxy (F (a0 :: k0) .. (an :: kn))
--  )
-- @
-- Where any non-polymorpic arguments /should/ be handled gracefully.
dropF_ :: Name -> [TyVarBndr] -> Kind -> ([Kind], Exp)
dropF_ fam args res = (kinds', expr)
  where
    kinds'  = kinds . fmap unBndr $ args
    expr    = SigE (VarE 'def) . foralls args res . funcT $ fmap (AppT (ConT ''Proxy)) sigArgs ++ [resultT]
    resultT = proxyFT (ConT fam) sigArgs
    sigArgs = fmap (uncurry (maybe <*> SigT) . first VarT . unBndr) args


-- | Make the forall from what's implicit in a list of `TyVarBndr`s and an additional `Kind`
foralls :: [TyVarBndr] -> Kind -> TH.Type -> TH.Type
foralls tys kind = ForallT (forallKinds ++ tys) []
  where
    forallKinds = catMaybes . fmap (fmap PlainTV . polyKindName) $ ks
    ks = kind : kinds (unBndr <$> tys)


-- | Return `Just` the name if the `Kind` is polymorphic
polyKindName :: Kind -> Maybe Name
polyKindName (VarT name) = Just name
polyKindName  _          = Nothing


-- | Just the `Kind`s
kinds :: [(Name, Maybe Kind)] -> [Kind]
kinds = catMaybes . fmap snd


-- | Replace `TyVarBndr` with an easier-to-use type.
unBndr :: TyVarBndr -> (Name, Maybe Kind)
unBndr (PlainTV  name     ) = (name, def   )
unBndr (KindedTV name kind) = (name, return kind)

exprr = SigE (VarE 'def) (ForallT [PlainTV (mkName "a")] [] (AppT (AppT ArrowT (AppT (ConT ''Proxy) (VarT (mkName "a")))) (AppT (ConT ''Proxy) (AppT (ConT ''Maybe) (VarT (mkName "a"))))))

ty1 = (return $ TypeK(ConT ''Int)def)


-- | Apply (def :: Proxy (t :: k)) to something that can be lifted.
-- (Unsafe, but not too so since it's just TemplateHaskell.)
appTK :: ExpQ -> TypeKQ k -> ExpQ
appTK f tk = [| $f (def :: Proxy $(getTypeK <$> tk)) |]


-- reflexive :: TypeKQ k -> TH.ExpQ
-- reflexive x = x >>= \(TypeK t _) -> [| let result = $([| reflexive_ |])(def :: $(TH.conT ''X `TH.appT` return t)) in if success result then True else False |]

-- reflexive_ :: (X (a :: k)) -> X (Reflexive a)
-- reflexive_ _ = def

-- type family IsTrue (a :: Bool) :: TyResult where
--   IsTrue a = a === 'True

-- isTrue :: TypeKQ Bool -> TH.ExpQ
-- isTrue x = x >>= \(TypeK t _) -> [| let result = isTrue_ (def :: $(TH.conT ''X `TH.appT` return t)) in if success result then True else False |]

-- isTrue_ :: X (a :: Bool) -> X (IsTrue a)
-- isTrue_ _ = def



-- mkName ("dropped_" ++ show name) :: tos (ConT 'TypeK <$> args ++ [DecsQ])

-- mkName ("dropped_" ++ show name) x0 x1 .. xn = appTs (ConT 'Proxy : xs)

-- (foldl AppT (ConT (mkName "Data.Proxy.Proxy")) ((ConT (mkName "Test.QuickCheck.Types.Reflexive")) : [ConT ''Int]))

-- FunD Name [Clause]
-- { f p1 p2 = b where decs }

-- Clause [Pat] Body [Dec]
-- f { p1 p2 = body where decs }

-- pats = (\x -> ConP 'TypeK [VarP x, WildP]) <$> xs

-- body = NormalB Exp
-- f p { = e } where ds

-- foldl AppE (

-- λ> runQ [| \(TypeK t _) -> (+) t |]
-- LamE [ConP Test.QuickCheck.Types.TH.TypeK [VarP t_0,WildP]] (AppE (VarE GHC.Num.+) (VarE t_0))

-- foldl AppE (VarE (mkName "hi")) (map (LitE . IntegerL) [1..4])


familyResultKind :: FamilyResultSig -> Maybe Kind
familyResultKind (KindSig  kind) = Just kind
familyResultKind (NoSig        ) = Nothing
familyResultKind (TyVarSig bndr) = bndrKind bndr

bndrKind :: TyVarBndr -> Maybe Kind
bndrKind (PlainTV _) = Nothing
bndrKind (KindedTV _ kind) = Just kind

-- TypeFamilyHead Name [TyVarBndr] FamilyResultSig (Maybe InjectivityAnn)

-- testF :: TypeK Nat -> DecsQ
-- testF = do
--   name <- randName "testF_"
--   makeTest name family nat

-- testF_213 = Proxy :: Proxy (F n)
-- testF_213 = Proxy

-- checkTestF_213_123 :: Q Decs
-- checkTestF_213_123 = do
--   reify 'testF_213
--   if Type is Success
--      then empty Dec
--      else case shrinkLastType typeK_Nat_213 of
--             (x:xs) -> somehow shrink?
--             []     -> dropD 'testF_213

-- dropD :: Name -> DecQ
-- dropD = just TH of dropping name

-- Much, much simpler solution: pipe stdin to ghci!
-- It'll require some parsing, but it'll also be a lot easier than the mess of a lower-level interface.



-- TODO: Use TemplateHaskell to generate TypeK's and ArbitraryS instances.
-- Use GHC to make `dropFamily` function: TypeK k0 -> TypeK k1 -> StackGHC Result

-- typeOf (Proxy :: Proxy (CmpNat n n')) == typeOf (Proxy :: Proxy 'LT) = Just (SomeNatCmp n' n)
--                             | otherwise = Nothing

-- TODO: Need to make: Arbitrary, CoArbitrary, using GenS to prevent blowup
-- Cleanup imports
-- Migrate most of this to its own module, here we should really just have
--  the type family -> TH -> test functions.
-- Remember to make quasiquoters for Limbo once this is all done.

-- TODO: GenS elements/oneOf need to decrement state when traversing!



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

instance ArbitraryS (TypeKQ a) => LiftGenS (Proxy a) a where
  liftGenS = arbitrarySProxy

instance LiftGenS (GenS (TypeKQ a)) a where
  liftGenS = id

-- appTypeGenS :: (ArbitraryS (TypeKQ (a -> b)), ArbitraryS (TypeKQ a)) => Proxy (a -> b) -> Proxy a -> GenS (Q (TypeK b))
-- appTypeGenS p1 p2 = liftM2 (liftM2 (<*>)) (arbitrarySProxy p1) (arbitrarySProxy p2)

-- | Apply one `TypeKQ` `GenS` to another. See `LiftGenS`
appTypeGenS :: (LiftGenS f (a -> b), LiftGenS t a) => f -> t -> GenS (TypeKQ b)
appTypeGenS f x = liftM2 (liftM2 (<*>)) (liftGenS f) (liftGenS x)

infixl 1 $~>
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




