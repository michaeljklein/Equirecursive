{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoMonoLocalBinds #-}

module Test.QuickCheck.Types.TH where

import Language.Haskell.TH hiding (Type)
import Test.QuickCheck hiding (Result(..))

import Data.TypeK
import Data.TyResult
import Language.Haskell.TH.Utils
import Control.Monad
import Data.Default
import Data.Maybe
import Data.Proxy
import Control.Arrow
import GHC.TypeLits -- (Nat, Symbol)
import Language.Haskell.TH.Syntax hiding (Type)
import Test.QuickCheck.Property
import qualified Language.Haskell.TH as TH
import Test.QuickCheck.Gen.Unsafe

-- | TODO: as it stands, it's practically impossible to add types to the Arbitrary instances for TypeK.
-- Possible solutions include:
--  use TH to collect Lift instances, including things like (Proxy k) as lifting k.
--  hmmm... otherwise collect types and allow extras to be added as args to the tester (should be done anyway for PromotedT types/kinds)
--


-- | For testing
type family R (a :: Nat) :: Bool where
  R a = 'True

reifyFamily :: Name -> Q TypeFamilyHead
reifyFamily name = do
  info <- reify name
  case info of
    FamilyI (OpenTypeFamilyD   tyHead  ) _ -> return tyHead
    FamilyI (ClosedTypeFamilyD tyHead _) _ -> return tyHead
    other -> fail $ unwords [show name, "is not a type family, it is a:", show other]

reifyFamilyInfo :: Name -> Q (Name, [TyVarBndr], Kind)
reifyFamilyInfo name' = do
  (TypeFamilyHead name tyVars resultSig _) <- reifyFamily name'
  when (name /= name') $ fail $ unwords [ "reifyFamilyInfo: Given name is:"
                                       , show name'
                                       , "while reifyFamily gave:"
                                       , show name]
  case (familyResultKind resultSig) of
    (Just res) -> return (name, tyVars, res)
    other      -> fail $ unwords [ "reifyFamilyInfo:"
                                 , show name
                                 , "does not have a result kind annotation."]

-- | Drop a family into a declaration. See `dropFamily`.
dropFamilyD :: Name -> DecsQ
dropFamilyD name = do
  (_, args, res) <- reifyFamilyInfo name
  dropFamilyD' name args res

-- | Drop a family into a declaration, without `reify`.
dropFamilyD' :: Name -> [TyVarBndr] -> Kind -> DecsQ
dropFamilyD' fam args res = (:[]) <$> funD droppedName [clause [] (normalB (dropFamily' fam args res)) []]
  where
    droppedName = mkName $ "dropped_" ++ until (not . ('.' `elem`)) tail (show fam)



-- |
-- @
--  `funcT` [type1, type2, type3] = type1 -> type2 -> type3
-- @
funcT :: [TH.Type] -> TH.Type
funcT (x:y:zs) = AppT (AppT ArrowT x) (funcT (y:zs))
funcT (x:ys)   = x

-- |
-- @
--  `proxyFT` f [x, y, z] = `Proxy` (f x y z)
-- @
proxyFT :: TH.Type -> [TH.Type] -> TH.Type
proxyFT f ts = AppT (ConT ''Proxy) (foldl AppT f ts)


-- | makes @tt = "given string"@. for debugging non-IO splices.
-- Should be updated to: 1) use given name, 2) seperate for default name, 3) show automatically, 4) somehow have option to ppr
mtt :: String -> Dec
mtt s = ValD (VarP (mkName "tt")) (NormalB . LitE . StringL $ s) []

instance Default (Proxy (a :: k)) where
  def = Proxy

-- | Dummy family for testing
type family F (a :: k0) (b :: k1) :: k

-- | Just like `foldl`:
-- @
--  `foldl` :: (b -> a -> b) -> b -> [a] -> b
-- @
-- Why? So that the type of the function is not defaulted.
-- Probably obsolete.
foldlE :: ExpQ -> ExpQ -> [ExpQ] -> ExpQ
foldlE f z []     = z
foldlE f z (x:xs) = foldlE f [| $f $z $x |] xs


-- | Drop a family into a function
-- that can be used at compile time.
-- See `DropFamily'` for a version that doesn't use `reify`.
dropFamily :: Name -> ExpQ
dropFamily name = do
  (_, args, res) <- reifyFamilyInfo name
  dropFamily' name args res


-- | Given:
-- @
--  F (a0 :: k0) (a1 :: k1) .. (an :: kn) :: k
-- @
-- it outputs:
-- @
--  it :: TypeKQ k0 -> .. TypeKQ k1 -> QExp
--  it = \(TypeK t0 _) .. (TypeK tn _) -> [| $(dropF ..) (def :: Proxy $t0) .. (def :: Proxy $tn) |]
-- @
dropFamily' :: Name -> [TyVarBndr] -> Kind -> ExpQ
dropFamily' fam args res = sigE dropped (return droppedType)
  where
    droppedType = funcT $ fmap (AppT (ConT ''TypeKQ)) argKinds ++ [ConT ''ExpQ]
    dropped = lamE (fmap varP xs) applied
    applied = [| foldl ((. fmap (flip TypeK def)) . appTK)|] `appE` [| return droppedF |] `appE` ts
    (argKinds, droppedF) = dropFamily_ fam args res
    ts = listE . fmap (\x -> appE (varE 'fmap `appE` varE 'getTypeK) (varE x)) $ xs
    xs = (mkName . ('x':). show) <$> [1..length args]


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
dropFamily_ :: Name -> [TyVarBndr] -> Kind -> ([Kind], Exp)
dropFamily_ fam args res = (kinds', expr)
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


-- | Apply (def :: Proxy (t :: k)) to something that can be lifted.
-- (Unsafe, but not too so since it's just TemplateHaskell.)
appTK :: ExpQ -> TypeKQ k -> ExpQ
appTK f tk = [| $f (def :: Proxy $(getTypeK <$> tk)) |]


-- | Returns `Nothing` if input is `NoSig`
familyResultKind :: FamilyResultSig -> Maybe Kind
familyResultKind (KindSig  kind) = Just kind
familyResultKind (NoSig        ) = Nothing
familyResultKind (TyVarSig bndr) = bndrKind bndr


-- | Get the kind from a `TyVarBndr` or
-- return `Nothing` if input is `PlainTV`
bndrKind :: TyVarBndr -> Maybe Kind
bndrKind (PlainTV _) = Nothing
bndrKind (KindedTV _ kind) = Just kind


-- We have function of type TypeK k0 -> TypeK k1 -> TypeK k2 -> ExprQ, where Expr is of type
-- Proxy ('TySuccess e) or
-- Proxy ('TyFails e)
-- 1) detect success, continue
-- 2) detect failure, shrink
-- 3) shrink, if not, return failing constraint
--


newtype ResultQ = ResultQ { getResultQ :: TExpQ (TyResult, Result) }

-- | Discard this case
discardQ :: ResultQ
discardQ = undefined


mapTotalResultQ :: TestableQ prop => (ResultQ -> ResultQ) -> prop -> PropertyQ
mapTotalResultQ f = mapRoseResultQ (fmap f)

-- f here mustn't throw an exception (rose tree invariant).
mapRoseResultQ :: TestableQ prop => (RoseQ ResultQ -> RoseQ ResultQ) -> prop -> PropertyQ
mapRoseResultQ f = mapPropQ (\(MkPropQ t) -> MkPropQ (f t))

mapPropQ :: TestableQ prop => (PropQ -> PropQ) -> prop -> PropertyQ
mapPropQ f = MkPropertyQ . fmap f . unPropertyQ . propertyQ


-- | Make a `ResultQ` from a `TyResult`.
-- Internally, it uses `success` to find whether the
-- `TyResult` is a success and `showErrorMessage` to
-- give the failing message.
resultQ :: TyResultQ -> ResultQ
resultQ tr@(TyResultQ res) = do
  let res' = unTypeQ res
  ResultQ (TExp <$> [| if success $res'
     then ($res', succeeded)
     else ($res', failed {reason = $(unTypeQ (showErrorMessage tr)) }) |])



data RoseQ a = MkRoseQ a [RoseQ a] | QRose (Q (RoseQ a))


joinRoseQ :: RoseQ (RoseQ a) -> RoseQ a
joinRoseQ (QRose rs) = QRose (fmap joinRoseQ rs)
joinRoseQ (MkRoseQ (QRose rm) rs) = QRose $ do r <- rm; return (joinRoseQ (MkRoseQ r rs))
joinRoseQ (MkRoseQ (MkRoseQ x ts) tts) = MkRoseQ x (map joinRoseQ tts ++ ts)

-- | Execute the "QRose" bits of a rose tree, returning a tree
-- constructed by MkRoseQ.
reduceRoseQ :: RoseQ ResultQ -> Q (RoseQ ResultQ)
reduceRoseQ r@(MkRoseQ _ _) = return r
reduceRoseQ (QRose m) = m >>= reduceRoseQ

-- | Apply a function to the outermost MkRose constructor of a rose tree.
-- The function must be total!
onRoseQ :: (a -> [RoseQ a] -> RoseQ a) -> RoseQ a -> RoseQ a
onRoseQ f (MkRoseQ x rs) = f x rs
onRoseQ f (QRose m) = QRose (fmap (onRoseQ f) m)


instance Functor RoseQ where
  fmap f (MkRoseQ x rqs) = MkRoseQ (f x) (fmap f <$> rqs)

instance Applicative RoseQ where
  pure = return
  (<*>) = liftM2 ($)

instance Monad RoseQ where
  return x = MkRoseQ x []
  m >>= k = joinRoseQ (fmap k m)


newtype PropQ = MkPropQ { unPropQ :: RoseQ ResultQ }

newtype PropertyQ = MkPropertyQ { unPropertyQ :: Gen PropQ }

class TestableQ prop where
  propertyQ :: prop -> PropertyQ

instance TestableQ PropertyQ where
  propertyQ = id

instance TestableQ ResultQ where
  propertyQ = MkPropertyQ . return . MkPropQ . return

instance TestableQ PropQ where
  propertyQ = MkPropertyQ . return

instance TestableQ Discard where
  propertyQ _ = propertyQ discardQ

instance TestableQ prop => TestableQ (Gen prop) where
  propertyQ = undefined

instance (Arbitrary a, Show a, TestableQ prop) => TestableQ (a -> prop) where
  propertyQ f = forAllShrinkQ arbitrary shrink f


forAllShrinkQ :: (Show a, TestableQ prop)
             => Gen a -> (a -> [a]) -> (a -> prop) -> PropertyQ
forAllShrinkQ gen shrinker pf = undefined
  -- again $
  -- MkProperty $
  -- gen >>= \x ->
  --   unProperty $
  --   shrinking shrinker x $ \x' ->
  --     counterexample (show x') (pf x')


-- | Undoes the effect of 'once'.
againQ :: TestableQ prop => prop -> PropertyQ
againQ = mapTotalResultQ undefined --(\res -> res{ abort = False })


-- | Shrinks the argument to property if it fails. Shrinking is done
-- automatically for most types. This is only needed when you want to
-- override the default behavior.
shrinkingQ :: TestableQ prop =>
             (a -> [a])  -- ^ 'shrink'-like function.
          -> a           -- ^ The original argument
          -> (a -> prop) -> PropertyQ
shrinkingQ shrinker x0 pf = MkPropertyQ (fmap (MkPropQ . joinRoseQ . fmap unPropQ) (promote (props x0)))
 where
  props x =
    MkRoseQ (unPropertyQ (propertyQ (pf x))) [ props x' | x' <- shrinker x ]


-- | Adds the given string to the counterexample.
counterexampleQ :: TestableQ prop => String -> prop -> PropertyQ
counterexampleQ s = undefined
  -- callback $ PostFinalFailure Counterexample $ \st _res -> do
  --   res <- tryEvaluateIO (putLine (terminal st) s)
  --   case res of
  --     Left err ->
  --       putLine (terminal st) (formatException "Exception thrown while printing test case" err)
  --     Right () ->
  --       return ()




