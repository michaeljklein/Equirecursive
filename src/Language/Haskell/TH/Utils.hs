{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Haskell.TH.Utils where


import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Proxy
import Data.Maybe
import Data.Default

-- | Missing convenience synonym
type TExpQ a = Q (TExp a)


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
deriving instance Lift Type
deriving instance Lift TyLit
deriving instance Lift TySynEqn
deriving instance Lift TyVarBndr
deriving instance Lift TypeFamilyHead
deriving instance Lift (TExp a)

instance Lift a => Lift (Q a) where
  lift = join . fmap Language.Haskell.TH.Syntax.lift


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


-- |
-- @
--  `funcT` [type1, type2, type3] = type1 -> type2 -> type3
-- @
funcT :: [Type] -> Type
funcT (x:y:zs) = AppT (AppT ArrowT x) (funcT (y:zs))
funcT (x:ys)   = x

-- |
-- @
--  `proxyFT` f [x, y, z] = `Proxy` (f x y z)
-- @
proxyFT :: Type -> [Type] -> Type
proxyFT f ts = AppT (ConT ''Proxy) (foldl AppT f ts)


-- | makes @tt = "given string"@. for debugging non-IO splices.
-- Should be updated to: 1) use given name, 2) seperate for default name, 3) show automatically, 4) somehow have option to ppr
mtt :: String -> Dec
mtt s = ValD (VarP (mkName "tt")) (NormalB . LitE . StringL $ s) []

-- | Just like `foldl`:
-- @
--  `foldl` :: (b -> a -> b) -> b -> [a] -> b
-- @
-- Why? So that the type of the function is not defaulted.
-- Probably obsolete.
foldlE :: ExpQ -> ExpQ -> [ExpQ] -> ExpQ
foldlE f z []     = z
foldlE f z (x:xs) = foldlE f [| $f $z $x |] xs


-- | Make the forall from what's implicit in a list of `TyVarBndr`s and an additional `Kind`
foralls :: [TyVarBndr] -> Kind -> Type -> Type
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








