{-# LANGUAGE TemplateHaskell #-}

module Data.Equirecursive.Class.TH where

import Control.Monad
import Language.Haskell.TH
import Data.Default
-- import Data.Equirecursive.Class


-- | These are the possibilities:
-- ConE Name
-- data T1 = C1 t1 t2; p = {C1} e1 e2
-- AppE Exp Exp
-- { f x }
typeExp :: ExpQ -> Q [Name]
typeExp expq = do
  exp <- expq
  case exp of
    ConE name -> return . return $ name
    AppE exp1 exp2 -> liftM2 (++) (typeExp $ return exp1) (typeExp $ return exp2)
    ParensE exp -> typeExp (return exp)
    UnboundVarE name -> return . return $ name
    els -> fail . show $ els
    -- LamE _ _ -> failExp
    -- LamCaseE _ -> failExp
    -- TupE _ -> failExp
    -- UnboxedTupE _ -> failExp
    -- CondE _ _ _ -> failExp
    -- MultiIfE _ -> failExp
    -- LetE _ _ -> failExp
    -- CaseE _ _ -> failExp
    -- DoE _ -> failExp
    -- CompE _ -> failExp
    -- ArithSeqE _ -> failExp
    -- ListE _ -> failExp
    -- SigE _ _ -> failExp
    -- RecConE _ _ -> failExp
    -- RecUpdE _ _ -> failExp
    -- StaticE _ -> failExp
  where
    failExp = fail "Only data type declarations can be passed to Data.Equirecursive.Class.TH.typeExp."


lookupData :: Name -> DecQ
lookupData n = do
  info <- reify n
  case info of
    ClassI _ _ -> failLookup
    ClassOpI _ _ _ -> failLookup
    TyConI dec -> return dec
    FamilyI _ _ -> failLookup
    PrimTyConI _ _ _ -> error "not implemented" --failLookup
    DataConI name ty parentName -> failLookup
    VarI _ _ _ -> failLookup
    TyVarI _ _ -> failLookup
  where
    failLookup = fail "Only data names can be passed to Data.Equirecursive.Class.TH.lookupData."

-- | Here are the possibilities
-- DataD Cxt Name [TyVarBndr] (Maybe Kind) [Con] Cxt
-- { data Cxt x => T x = A x | B (T x)
--        deriving (Z,W)}
-- NewtypeD Cxt Name [TyVarBndr] (Maybe Kind) Con Cxt
-- { newtype Cxt x => T x = A (B x)
--        deriving (Z,W Q)}
decData :: Dec -> (Cxt, Name, [TyVarBndr], Maybe Kind, [Con], Cxt)
decData dec =
   case dec of
     DataD cxt name types kind con cxt' -> (cxt, name, types, kind, con, cxt')
     NewtypeD cxt name types kind con cxt' -> (cxt, name, types, kind, [con], cxt')
     TySynD name types kind -> ([], name, types, def, [], [])
     els -> error . show $ els
     -- FunD _ _ -> failData
     -- ClassD _ _ _ _ _ -> failData
     -- InstanceD _ _ _ _ -> failData
     -- SigD _ _ -> failData
     -- ForeignD _ -> failData
     -- InfixD _ _ -> failData
     -- PragmaD _ -> failData
     -- DataFamilyD _ _ _ -> failData
     -- DataInstD _ _ _ _ _ _ -> failData
     -- NewtypeInstD _ _ _ _ _ _ -> failData
     -- TySynInstD _ _ -> failData
     -- OpenTypeFamilyD _ -> failData
     -- ClosedTypeFamilyD _ _ -> failData
     -- RoleAnnotD _ _ -> failData
     -- StandaloneDerivD _ _ -> failData
     -- DefaultSigD _ _ -> failData
   where
     failData = error "Only data declarations can be passed to Data.Equirecursive.Class.TH.decData."

dataExp :: ExpQ -> Q [(Cxt, Name, [TyVarBndr], Maybe Kind, [Con], Cxt)]
dataExp = fmap (fmap decData) . (mapM lookupData =<<) . typeExp










