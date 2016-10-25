{-# LANGUAGE TemplateHaskell #-}

module Data.Type.Family.Drop where

import Control.Arrow
import Data.Default
import Data.Proxy
import Data.TypeK
import Language.Haskell.TH
import Language.Haskell.TH.Utils


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


