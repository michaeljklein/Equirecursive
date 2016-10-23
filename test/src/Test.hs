{-# LANGUAGE TemplateHaskell #-}

module Test where

import GHC.Generics
import Language.Haskell.TH


main = $((LitE . StringL . show . ppr) <$> reify 'unComp1)

main2 = $((LitE . StringL . show . (\(VarI _ x _) -> x)) <$> reify 'unComp1)

main3 = $((LitE . StringL . show . (\(VarI _ x _) -> x)) <$> reify 'unRec1)

main4 = $((LitE . StringL . show) <$> reifyInstances ''Functor [ConT ''Rec1 `AppT` VarT (mkName "f")])


-- This clearly brings up the context of the instance!
InstanceD Nothing [ConT ''Functor `AppT` VarT f] (ConT ''Functor `AppT` ConT ''Rec1 `AppT` VarT f) []

