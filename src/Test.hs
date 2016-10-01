{-# LANGUAGE TemplateHaskell #-}

module Test where

import GHC.Generics
import Language.Haskell.TH


main = $((LitE . StringL . show . ppr) <$> reify 'unComp1)

main2 = $((LitE . StringL . show . (\(VarI _ x _) -> x)) <$> reify 'unComp1)

main3 = $((LitE . StringL . show . (\(VarI _ x _) -> x)) <$> reify 'unRec1)

