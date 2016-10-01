{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Data.X.Map.TH where

import Language.Haskell.TH
-- import Data.X.Map (XMap(..), xmapFunctor, ymapFunctor, xmapBifunctor, ymapBifunctor)
import Data.String
import Control.Monad
import Data.Bifunctor (Bifunctor())
import Data.Foldable (toList)
import Data.X
import Data.Rec

instance IsString Name where
  fromString = mkName

instance IsString TypeQ where
  fromString = varT . mkName

data XMaps = XMaps { cls :: Name, xmp :: Name, ymp :: Name, xmpFunctor :: Name, ympFunctor :: Name, xmpBifunctor :: Name, ympBifunctor :: Name }

-- XMaps ''XMap 'xmap 'ymap 'xmapFunctor 'ymapFunctor 'xmapBifunctor 'ymapBifunctor

-- | Make zero or more instances
instances :: Traversable t => (a -> DecQ) -> t a -> DecsQ
instances f = fmap toList . sequence . fmap f

-- | Convenience function
baseInstances' :: XMaps -> [Name] -> DecsQ
baseInstances' x = instances (baseInstance x)

-- | Convenience function
functorInstances' :: XMaps -> [TypeQ] -> DecsQ
functorInstances' x = instances (functorInstance x)

-- | Convenience function
constrainedFunctorInstances' :: XMaps -> [(TypeQ, TypeQ)] -> DecsQ
constrainedFunctorInstances' x = instances (constrainedFunctorInstance x)

-- | Convenience function
bifunctorInstances' :: XMaps -> [TypeQ] -> DecsQ
bifunctorInstances' x = instances (bifunctorInstance x)



-- | Infix `appT`
(~>) :: TypeQ -> TypeQ -> TypeQ
(~>) = appT

-- | @\(s, t, a, b) -> `XMap` s t a b@
xMap :: XMaps -> (TypeQ, TypeQ, TypeQ, TypeQ) -> TypeQ
xMap x (s, t, a, b) = conT (cls x) ~> s ~> t ~> a ~> b

-- | @`XMap` s t a b@
xMap_ :: XMaps -> TypeQ
xMap_ x = xMap x ("s", "t", "a", "b")

-- | @\n -> `XMap` sn tn a b@
xMapN :: XMaps -> Int -> TypeQ
xMapN x n = xMap x ( varT . mkName . ('s':) . show $ n
                   , varT . mkName . ('t':) . show $ n
                   , "a"
                   , "b")





-- | Make an `XMap` instance for a non-parameterized type
--
-- @
-- baseInstance ''Int
-- instance Data.X.Map.XMap GHC.Types.Int GHC.Types.Int a b
--     where Data.X.Map.xmap _ = GHC.Base.pure
--           Data.X.Map.ymap _ = GHC.Base.pure
-- @
--
baseInstance :: XMaps -> Name -> DecQ
baseInstance x n = instanceD (cxt []) (xMap x (conT n, conT n, conT ''XX ~> "a", conT ''Rec ~> "b")) [baseXmap x, baseYmap x]

-- | @`xmap` _ = `pure`@
baseXmap :: XMaps -> DecQ
baseXmap x = funD (xmp x) [clause [wildP] (normalB . varE $ 'pure) []]

-- | @`ymap` _ = `pure`@
baseYmap :: XMaps -> DecQ
baseYmap x = funD (ymp x) [clause [wildP] (normalB . varE $ 'pure) []]



-- | @\f -> `XMap` (f s) (f t) a b@
xMapFunctor :: XMaps -> TypeQ -> TypeQ
xMapFunctor x f = xMap x (f ~> "s", f ~> "t", "a", "b")

-- | Make an `XMap` instance for a single-parameter `Functor` type:
--
-- @
-- functorInstance ''IO
-- instance Data.X.Map.XMap s t a b => Data.X.Map.XMap (GHC.Types.IO s) (GHC.Types.IO t) a b
--     where Data.X.Map.xmap = Data.X.Map.xmapFunctor
--           Data.X.Map.ymap = Data.X.Map.ymapFunctor
-- @
--
functorInstance :: XMaps -> TypeQ -> DecQ
functorInstance x n = do
  n' <- n
  isFunctor <- isInstance ''Functor [n']
  unless isFunctor . fail $ "Data.X.Map.functorInstance must be supplied with the name of a Functor"
  unsafeFunctorInstance [] x n

-- | `functorInstance` without checking that the type has a `Functor` instance.
unsafeFunctorInstance :: [TypeQ] -> XMaps -> TypeQ -> DecQ
unsafeFunctorInstance cs x n = instanceD (cxt $ cs ++ [xMap_ x]) (xMapFunctor x n) [functorXmap x, functorYmap x]

-- | @`xmap` = `xmapFunctor`@
functorXmap :: XMaps -> DecQ
functorXmap x = funD (xmp x) [clause [] (normalB . varE . xmpFunctor $ x) []]

-- | @`ymap` = `ymapFunctor`@
functorYmap :: XMaps -> DecQ
functorYmap x = funD (ymp x) [clause [] (normalB . varE . ympFunctor $ x) []]

-- | `functorInstance`, but additional constraints can be specified.
constrainedFunctorInstance :: XMaps -> (TypeQ, TypeQ) -> DecQ
constrainedFunctorInstance x (c, n) = do
  n' <- n
  isFunctor <- isInstance ''Functor [n']
  unless isFunctor . fail $ "Data.X.Map.constrainedFunctorInstance must be supplied with the name of a Functor"
  unsafeFunctorInstance [c] x n


-- | @\p -> `XMap` (p s0 s1) (p t0 t1) a b@
xMapBifunctor :: XMaps -> TypeQ -> TypeQ
xMapBifunctor x f = xMap x (f ~> "s0" ~> "s1", f ~> "t0" ~> "t1", "a", "b")

-- | Make an `XMap` instance for a dual-parameter `Bifunctor` type:
--
-- @
-- bifunctorInstance ''(,)
-- instance (Data.X.Map.XMap s0 t0 a b,
--           Data.X.Map.XMap s1 t1 a b) => Data.X.Map.XMap (GHC.Tuple.(,) s0 s1) (GHC.Tuple.(,) t0 t1) a b
--     where Data.X.Map.xmap = Data.X.Map.xmapBifunctor
--           Data.X.Map.ymap = Data.X.Map.ymapBifunctor
-- @
--
bifunctorInstance :: XMaps -> TypeQ -> DecQ
bifunctorInstance x n = do
  n' <- n
  isBifunctor <- isInstance ''Bifunctor [n']
  unless isBifunctor . fail $ "Data.X.Map.bifunctorInstance must be supplied with the name of a Bifunctor"
  unsafeBifunctorInstance [] x n

-- | `bifunctorInstance` without checking that the type has a `Bifunctor` instance.
unsafeBifunctorInstance :: [TypeQ] -> XMaps -> TypeQ -> DecQ
unsafeBifunctorInstance cs x n = instanceD (cxt $ cs ++ [xMapN x 0, xMapN x 1]) (xMapBifunctor x n) [bifunctorXmap x, bifunctorYmap x]

-- | @`xmap` = `xmapBifunctor`@
bifunctorXmap :: XMaps -> DecQ
bifunctorXmap x = funD (xmp x) [clause [] (normalB . varE . xmpBifunctor $ x) []]

-- | @`ymap` = `ymapBifunctor`@
bifunctorYmap :: XMaps -> DecQ
bifunctorYmap x = funD (ymp x) [clause [] (normalB . varE . ympBifunctor $ x) []]

-- | `bifunctorInstance`, but specifying additional constraints is possible.
constrainedBifunctorInstance :: XMaps -> (TypeQ, TypeQ) -> DecQ
constrainedBifunctorInstance x (c, n) = do
  n' <- n
  isBifunctor <- isInstance ''Bifunctor [n']
  unless isBifunctor . fail $ "Data.X.Map.constainedBifunctorInstance must be supplied with the name of a Bifunctor"
  unsafeBifunctorInstance [c] x n


