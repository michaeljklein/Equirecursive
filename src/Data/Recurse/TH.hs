{-# LANGUAGE TemplateHaskell #-}

module Data.Recurse.TH where

import Language.Haskell.TH
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Default
import Data.Lifted

-- aa = $(litE $ stringL <$> (litE . ppr) <$> reify ''Either)

-- bb = $(litE $ (stringL . litE . show) <$> reify ''Either)

-- aa = $(((stringL . show) <$> reify ''Int) >>= litE)

-- bb = $(((stringL . show) <$> reify ''Int#) >>= litE)

-- bb = $(((stringL . show) <$> reify ''Int#) >>= litE)



-- Instead of DeqType, should do Deq ++ newName


instance Monad m => Default (MaybeT m a) where
  def = MaybeT (return Nothing)

reifyCon :: Name -> MaybeT Q (Name, Arity)
reifyCon n = lift (reify n) >>= infoCon

infoCon :: Info -> MaybeT Q (Name, Arity)
infoCon info = case info of
  PrimTyConI name arity False -> return (name, arity)
  TyConI dec                  -> decCon dec
  _                           -> def

decCon :: Dec -> MaybeT Q (Name, Arity)
decCon dec = case dec of
  DataD    _ name args Nothing _ _ -> return (name, length args)
  NewtypeD _ name args Nothing _ _ -> return (name, length args)
  _                                -> def


deqInstances :: Name -> DecsQ
deqInstances n = do
  Just (name, arity) <- runMaybeT . reifyCon $ n
  return [ deqInstance     name arity
    , deqTypeInstance name arity]


ands :: [Type] -> Type
ands [  ] = PromotedT 'True
ands [x ] = x
ands  xs  = foldr1 (\x y -> ConT ''(:&&) `AppT` x `AppT` y) xs

-- | @F ?$ [a, b, c] = F a b c@
(?$) :: Type -> [Type] -> Type
(?$) f as = foldl AppT f as

mkVar :: String -> Type
mkVar = VarT . mkName

mkVars :: Functor f => f String -> f Type
mkVars = fmap mkVar

mkCon :: String -> Type
mkCon = ConT . mkName

mkCons :: Functor f => f String -> f Type
mkCons = fmap mkCon

mkStar :: String -> TyVarBndr
mkStar = flip KindedTV StarT . mkName

mkStars :: Functor f => f String -> f TyVarBndr
mkStars = fmap mkStar


-- | type family DeqType (ar :: *) (a0 :: *) .. (an :: *) (br :: *) (b :: *) (d :: Nat) where
--     ..
deqTypeInstance :: Name -> Arity -> Dec
deqTypeInstance nm ay = ClosedTypeFamilyD (deqTypeHead nm ay) [baseDeqTypeInstance nm ay, xyDeqTypeInstance nm ay, defDeqTypeInstance nm ay]

-- | DeqType (ar :: *) (a0 :: *) .. (an :: *) (br :: *) (b :: *) (d :: Nat)
deqTypeHead :: Name -> Arity -> TypeFamilyHead
deqTypeHead nm ay = TypeFamilyHead (mkName $ "Deq" ++ noDots (show nm)) (tyVarBndrs ay) (KindSig . ConT $ ''Bool) def

-- | [ar, a0, .. an, br, b, d]
tyVarBndrs :: Arity -> [TyVarBndr]
tyVarBndrs ay = (mkStars $ ["ar"] ++ [ 'a' : show i | i <- [0..ay-1]] ++ ["br", "b"]) ++ [KindedTV (mkName "d") $ ConT ''Nat]

tyVars :: Arity -> [Type]
tyVars ay = mkVars $ ["ar"] ++ [ 'a' : show i | i <- [0..ay-1]] ++ ["br"]


-- |   DeqType ar a0 .. an br (Type b0 .. bn) d = Deq ar a0 br b0 d :&& .. :&& Deq ar an br bn d
baseDeqTypeInstance :: Name -> Arity -> TySynEqn
baseDeqTypeInstance nm ay = TySynEqn (tyVars ay ++ [fullTypeB nm ay, d]) $ ands [ mkCon "Deq" ?$ [ar, a i, br, b i, d] | i <- [0..ay-1]]
  where
    [ar, br, d] = mkVars ["ar", "br", "d"]
    a i = mkVar $ 'a' : show i
    b i = mkVar $ 'a' : show i

-- |   DeqType ar a0 .. an br      XY         d = Deq ar (Type a0 .. an) br br d
xyDeqTypeInstance :: Name -> Arity -> TySynEqn
xyDeqTypeInstance nm ay = TySynEqn (tyVars ay ++ [mkCon "XY", d]) $ mkCon "Deq" ?$ [ar, fullType nm ay, br, br, d]
  where
    [ar, br, d] = mkVars ["ar", "br", "d"]

-- |   DeqType ar a0 .. an br       b         d = 'False
defDeqTypeInstance :: Name -> Arity -> TySynEqn
defDeqTypeInstance _  ay = TySynEqn (tyVars ay ++ [b, d]) $ PromotedT 'False
  where
    [b, d] = mkVars ["b", "d"]

-- | @type instance `Deq` ar (Type a0 .. an) br b d = `DeqType` ar a0 .. an br b d@
deqInstance :: Name -> Arity -> Dec
deqInstance nm ay = TySynInstD (mkName "Deq") $ TySynEqn [ar, fullType nm ay, br, b, PromotedT 'S `AppT` d] $ fullDeqType nm ay
  where
    [ar, br, b, d] = map mkVar ["ar", "br", "b", "d"]

-- | @fullType T n -> T a0 .. a(n-1)@
fullType :: Name -> Arity -> Type
fullType nm ay = ConT nm ?$ [ a i | i <- [0..ay-1]]
  where
    a i = mkVar $ 'a' : show i

noDots :: String -> String
noDots = map noDot
  where
    noDot '.' = '_'
    noDot  x  =  x

-- | @fullTypeB T n -> T b0 .. b(n-1)@
fullTypeB :: Name -> Arity -> Type
fullTypeB nm ay = ConT nm ?$ [ b i | i <- [0..ay-1]]
  where
    b i = mkVar $ 'b' : show i

-- | @fullDeqType T n -> DeqT ar a0 .. an br b d@
fullDeqType :: Name -> Arity -> Type
fullDeqType nm ay = ConT (mkName $ "Deq" ++ noDots (show nm)) ?$ (ar : [a i | i <- [0..ay-1]] ++ [br, b, d])
  where
    [ar, br, b, d] = mkVars ["ar", "br", "b", "d"]
    a i = mkVar $ 'a' : show i

