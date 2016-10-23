{-# LANGUAGE TemplateHaskell #-}

module Data.Recurse.TH where

import Language.Haskell.TH
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Default
import Data.Lifted
import Control.Monad (liftM2)

import Control.Applicative

-- aa = $(litE $ stringL <$> (litE . ppr) <$> reify ''Either)
-- bb = $(litE $ (stringL . litE . show) <$> reify ''Either)
-- aa = $(((stringL . show) <$> reify ''Int) >>= litE)
-- bb = $(((stringL . show) <$> reify ''Int#) >>= litE)
-- bb = $(((stringL . show) <$> reify ''Int#) >>= litE)
bb = $(((stringL . show) <$> reify (tupleDataName 4)) >>= litE)


-- What about (f :*: g) a?
-- This doesn't currently support non-* kinded arguments
-- However, it may if we can deconstruct types...


instance Monad m => Default (MaybeT m a) where
  def = MaybeT (return Nothing)

-- | Fails if not representable as
-- @`ConT` name \``AppT`\` a_1 .. \``AppT`\` a_arity@
reifyCon :: Name -> MaybeT Q (Name, Arity)
reifyCon n = lift (reify n) >>= infoCon

infoCon :: Info -> MaybeT Q (Name, Arity)
infoCon info = case info of
  PrimTyConI name          arity     False -> return (name, arity)
  DataConI   name (ForallT args _ _) _     -> return (name, length args)
  TyConI dec                               -> decCon dec
  _                                        -> def

decCon :: Dec -> MaybeT Q (Name, Arity)
decCon dec = case dec of
  DataD    _ name args Nothing _ _ -> return (name, length args)
  NewtypeD _ name args Nothing _ _ -> return (name, length args)
  _                                -> def


-- | Generate instances for Deq
deqInstances :: Name -> DecsQ
deqInstances n = do
  Just (name, arity) <- runMaybeT . reifyCon $ n
  (unique1, unique2) <- liftM2 (,) (newName "") (newName "")
  return [ reqTypeInstance name unique1 arity
    , reqInstance     name unique1 arity
    , deqTypeInstance name unique2 arity
    , deqInstance     name unique2 arity
    ]


--------------------------------------------------------------------------------
-- Type folds

ands :: [Type] -> Type
ands [  ] = PromotedT 'True
ands [x ] = x
ands  xs  = foldr1 (\x y -> ConT ''(:&&) `AppT` x `AppT` y) xs

-- | @F ?$ [a, b, c] = F a b c@
(?$) :: Type -> [Type] -> Type
(?$) f as = foldl AppT f as

--------------------------------------------------------------------------------
-- Lots of String -> Type helpers

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

--------------------------------------------------------------------------------
-- Aliases for names, would be newtypes, ideally

type RecName = Name
type TypeName = Name
type UniqueName = Name

--------------------------------------------------------------------------------
-- Generic type variable lists

-- | [ar, a0, .. an, br, b, d]
tyVarBndrs :: Arity -> [TyVarBndr]
tyVarBndrs ay = undefined -- (mkStars $ ["ar"] ++ [ 'a' : show i | i <- [0..ay-1]] ++ ["br", "b"]) ++ [KindedTV (mkName "d") $ ConT ''Nat]

-- | [ar, a0, .. an, br]
tyVars :: Arity -> [Type]
tyVars ay = mkVars $ ["ar"] ++ [ 'a' : show i | i <- [0..ay-1]] ++ ["br"]

--------------------------------------------------------------------------------
-- Methods equivalent for Deq and Req

-- | RecNameUniqueName (ar :: *) (a0 :: *) .. (an :: *) (br :: *) (b :: *) (d :: Nat)
typeHead :: RecName -> UniqueName -> Arity -> TypeFamilyHead
typeHead rn un ay = TypeFamilyHead (mkName $ show rn ++ show un) (tyVarBndrs ay) (KindSig . ConT $ ''Bool) def

-- | @fullType 'a' T n -> T a0 .. a(n-1)@
fullType :: Char -> TypeName -> Arity -> Type
fullType c tn ay = ConT tn ?$ [ a i | i <- [0..ay-1]]
  where
    a i = mkVar $ c : show i

-- | @fullRecType T n -> DeqT ar a0 .. an br b d@
fullRecType :: RecName -> UniqueName -> Arity -> Type
fullRecType rn un ay = ConT (mkName $ show rn ++ show un) ?$ (ar : [a i | i <- [0..ay-1]] ++ [br, b, d])
  where
    [ar, br, b, d] = mkVars ["ar", "br", "b", "d"]
    a i = mkVar $ 'a' : show i

-- |   RecType ar a0 .. an br       b         d = 'False
defTypeInstance :: Arity -> TySynEqn
defTypeInstance ay = TySynEqn (tyVars ay ++ [b, d]) $ PromotedT 'False
  where
    [b, d] = mkVars ["b", "d"]

--------------------------------------------------------------------------------

-- | @type instance Deq ar (Type a0 .. an) br b d = `DeqUnique` ar a0 .. an br b d@
deqInstance :: TypeName -> UniqueName -> Arity -> Dec
deqInstance tn un ay = undefined -- TySynInstD (mkName "Deq") $ TySynEqn [ar, fullType 'a' tn ay, br, b, PromotedT 'S `AppT` d] $ fullRecType (mkName "Deq") un ay
  where
    [ar, br, b, d] = map mkVar ["ar", "br", "b", "d"]

-- | @type instance Req ar (Type a0 .. an) br b d = `ReqUnique` ar a0 .. an br b d@
reqInstance :: TypeName -> UniqueName -> Arity -> Dec
reqInstance tn un ay = TySynInstD (mkName "Req") $ TySynEqn [ar, fullType 'a' tn ay, br, b, d] $ fullRecType (mkName "Req") un ay
  where
    [ar, br, b, d] = map mkVar ["ar", "br", "b", "d"]


-- | type family DeqType (ar :: *) (a0 :: *) .. (an :: *) (br :: *) (b :: *) (d :: Nat) where
--     ..
deqTypeInstance :: TypeName -> UniqueName -> Arity -> Dec
deqTypeInstance tn un ay = ClosedTypeFamilyD (typeHead (mkName "Deq") un ay) [baseDeqTypeInstance tn ay, xyDeqTypeInstance tn ay, defTypeInstance ay]

-- | type family ReqType (ar :: *) (a0 :: *) .. (an :: *) (br :: *) (b :: *) (d :: Nat) where
--     ..
reqTypeInstance :: TypeName -> UniqueName -> Arity -> Dec
reqTypeInstance tn un ay = ClosedTypeFamilyD (typeHead (mkName "Req") un ay) [baseReqTypeInstance tn ay, xyReqTypeInstance tn ay, defTypeInstance ay]


-- |   DeqType ar a0 .. an br (Type b0 .. bn) d = Deq ar a0 br b0 d :&& .. :&& Deq ar an br bn d
baseDeqTypeInstance :: Name -> Arity -> TySynEqn
baseDeqTypeInstance tn ay = TySynEqn (tyVars ay ++ [fullType 'b' tn ay, d]) $ ands [ mkCon "Deq" ?$ [ar, a i, br, b i, d] | i <- [0..ay-1]]
  where
    [ar, br, d] = mkVars ["ar", "br", "d"]
    a i = mkVar $ 'a' : show i
    b i = mkVar $ 'b' : show i

-- |   ReqType ar a0 .. an br (Type b0 .. bn) d = Req ar a0 br b0 ('S d) :&& .. :&& Req ar an br bn ('S d)
baseReqTypeInstance :: Name -> Arity -> TySynEqn
baseReqTypeInstance tn ay = undefined -- TySynEqn (tyVars ay ++ [fullType 'b' tn ay, d]) $ ands [ mkCon "Req" ?$ [ar, a i, br, b i, PromotedT 'S `AppT` d] | i <- [0..ay-1]]
  where
    [ar, br, d] = mkVars ["ar", "br", "d"]
    a i = mkVar $ 'a' : show i
    b i = mkVar $ 'b' : show i


-- |   DeqType ar a0 .. an br      XY         d = Deq ar (Type a0 .. an) br br    d
xyDeqTypeInstance :: TypeName -> Arity -> TySynEqn
xyDeqTypeInstance tn ay = TySynEqn (tyVars ay ++ [mkCon "XY", d]) $ mkCon "Deq" ?$ [ar, fullType 'a' tn ay, br, br, d]
  where
    [ar, br, d] = mkVars ["ar", "br", "d"]

-- |   ReqType ar a0 .. an br      XY         d = ReqExpand br ar (Type a0 .. an) d
xyReqTypeInstance :: TypeName -> Arity -> TySynEqn
xyReqTypeInstance tn ay = TySynEqn (tyVars ay ++ [mkCon "XY", d]) $ mkCon "ReqExpand" ?$ [br, ar, fullType 'a' tn ay, d]
  where
    [ar, br, d] = mkVars ["ar", "br", "d"]

