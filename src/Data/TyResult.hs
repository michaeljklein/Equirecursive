{-# LANGUAGE TemplateHaskell #-}

module Data.TyResult where

import GHC.TypeLits
import Data.X
import Language.Haskell.TH.Utils
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (TExp(..))


-- | Kind-level simple result. Must be dropped to resolve its `ErrorMessage`
data TyResult = TySuccess ErrorMessage | TyFails ErrorMessage

-- | Get whether a type with kind `TyResult` is success.
class Success (r :: TyResult) where
  -- | Return `True` if `TySuccess`
  success :: forall proxy. proxy r -> Bool

-- | Return `True`
instance Success ('TySuccess e) where
  success _ = True

-- | Return `False`
instance Success ('TyFails e) where
  success _ = False

-- | Raise the type error contained in a `TyFails`
class DropTyError (r :: TyResult) where
  -- | This is just to match the type with the instance
  dropTyError :: X r -> X r

-- | Do nothing
instance DropTyError (TySuccess e) where
  dropTyError = id

-- | Raise a `TypeError`
instance TypeError e => DropTyError (TyFails e) where
  dropTyError = id

-- | To help make sure that an `ExpQ` includes a `TyResult`
newtype TyResultQ = TyResultQ { getTyResultQ :: TExpQ TyResult }

-- | Show an `ErrorMessage` inside of a `TyResult` using TH.
-- TODO: This might be a pain.
showErrorMessage :: TyResultQ -> TExpQ String
showErrorMessage (TyResultQ expq) = TExp <$> [| unTypeQ expq >>= \(SigT _ ty) -> return (showErrorMessageType ty) |]

-- | Incomplete
showErrorMessageType :: Type -> String
showErrorMessageType t = case t of
  ty@(AppT (PromotedT text    ) (LitT (StrTyLit str))) -> if text == 'Text
                                                             then str
                                                             else show (ppr ty)
  ty@(AppT (PromotedT showType) (               ty1 )) -> if showType == 'ShowType
                                                                   then show (ppr ty1)
                                                                   else show (ppr ty)
  ty@(AppT (AppT (PromotedT append) ty1) ty2) -> if append == '(:<>:)
                                                      then unwords . fmap showErrorMessageType $ [ty1, ty2]
                                                      else if append == '(:$$:)
                                                      then unlines . fmap showErrorMessageType $ [ty1, ty2]
                                                      else show (ppr ty)
  ty -> show (ppr ty)


