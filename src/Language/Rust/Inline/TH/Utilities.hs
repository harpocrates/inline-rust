{-|
Module      : Language.Rust.Inline.TH.Utilities
Description : Generate Storable instances
Copyright   : (c) Alec Theriault, 2018
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : GHC
-}
{-# LANGUAGE RankNTypes #-}

module Language.Rust.Inline.TH.Utilities (
  getConstructors,
  varName,
  getAllVars,
  getTyConOpt,
  getTyCon,
) where

import Language.Haskell.TH

import Data.Maybe                ( fromMaybe )
import Data.Data                 ( Data(..), Typeable, cast )


-- | Given a fully applied type, decompose it into a list of constructors and
-- the type of the arguments they expect.
--
-- >>> getContructors =<< [t| Either Int a |]
--
--
-- Supports only simple Haskell 98 style constructors.
getConstructors :: Type -> Q (Name, [(Name, [Type])])
getConstructors ty = do
  -- Get the type constructors name
  (n,args) <- getTyCon ty
  info <- reify n

  -- Get the constructors out
  (cons, tyvars) <-
    case info of
      TyConI (DataD    _c _n tyvars _k cons _ds) -> pure (cons, tyvars)
      TyConI (NewtypeD _c _n tyvars _k con  _ds) -> pure ([con], tyvars)
      _ -> fail "getConstructors: could not find simple type constructor"

  -- Get the fields
  let dict = zip (map varName tyvars) args
  cons' <- traverse (getSubCon dict) cons

  pure (n, cons')


-- | Get the name of a type variable binder
varName :: TyVarBndr -> Name
varName (PlainTV n) = n
varName (KindedTV n _) = n

-- | Apply a substitution (of type variable to type) to a type
subTy :: [(Name, Type)] -> Type -> Type
subTy dict (AppT t1 t2) = AppT (subTy dict t1) (subTy dict t2)
subTy dict (VarT n) = fromMaybe (VarT n) $ lookup n dict
subTy dict (InfixT t1 n t2) = InfixT (subTy dict t1) n (subTy dict t2)
subTy dict (ParensT t) = ParensT (subTy dict t)
subTy _    t = t

-- | Extract the type constructor of a type, along with the type arguments
getTyCon :: Type -> Q (Name, [Type])
getTyCon = maybe (fail msg) pure . getTyConOpt
  where  msg = "getTyCon: could not find type constructor"


-- | Extract the type constructor of a type, along with the type arguments
getTyConOpt :: Type -> Maybe (Name, [Type])
getTyConOpt = fmap (\(n, argsRev) -> (n, reverse argsRev)) . go
  where
    go (ConT n) = pure (n, [])
    go (AppT t1 t2) = fmap (\(n, args) -> (n, t2 : args)) (go t1)
    go (InfixT t1 n t2) = pure (n, [t1, t2])
    go (ParensT t) = go t
    go _ = Nothing 


-- | Extract the fields from a constructor, applying a substitution along the
-- way
getSubCon :: [(Name, Type)] -> Con -> Q (Name, [Type])
getSubCon dict (NormalC c ts)   = pure (c, [ subTy dict t | (_, t) <- ts ])
getSubCon dict (RecC c ts)      = pure (c, [ subTy dict t | (_, _, t) <- ts ])
getSubCon dict (InfixC t1 c t2) = pure (c, [ subTy dict t | (_, t) <- [t1,t2] ])
getSubCon _    _ = fail "processCon: unsupported constructor type"

-- | Extract all of the type variables from a type
getAllVars :: Type -> [Name]
getAllVars = collect getVar 
  where
  getVar :: Typeable a => a -> [Name]
  getVar x = case cast x of
               Just (VarT n) -> [n]
               _ -> []

  collect :: Data b => (forall a. Data a => a -> [r]) -> b -> [r]
  collect f x = concat (f x : gmapQ (collect f) x)

