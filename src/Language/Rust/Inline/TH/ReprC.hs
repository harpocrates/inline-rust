{-|
Module      : Language.Rust.Inline.TH.ReprC
Description : Generate #[repr(C)] Rust types
Copyright   : (c) Alec Theriault, 2018
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : GHC
-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wwarn #-}         -- TODO: GHC bug around "unused pattern binds" in splices
                                   -- TODO: GHC bug around setting extensions from within TH
module Language.Rust.Inline.TH.ReprC where

import Language.Rust.Inline.TH.Utilities
import Language.Rust.Inline.Context

import Language.Haskell.TH
import Language.Rust.Syntax

mkPathTy :: Ident -> Ty ()
mkPathTy i = PathTy Nothing (Path False [ PathSegment i Nothing () ] ()) ()

mkReprC :: TypeQ               -- ^ a Haskell type
        -> (Type -> RType)
        -> Q Context           -- ^ the declarations needed for a compatible Rust type
mkReprC tyq func = do
  (n, cons) <- getConstructors =<< tyq
  

  case cons of
    [(_, tys)] -> do
      (i, item) <- mkStruct n tys
      pure (i, [item])
    _ -> do
      (is, items) <- unzip <$> traverse (curry mkStruct) cons
      (i, item) <- mkUnion n is
      pure (i, item : items)
      

  let rustTy = mkPathTy rustN


  pure $ Context [ \rty _ -> if rustTy /= rty then mempty else pure tyq ]
  
  -- Rust stuff...
  case cons of
    [con] -> 
    _ ->

  where
    noGen :: Generics ()
    noGen = Generics [] [] (WhereClause [] ()) ()
     
    mkVariant :: Context -> [Type] -> Q (VariantData ())
    mkVariant ctx tys = do
      rustTys <- traverse func tys
      let structFlds = [ StructField Nothing PublicV t [] () | t <- rustTys ]
      pure (TupleD structFlds ())
       

    mkStruct :: Name -> [Type] -> Q (Ident, Item)
    mkStruct n tys = do
      var <- mkVariant n tys

      let rustN = fromString (nameBase n)
          struct = StructItem [] PublicV itemN var noGen ()

      pure (rustN, struct)

    mkUnion :: Name -> [Ident] -> Q (Ident, Item)
    mkUnion n variantNames = do
      let rustN = fromString (nameBase n)
          vars = [ TupleD [StructField Nothing PublicV (mkPathTy i) [] () ] ()
                 | i <- variantNames
                 ]
    
      pure (rustN, Union [] PublicV rustN _ noGen vars ())
      
