{-|
Module      : Language.Rust.Inline.TH.ReprC
Description : Generate #[repr(C)] Rust types
Copyright   : (c) Alec Theriault, 2018
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : GHC
-}

{-# OPTIONS_GHC -Wwarn #-}         -- TODO: GHC bug around "unused pattern binds" in splices
                                   -- TODO: GHC bug around setting extensions from within TH
module Language.Rust.Inline.TH.ReprC where

import Language.Rust.Inline.TH.Utilities
import Language.Rust.Inline.Context

import Language.Haskell.TH
import Language.Rust.Syntax
import Language.Rust.Data.Ident ( Ident, mkIdent )

import Data.Traversable ( for )

-- | TODO mangle me
freshIdent :: String -> Q Ident
freshIdent str = pure (mkIdent str)

-- | The @#[derive(Copy,Clone)]@ attributes
deriveCopyClone :: Attribute ()
deriveCopyClone = undefined

-- | The @#[repr(C)]@ attribute
reprC :: Attribute ()
reprC = undefined

-- | Make a generic path from an idenitifer
mkGenPath :: Ident -> [Ty ()] -> Path ()
mkGenPath i g = Path False [ PathSegment i (Just (AngleBracketed [] g [] ())) () ] ()

-- | Make a path from a single identifier.
mkPath :: Ident -> Path ()
mkPath i = Path False [ PathSegment i Nothing () ] ()

-- | Make a path type (e.g. something like @SomeStruct@).
mkPathTy :: Ident -> Ty ()
mkPathTy i = PathTy Nothing (mkPath i) ()

-- | Make a generic path type (e.g. something like @Vec<T>@).
mkGenPathTy :: Ident -> [Ty ()] -> Ty ()
mkGenPathTy i g = PathTy Nothing (mkGenPath i g) ()

-- | Make a new type parameter.
mkTyParam :: Ident -> TyParam ()
mkTyParam i = TyParam [] i [] Nothing ()

-- | Add a type parameter bound on an existing type parameter.
consBound :: TyParamBound () -> TyParam () -> TyParam ()
consBound bd (TyParam as i bds def x) = TyParam as i (bd : bds) def x

-- | The @Copy@ type parameter bound.
copyBound :: TyParamBound ()
copyBound = TraitTyParamBound (PolyTraitRef [] (TraitRef (mkPath (mkIdent "Copy"))) ()) None ()

-- | Make simple generics from a list of type parameters.
mkGenerics :: [TyParam ()] -> Generics ()
mkGenerics ps = Generics [] ps (WhereClause [] ()) ()

type TyVarDict = [(Name, TyParam ())]


mkReprC :: Context     -- ^ current context (in order to lookup what Rust types corresponding to
                       -- Haskell ones)
        -> Type        -- ^ Haskell type to convert
        -> Q ()          -- TODO
mkReprC ctx ty = do
  
  -- Extract the context
  (tyvars, ty') <-
    case ty of
      ForallT tyvars [] ty -> pure (tyvars, ty)
      ForallT _      _  _  -> fail "mkReprC: type cannot have context"
      ty                   -> pure ([], ty)

  -- Synthesize the new generic args and the mapping
  rustTyParams <- traverse (fmap mkTyParam . freshIdent . nameBase . varName) tyvars
  let dict :: TyVarDict
      dict = zip (map varName tyvars) rustTyParams

  ctx' <- extendCtx dict ctx

  -- Get the type constructors name
  (n, cons) <- getConstructors ty'

  case cons of
    [(_, tys)] -> do
      (ty, i, item) <- mkStruct ctx' dict False n tys
   
      error "incomplete"
    _ -> do
      (tys, is,     items) <- unzip3 <$> traverse (uncurry (mkStruct ctx' dict True)) cons
      (tyU, iUnion, itemU) <- mkUnion dict (mkName (nameBase n ++ "CUnion")) tys
      (tyE, iTagged, item) <- mkTagged ctx' dict (mkName (nameBase n ++ "C")) (mkPathTy (mkIdent "u8")) tyU
     
      (tyE, iEnum, iVars, itemE) <- mkEnum ctx' dict n cons
      error "unimplemented" -- TODO two 'Into' symmetric impls



-- | Extend the context with the type variables in the given dictionary.
extendCtx :: TyVarDict -> Context -> Q Context
extendCtx dict ctx = dictCtx <> pure ctx
  where
    dictCtx = mkContext [ (mkPathTy i, varT v)
                        | (v, TyParam _ i _ _ _) <- dict
                        ]


-- | Make an enum corresponding to an ADT. For example:
--
-- @
-- enum EitherInt<T> {
--   LeftInt(i32),
--   Right(T),
-- }
-- @
--
mkEnum :: Context           -- ^ current context (in order to lookup what Rust types corresponding to
                            -- Haskell ones)
       -> TyVarDict         -- ^ Mapping of Haskell type variables to Rust type parameters
       -> Name              -- ^ What name to give the union
       -> [(Name, [Type])]  -- ^ Variants
       -> Q ( Ty ()         --   Output type
            , Ident         --   Output name
            , [Ident]       --   Variant names
            , Item ()       --   Union definition
            )
mkEnum ctx dict n cons = do
  let itemN = mkIdent (nameBase n)
      ps = [ typ  | (ht, typ) <- dict ]
      outTy = mkGenPathTy itemN [ mkPathTy i | TyParam _ i _ _ _ <- ps ]
  
  (varNs, vars) <- fmap unzip $
    for cons $ \(n, flds) -> do
      let varN = mkIdent (nameBase n)
      varData <- mkVariant ctx flds
      pure (varN, Variant varN [] varData Nothing ())

  let enum = Enum [] PublicV itemN vars (mkGenerics ps) ()

  pure (outTy, itemN, varNs, enum)


-- | Make a union struct. It is assumed all of the parameters in the dictionary are used. For
-- instance, given the variants @SomeStruct<a>@ and @AnotherStruct@, the following would be
-- generated:
--
-- @
-- #[repr(C)]
-- #[derive(Copy,Clone)]
-- union SomeUnion<a: Copy> {
--   v0: SomeStruct<a>,
--   v1: AnotherStruct,
-- }
-- @
--
mkUnion :: TyVarDict   -- ^ Mapping of Haskell type variables to Rust type parameters
        -> Name        -- ^ What name to give the union
        -> [Ty ()]     -- ^ Variants
        -> Q ( Ty ()   --   Output type
             , Ident   --   Output name
             , Item () --   Union definition
             )
mkUnion dict n tys = do
  let itemN = mkIdent (nameBase n)
      copyPs = [ consBound copyBound typ  | (ht, typ) <- dict ]
      fields = [ StructField (Just fld) PublicV ty [] () 
               | (ty, i) <- zip tys [0..]
               , let fld = mkIdent ("v" ++ show i)
               ]
      var = StructD fields ()
      union = Union [reprC, deriveCopyClone] PublicV itemN var (mkGenerics copyPs) ()
      outTy = mkGenPathTy itemN [ mkPathTy i | TyParam _ i _ _ _ <- copyPs ]

  pure (outTy, itemN, union)


-- | Make a tagged (esp. union) compound data. For example:
--
-- @
-- #[repr(C)]
-- #[derive(Copy,Clone)]
-- struct TaggedADT<a: Copy> {
--   tag: u8,
--   payload: UnionADT<a>,
-- }
-- @
mkTagged :: Context     -- ^ current context (in order to lookup what Rust types corresponding to
                        -- Haskell ones)
         -> TyVarDict   -- ^ Mapping of Haskell type variables to Rust type parameters
         -> Name        -- ^ What name to give the struct
         -> Ty ()       -- ^ Discriminator type
         -> Ty ()       -- ^ Payload union type
         -> Q ( Ty ()   --   Output type
              , Ident   --   Output name
              , Item () --   Struct deifnition
              )
mkTagged ctx dict n disc union = do
  let itemN = mkIdent (nameBase n)
      copyPs = [ consBound copyBound typ | (ht, typ) <- dict ]
      fields = [ StructField (Just (mkIdent "tag")) PublicV disc [] ()
               , StructField (Just (mkIdent "payload")) PublicV union [] ()
               ]
      var = StructD fields ()
      struct = StructItem [reprC, deriveCopyClone] PublicV itemN var (mkGenerics copyPs) ()
      outTy = mkGenPathTy itemN [ mkPathTy i | TyParam _ i _ _ _ <- copyPs ]

  pure (outTy, itemN, struct)


-- | Make a tuple struct from a given set of fields. For instance, given the fields @[Int, a]@,
-- the following would be generated:
--
-- @
-- #[repr(C)]
-- #[derive(Copy,Clone)]
-- struct SomeStruct<a: Copy>(i32, a);
-- @
--
mkStruct :: Context     -- ^ current context (in order to lookup what Rust types corresponding to
                        -- Haskell ones)
         -> TyVarDict   -- ^ Mapping of Haskell type variables to Rust type parameters
         -> Bool        -- ^ Enforce a 'Copy' constraint on type parameters
         -> Name        -- ^ What name to give the struct
         -> [Type]      -- ^ Fields of the struct
         -> Q ( Ty ()   --   Output type
              , Ident   --   Output name
              , Item () --   Struct definition
              )
mkStruct ctx dict copy n tys = do
  var <- mkVariant ctx tys

  let itemN = mkIdent (nameBase n)
      haskTysUsed = concatMap getAllVars tys
      copyPs = [ if copy then consBound copyBound typ else typ
               | (ht, typ) <- dict, ht `elem` haskTysUsed
               ]
      struct = StructItem [reprC, deriveCopyClone] PublicV itemN var (mkGenerics copyPs) ()
      outTy = mkGenPathTy itemN [ mkPathTy i | TyParam _ i _ _ _ <- copyPs ]


  pure (outTy, itemN, struct)

-- | Construct the variant data for struct fields.
mkVariant :: Context -> [Type] -> Q (VariantData ())
mkVariant _ [] = pure (UnitD ())
mkVariant ctx tys = do
  rustTys <- traverse (`getHTypeInContext` ctx) tys
  let structFlds = [ StructField Nothing PublicV t [] () | t <- rustTys ]
  pure (TupleD structFlds ())
