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
{-# LANGUAGE OverloadedStrings #-}

module Language.Rust.Inline.TH.ReprC (
  mkReprC,
  mkGenPathTy,
) where

import Language.Rust.Inline.TH.Utilities
import Language.Rust.Inline.Context

import Language.Haskell.TH hiding (Stmt, Match, WildP, Unsafe, LitP, Pat)
import Language.Rust.Syntax
import Language.Rust.Data.Ident            ( Ident, mkIdent )
import qualified Language.Rust.Quote as R

import Control.Monad                       ( void )
import Data.Traversable                    ( for )
import Data.List.NonEmpty                  ( NonEmpty(..) )

-- | TODO mangle me
freshIdent :: String -> Q Ident
freshIdent str = pure (mkIdent str)

-- | The @#[derive(Copy,Clone)]@ attributes
deriveCopyClone :: Attribute ()
deriveCopyClone = void [R.attr| #[derive(Copy,Clone)] |]

-- | The @#[repr(C)]@ attribute
reprC :: Attribute ()
reprC = void [R.attr| #[repr(C)] |]

-- | Make a generic path from an idenitifer
mkGenPath :: Ident -> [Ty ()] -> Path ()
mkGenPath i [] = Path False [ PathSegment i Nothing () ] ()
mkGenPath i g = Path False [ PathSegment i (Just (AngleBracketed [] g [] ())) () ] ()

-- | Make a path from a single identifier.
mkPath :: Ident -> Path ()
mkPath i = Path False [ PathSegment i Nothing () ] ()

-- | Make a path from multiple identifiers.
mkPath' :: [Ident] -> Path ()
mkPath' is = Path False [ PathSegment i Nothing () | i <- is ] ()

-- | Make a path type (e.g. something like @SomeStruct@).
mkPathTy :: Ident -> Ty ()
mkPathTy i = PathTy Nothing (mkPath i) ()

mkPathExpr :: Ident -> Expr ()
mkPathExpr i = PathExpr [] Nothing (mkPath i) ()

mkPathExpr' :: [Ident] -> Expr ()
mkPathExpr' is = PathExpr [] Nothing (mkPath' is) ()

mkIdentPat :: Ident -> Pat ()
mkIdentPat i = IdentP (ByValue Immutable) i Nothing ()

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
copyBound = TraitTyParamBound (PolyTraitRef [] (TraitRef (mkPath "Copy")) ()) None ()

-- | The @Into<T>@ type parameter bound.
intoBound :: Ty () -> TyParamBound ()
intoBound t = TraitTyParamBound (PolyTraitRef [] (TraitRef (mkGenPath "Into" [t])) ()) None ()

-- | Make simple generics from a list of type parameters.
mkGenerics :: [TyParam ()] -> Generics ()
mkGenerics ps = Generics [] ps (WhereClause [] ()) ()

tyParam2Ty :: TyParam () -> Ty ()
tyParam2Ty (TyParam [] i [] Nothing ()) = mkPathTy i
tyParam2Ty _ = error "tyParam2Ty: could not convert type parameter to type"

mkArm :: Pat () -> Expr () -> Arm ()
mkArm p e = Arm [] (p :| []) Nothing e ()

type TyVarDict = [(Name, TyParam ())]


mkReprC :: Context         -- ^ current context (in order to lookup what Rust types corresponding to
                           -- Haskell ones)
        -> Type            -- ^ Haskell type to convert
        -> Q ( Ident       --   Name of Rust enum
             , Maybe Ident --   Name of #[repr(C)] tagged union  
             , [Item ()]   --   Support type definitions and 'impl's
             )
mkReprC ctx ty = do

  -- Extract the context
  (tyvars, ty') <-
    case ty of
      ForallT tyvars [] t -> pure (tyvars, t)
      ForallT _      _  _ -> fail "mkReprC: type cannot have context"
      t                   -> pure ([], t)

  -- Synthesize the new generic args and the mapping
  rustTyParams <- traverse (fmap mkTyParam . freshIdent . nameBase . varName) tyvars
  let dict :: TyVarDict
      dict = zip (map varName tyvars) rustTyParams

  ctx' <- extendCtx dict ctx

  -- Get the type constructors name
  (n, cons) <- getConstructors ty'

  case cons of
    [(_, tys)] -> do
      (_, (i, _), item) <- mkStruct ctx' dict False n tys
  
      pure (i, Nothing, [item])   
    _ -> do
      (tys, is,     items) <- unzip3 <$> traverse (\(n',ts) -> mkStruct ctx' dict True (mkName (nameBase n' ++ "C")) ts) cons
      (tyU, iUnion, itemU) <- mkUnion dict (mkName (nameBase n ++ "CUnion")) tys
      (_, iTagged, item) <- mkTagged dict (mkName (nameBase n ++ "C")) (mkPathTy "u8") tyU
     
      (_, iEnum, iVars, itemE) <- mkEnum ctx' dict n cons
      (impl1, impl2) <- mkFromImpls dict iTagged iUnion is iEnum iVars

      pure (iEnum, Just iTagged, items ++ [itemU, item, itemE, impl1, impl2])


-- | Generate the pair of 'From' trait impl's that allow you to convert back and forth between the
-- #[repr(C)] tagged union and the Rust enum.
mkFromImpls :: TyVarDict     -- ^ Mapping of Haskell type variables to Rust type parameters
            -> Ident         -- ^ Tagged union name
            -> Ident         -- ^ Union name
            -> [(Ident,Int)] -- ^ Structs that are fields of union (and their arity)
            -> Ident         -- ^ Enum name
            -> [Ident]       -- ^ Enum variants
            -> Q ( Item ()   --   Impl mapping #[repr(C)] tagged union into enum
                 , Item ()   --   Impl mapping enum into tagged union
                 )
mkFromImpls dict
            nTagged nUnion nStructs
            nEnum nVariants = do
  
  let -- two copies of type parameters: @T, U, ...@ and @T1, U1, ...@
      (ts, ts1) = unzip [ (TyParam [] i [] Nothing (), TyParam [] (i <> "1") [] Nothing ())
                        | (_, TyParam _ i _ _ _) <- dict
                        ]

      -- type parameters with @Into@ bound: @T1 + Into<T>, U1 + Into<U>, ...@
      ts1BoundIntoT = zipWith consBound (map (intoBound . tyParam2Ty) ts) ts1

      -- full parameters: @T: Copy, U: Copy, ..., T1: Copy + Into<T>, U1: Copy + Into<U>, ...@ 
      fullImplBds = map (consBound copyBound) (ts ++ ts1BoundIntoT)

      -- From<TaggedUnion<T1, U1, ...>>
      fromTaggedUnionTr = mkFromTraitRef nTagged ts1

      -- Enum<T, U, ...> and TaggedUnion<T1, U1, ...>
      enumTy = mkGenPathTy nEnum (map tyParam2Ty ts)
      taggedTy1 = mkGenPathTy nTagged (map tyParam2Ty ts1)
  
      flds = [ FieldPat Nothing (mkIdentPat "tag") ()
             , FieldPat Nothing (mkIdentPat "payload") ()
             ]
      arms1 = [ mkArm pat body
              | (i, (s, n), v) <- zip3 [0..] nStructs nVariants
              , let pat = LitP (Lit [] (Int Dec i Unsuffixed ()) ()) ()
              , let vars = map (mkIdentPat . mkIdent . ('y' :) . show) [0..(n-1)]
              , let exps = map ( (\y -> MethodCall [] y "into" Nothing [] ())
                               . mkPathExpr
                               . mkIdent
                               . ('y' :)
                               . show )
                               [0..(n-1)]
              , let access = FieldAccess [] (mkPathExpr "payload") (mkIdent ("v" ++ show i)) () 
              , let pat2 = if null vars
                             then PathP Nothing (mkPath s) ()
                             else TupleStructP (mkPath s) vars Nothing ()
              , let stmts = [ Local pat2 Nothing (Just access) [] ()
                            , if n == 0
                                then NoSemi (mkPathExpr' [nEnum, v]) ()
                                else NoSemi (Call [] (mkPathExpr' [nEnum, v]) exps ()) ()
                            ]
              , let body = BlockExpr [] (Block stmts Unsafe ()) ()
              ] ++ [ mkArm (WildP ()) (void [R.expr| panic!("Unexpected tag!") |]) ]
      body1 = [ Local (StructP (mkPath nTagged) flds False ())
                      Nothing
                      (Just (mkPathExpr "x"))
                      []
                      ()
              , NoSemi (Match [] (mkPathExpr "tag") arms1 ())
                       ()
              ]

      -- impl<T: Copy, U: Copy, ..., T1: Copy + Into<T>, U1: Copy + Into<U>, ...>
      --   From<TaggedUnion<T1, U1, ...> for Enum<T, U, ...> { ... }
      impl1 = Impl [] InheritedV Final Normal Positive
                   (mkGenerics fullImplBds) (Just fromTaggedUnionTr)
                   enumTy [mkFromFunc taggedTy1 enumTy body1] ()

      -- From<Enum<T1, U1, ...>>
      fromEnumTr = mkFromTraitRef nEnum ts1
      
      -- TaggedUnion<T, U, ...> and Enum<T1, U1, ...>
      taggedTy = mkGenPathTy nTagged (map tyParam2Ty ts)
      enumTy1 = mkGenPathTy nEnum (map tyParam2Ty ts1)

      arms2 = [ mkArm pat body
              | (i, (s, n), v) <- zip3 [0..] nStructs nVariants
              , let vars = map (mkIdentPat . mkIdent . ('y' :) . show) [0..(n-1)]
              , let exps = map ( (\y -> MethodCall [] y "into" Nothing [] ())
                               . mkPathExpr
                               . mkIdent
                               . ('y' :)
                               . show )
                               [0..(n-1)]
              , let pat = if null vars
                            then PathP Nothing (mkPath' [nEnum, v]) ()
                            else TupleStructP (mkPath' [nEnum, v]) vars Nothing ()
              , let e = if n == 0
                          then mkPathExpr s
                          else Call [] (mkPathExpr s) exps ()
              , let flds' = [ Field (mkIdent ('v' : show i)) (Just e) () ]
              , let payload = Struct [] (mkPath nUnion) flds' Nothing ()
              , let flds'' = [ Field "tag" (Just (Lit [] (Int Dec i Unsuffixed ()) ())) ()
                             , Field "payload" (Just payload) ()
                             ]
              , let body = Struct [] (mkPath nTagged) flds'' Nothing ()
              ]
      body2 = [ NoSemi (Match [] (mkPathExpr "x") arms2 ()) () ]

      -- impl<T: Copy, U: Copy, ..., T1: Copy + Into<T>, U1: Copy + Into<U>, ...>
      --   From<Enum<T1, U1, ...> for TaggedUnion<T, U, ...> { ... }
      impl2 = Impl [] InheritedV Final Normal Positive
                   (mkGenerics fullImplBds) (Just fromEnumTr)
                   taggedTy [mkFromFunc enumTy1 taggedTy body2] ()

      
  pure (impl1, impl2)
  where
    mkFromTraitRef :: Ident -> [TyParam ()] -> TraitRef ()
    mkFromTraitRef n t = TraitRef (mkGenPath "From" [ mkGenPathTy n (map tyParam2Ty t) ])

    mkFromFunc :: Ty () -> Ty () -> [Stmt ()] -> ImplItem ()
    mkFromFunc argTy retTy body = let gen = mkGenerics []
                                      arg = Arg (Just (mkIdentPat "x")) argTy ()
                                      decl = FnDecl [arg] (Just retTy) False ()
                                      sig = MethodSig Normal NotConst Rust decl 
                                      blk = Block body Normal ()
                                  in MethodI [] InheritedV Final "from" gen sig blk ()

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
      ps = [ typ  | (_, typ) <- dict ]
      outTy = mkGenPathTy itemN [ mkPathTy i | TyParam _ i _ _ _ <- ps ]
  
  (varNs, vars) <- fmap unzip $
    for cons $ \(nCon, flds) -> do
      let varN = mkIdent (nameBase nCon)
      varData <- mkVariant ctx flds
      pure (varN, Variant varN [] varData Nothing ())

  let enum = Enum [deriveCopyClone] PublicV itemN vars (mkGenerics ps) ()

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
      copyPs = [ consBound copyBound typ  | (_, typ) <- dict ]
      fields = [ StructField (Just fld) InheritedV ty [] () 
               | (ty, i) <- zip tys [(0 :: Int)..]
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
mkTagged :: TyVarDict   -- ^ Mapping of Haskell type variables to Rust type parameters
         -> Name        -- ^ What name to give the struct
         -> Ty ()       -- ^ Discriminator type
         -> Ty ()       -- ^ Payload union type
         -> Q ( Ty ()   --   Output type
              , Ident   --   Output name
              , Item () --   Struct deifnition
              )
mkTagged dict n disc union = do
  let itemN = mkIdent (nameBase n)
      copyPs = [ consBound copyBound typ | (_, typ) <- dict ]
      fields = [ StructField (Just "tag") InheritedV disc [] ()
               , StructField (Just "payload") InheritedV union [] ()
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
mkStruct :: Context          -- ^ current context (in order to lookup what Rust types corresponding to
                             -- Haskell ones)
         -> TyVarDict        -- ^ Mapping of Haskell type variables to Rust type parameters
         -> Bool             -- ^ Enforce a 'Copy' constraint on type parameters
         -> Name             -- ^ What name to give the struct
         -> [Type]           -- ^ Fields of the struct
         -> Q ( Ty ()        --   Output type
              , (Ident, Int) --   Output name and arity
              , Item ()      --   Struct definition
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


  pure (outTy, (itemN, length tys), struct)

-- | Construct the variant data for struct fields.
mkVariant :: Context -> [Type] -> Q (VariantData ())
mkVariant _ [] = pure (UnitD ())
mkVariant ctx tys = do
  rustTys <- traverse (`getHTypeInContext` ctx) tys
  let structFlds = [ StructField Nothing InheritedV t [] () | t <- rustTys ]
  pure (TupleD structFlds ())
