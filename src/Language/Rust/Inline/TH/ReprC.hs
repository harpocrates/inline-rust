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

-- TODO: Map haskell records to Rust records
-- TODO: Way for users to specify the attributes they want on generated data types

import Language.Rust.Inline.TH.Utilities
import Language.Rust.Inline.Context

import Language.Haskell.TH hiding (Stmt, Match, WildP, Unsafe, LitP, Pat)
import Language.Rust.Syntax
import Language.Rust.Data.Ident            ( Ident(..), mkIdent )
import qualified Language.Rust.Quote as R

import Control.Monad                       ( void )
import Data.Traversable                    ( for )
import Data.List.NonEmpty                  ( NonEmpty(..) )
import Data.Char                           ( toUpper )
import Data.Maybe                          ( isJust, fromMaybe )
import Data.Word

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
marshalBound :: Ty () -> TyParamBound ()
marshalBound t = TraitTyParamBound (PolyTraitRef [] (TraitRef (mkGenPath "MarshalInto" [t])) ()) None ()

-- | Make simple generics from a list of type parameters.
mkGenerics :: [TyParam ()] -> Generics ()
mkGenerics ps = Generics [] ps (WhereClause [] ()) ()

tyParam2Ty :: TyParam () -> Ty ()
tyParam2Ty (TyParam [] i [] Nothing ()) = mkPathTy i
tyParam2Ty _ = error "tyParam2Ty: could not convert type parameter to type"

mkArm :: Pat () -> Expr () -> Arm ()
mkArm p e = Arm [] (p :| []) Nothing e ()

type TyVarDict = [(Name, TyParam ())]

data TranslatedTy a = Translated
  { direct :: a       -- ^ The direct translation
  , optReprC :: Maybe a  -- ^ The @#[repr(C)]@ translation, if different from the
                      -- direct one
  }

instance Functor TranslatedTy where
  fmap f (Translated d r) = Translated (f d) (fmap f r)
{-
instance Applicative TranslatedTy where
  pure x = Translated x Nothing
  Translated d1 Nothing <*> Translated d2 Nothing = Translated (d1 d2) Nothing
  t1 <*> t2 = Translated (direct t1 $ direct t2) (getReprC t1 $ getReprC t2)
-}
getReprC :: TranslatedTy a -> a
getReprC = fromMaybe <$> direct <*> optReprC


mkReprC :: Context         -- ^ current context (in order to lookup what Rust types corresponding to
                           -- Haskell ones)
        -> Type            -- ^ Haskell type to convert
        -> Q ( Ident       --   Name of Rust enum
             , Maybe Ident --   Name of #[repr(C)] tagged union  
             , [Item ()]   --   Support type definitions
             , [Item ()]   --   'impl's of @MarshalInto@
             )
mkReprC ctx ty = do

  -- Extract the context
  (tyvars, ty') <-
    case ty of
      ForallT tyvars [] t -> pure (tyvars, t)
      ForallT _      _  _ -> fail "mkReprC: type cannot have context"
      t                   -> pure ([], t)

  -- Synthesize the new generic args and the mapping
  rustTyParams <- traverse (fmap mkTyParam . freshIdent . map toUpper . nameBase . varName) tyvars
  let dict :: TyVarDict
      dict = zip (map varName tyvars) rustTyParams

  ctx' <- extendCtx dict ctx

  -- Get the type constructors name
  (n, cons) <- getConstructors ty'

  case cons of
    [(_, tys)] -> do
      str <- mkStruct ctx' dict False n (mkName (nameBase n <> "C")) tys
      let (i1,i2,c,items) = case str of
            Translated (_, (i1', c'), item1) (Just (_, (i2', _), item2)) -> (i1',i2',c',[item1,item2])
            Translated (_, (i1', c'), item1) Nothing -> (i1',i1',c',[item1])
      impls <- mkMarshalStructImpls dict i1 i2 c

      pure (i1, Just i2, items, impls)   
    _ -> do
      (tys, is,     items) <- unzip3 <$> traverse (\(n',ts) ->  let nc = (mkName (nameBase n' ++ "C")) in fmap getReprC (mkStruct ctx' dict True nc nc ts)) cons
      
      (tyU, iUnion, itemU) <- mkUnion dict (mkName (nameBase n ++ "CUnion")) tys
      
      let numDiscs = length cons
          disc = snd . head . dropWhile (\(m,_) -> numDiscs > m + 1) $
              [ (fromIntegral (maxBound :: Word8),  "u8")
              , (fromIntegral (maxBound :: Word16), "u16")
              , (fromIntegral (maxBound :: Word32), "u32")
              , (fromIntegral (maxBound :: Word64), "u64")
              ]
      (_, iTagged, item) <- mkTagged dict (mkName (nameBase n ++ "C")) (mkPathTy disc) tyU
     
      (_, iEnum, iVars, itemE) <- mkEnum ctx' dict n cons
      (impl1, impl2) <- mkMarshalEnumImpls dict iTagged iUnion is iEnum iVars

      pure (iEnum, Just iTagged, items ++ [itemU, item, itemE], [impl1, impl2])


mkMarshalStructImpls :: TyVarDict
                     -> Ident     -- ^ struct 1's name
                     -> Ident     -- ^ struct 2's name
                     -> Int       -- ^ number of arguments
                     -> Q [Item ()]
mkMarshalStructImpls dict nStruct nStruct' n = do
  let -- two copies of type parameters: @T, U, ...@ and @T1, U1, ...@
      ts, ts1 :: [TyParam ()]
      (ts, ts1) = unzip [ (mkTyParam i', mkTyParam (i' <> "1"))
                        | (_, TyParam _ (Ident i _ _) _ _ _) <- dict
                        , let i' = mkIdent (map toUpper i)
                        ]

      -- type parameters: @T1 + MarshalInto<T>, U1 + MarshalInto<U>, ...@
      ts1BoundIntoT = zipWith consBound (map (marshalBound . tyParam2Ty) ts) ts1

      -- Full type parameter bounds.
      --
      -- @
      --    T:  Copy,                  U:  Copy,                  ...,
      --    T1: Copy + MarshalInto<T>, U1: Copy + MarshalInto<U>, ...
      -- @
      fullImplBds :: [TyParam ()]
      fullImplBds = map (consBound copyBound) (ts ++ ts1BoundIntoT)

      -- MarshalInto<MyStruct<T, U, ...>>
      marshalIntoStruct = mkMarshalInto nStruct ts
      marshalIntoStruct' = mkMarshalInto nStruct' ts

      -- MyStruct<T1, U1, ...> and MyStruct<T, U, ...>
      structTy1 = mkGenPathTy nStruct (map tyParam2Ty ts1)
      structTy1' = mkGenPathTy nStruct' (map tyParam2Ty ts1)
      structTy = mkGenPathTy nStruct (map tyParam2Ty ts)
      structTy' = mkGenPathTy nStruct' (map tyParam2Ty ts)

      vars = map (mkIdentPat . mkIdent . ('x' :) . show) [0..(n-1)]
      exps = map ( (\y -> MethodCall [] y "marshal" Nothing [] ())
                 . mkPathExpr
                 . mkIdent
                 . ('x' :)
                 . show )
                 [0..(n-1)]
      pat x = if null vars
                then PathP Nothing (mkPath x) ()
                else TupleStructP (mkPath x) vars Nothing ()
      stmts x y = [ Local (pat x) Nothing (Just (mkPathExpr "self")) [] ()
                  , if n == 0
                      then NoSemi (mkPathExpr y) ()
                      else NoSemi (Call [] (mkPathExpr y) exps ()) ()
                  ]

      -- impl<T: Copy, U: Copy, ..., T1: Copy + Into<T>, U1: Copy + Into<U>, ...>
      --   MarshalInto<TaggedUnion<T1, U1, ...> for Enum<T, U, ...> { ... }
      impl1 = Impl [] InheritedV Final Normal Positive
                   (mkGenerics fullImplBds) (Just marshalIntoStruct)
                   structTy1' [mkMarshalFunc structTy1' structTy (stmts nStruct' nStruct)] ()
      impl2 = Impl [] InheritedV Final Normal Positive
                   (mkGenerics fullImplBds) (Just marshalIntoStruct')
                   structTy1 [mkMarshalFunc structTy1 structTy' (stmts nStruct nStruct')] ()
  pure (if nStruct /= nStruct' then [impl1, impl2] else [impl1])

-- | Generate the pair of 'MarshalEnum' trait impl's that allow you to convert back and forth between the
-- #[repr(C)] tagged union and the Rust enum.
mkMarshalEnumImpls :: TyVarDict     -- ^ Mapping of Haskell type variables to Rust type parameters
                   -> Ident         -- ^ Tagged union name
                   -> Ident         -- ^ Union name
                   -> [(Ident,Int)] -- ^ Structs that are fields of union (and their arity)
                   -> Ident         -- ^ Enum name
                   -> [Ident]       -- ^ Enum variants
                   -> Q ( Item ()   --   Impl mapping #[repr(C)] tagged union into enum
                        , Item ()   --   Impl mapping enum into tagged union
                        )
mkMarshalEnumImpls dict
            nTagged nUnion nStructs
            nEnum nVariants = do
  
  let -- two copies of type parameters: @T, U, ...@ and @T1, U1, ...@
      (ts, ts1) = unzip [ (TyParam [] i' [] Nothing (), TyParam [] (i' <> "1") [] Nothing ())
                        | (_, TyParam _ (Ident i _ _) _ _ _) <- dict
                        , let i' = mkIdent (map toUpper i)
                        ]

      -- type parameters with @Into@ bound: @T1 + Into<T>, U1 + Into<U>, ...@
      ts1BoundIntoT = zipWith consBound (map (marshalBound . tyParam2Ty) ts) ts1

      -- full parameters: @T: Copy, U: Copy, ..., T1: Copy + Into<T>, U1: Copy + Into<U>, ...@ 
      fullImplBds = map (consBound copyBound) (ts ++ ts1BoundIntoT)

      -- MarshalInto<TaggedUnion<T, U, ...>>
      marshalIntoUnion = mkMarshalInto nTagged ts

      -- Enum<T1, U1, ...> and TaggedUnion<T, U, ...>
      enumTy1 = mkGenPathTy nEnum (map tyParam2Ty ts1)
      taggedTy = mkGenPathTy nTagged (map tyParam2Ty ts)
  
      flds = [ FieldPat Nothing (mkIdentPat "tag") ()
             , FieldPat Nothing (mkIdentPat "payload") ()
             ]
      arms1 = [ mkArm pat body
              | (i, (s, n), v) <- zip3 [0..] nStructs nVariants
              , let pat = LitP (Lit [] (Int Dec i Unsuffixed ()) ()) ()
              , let vars = map (mkIdentPat . mkIdent . ('y' :) . show) [0..(n-1)]
              , let exps = map ( (\y -> MethodCall [] y "marshal" Nothing [] ())
                               . mkPathExpr
                               . mkIdent
                               . ('y' :)
                               . show )
                               [0..(n-1)]
              , let access = FieldAccess [] (mkPathExpr "payload") (mkIdent ("v" ++ show i)) () 
              , let pat2 = [ TupleStructP (mkPath s) vars Nothing () | not (null vars) ] 
              , let stmts = [ Local p Nothing (Just access) [] () | p <- pat2 ] ++
                            [ if n == 0
                                then NoSemi (mkPathExpr' [nEnum, v]) ()
                                else NoSemi (Call [] (mkPathExpr' [nEnum, v]) exps ()) ()
                            ]
              , let safety = if null vars then Normal else Unsafe
              , let body = BlockExpr [] (Block stmts safety ()) ()
              ] ++ [ mkArm (WildP ()) (void [R.expr| panic!("Unexpected tag!") |]) ]
      body1 = [ Local (StructP (mkPath nTagged) flds False ())
                      Nothing
                      (Just (mkPathExpr "self"))
                      []
                      ()
              , NoSemi (Match [] (mkPathExpr "tag") arms1 ())
                       ()
              ]

      -- impl<T: Copy, U: Copy, ..., T1: Copy + Into<T>, U1: Copy + Into<U>, ...>
      --   MarshalInto<TaggedUnion<T1, U1, ...> for Enum<T, U, ...> { ... }
      impl1 = Impl [] InheritedV Final Normal Positive
                   (mkGenerics fullImplBds) (Just marshalIntoUnion)
                   enumTy1 [mkMarshalFunc enumTy1 taggedTy body2] ()

      -- From<Enum<T1, U1, ...>>
      fromEnumTr = mkMarshalInto nEnum ts
      
      -- TaggedUnion<T, U, ...> and Enum<T1, U1, ...>
      taggedTy1 = mkGenPathTy nTagged (map tyParam2Ty ts1)
      enumTy = mkGenPathTy nEnum (map tyParam2Ty ts)

      arms2 = [ mkArm pat body
              | (i, (s, n), v) <- zip3 [0..] nStructs nVariants
              , let vars = map (mkIdentPat . mkIdent . ('y' :) . show) [0..(n-1)]
              , let exps = map ( (\y -> MethodCall [] y "marshal" Nothing [] ())
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
      body2 = [ NoSemi (Match [] (mkPathExpr "self") arms2 ()) () ]

      -- impl<T: Copy, U: Copy, ..., T1: Copy + Into<T>, U1: Copy + Into<U>, ...>
      --   From<Enum<T1, U1, ...> for TaggedUnion<T, U, ...> { ... }
      impl2 = Impl [] InheritedV Final Normal Positive
                   (mkGenerics fullImplBds) (Just fromEnumTr)
                   taggedTy1 [mkMarshalFunc taggedTy1 enumTy body1] ()

      
  pure (impl1, impl2)
    
mkMarshalInto :: Ident -> [TyParam ()] -> TraitRef ()
mkMarshalInto n t = TraitRef (mkGenPath "MarshalInto" [ mkGenPathTy n (map tyParam2Ty t) ])

mkMarshalFunc :: Ty () -> Ty () -> [Stmt ()] -> ImplItem ()
mkMarshalFunc _     retTy body = let gen = mkGenerics []
                                     arg = SelfValue Immutable ()
                                     decl = FnDecl [arg] (Just retTy) False ()
                                     sig = MethodSig Normal NotConst Rust decl 
                                     blk = Block body Normal ()
                                     in MethodI [] InheritedV Final "marshal" gen sig blk ()

-- | Extend the context with the type variables in the given dictionary.
extendCtx :: TyVarDict -> Context -> Q Context
extendCtx dict ctx = dictCtx <> pure ctx
  where
    dictCtx = mkContext [ (mkPathTy i, varT v, False)
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
      outTy = mkGenPathTy itemN [ mkPathTy i' | TyParam _ (Ident i _ _) _ _ _ <- ps
                                              , let i' = mkIdent (map toUpper i)
                                              ]
  
  (varNs, vars) <- fmap unzip $
    for cons $ \(nCon, flds) -> do
      let varN = mkIdent (nameBase nCon)
      varData <- mkVariant ctx flds False
      pure (varN, Variant varN [] (direct varData) Nothing ())

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
      outTy = mkGenPathTy itemN [ mkPathTy i' | TyParam _ (Ident i _ _) _ _ _ <- copyPs
                                              , let i' = mkIdent (map toUpper i)
                                              ]

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
      outTy = mkGenPathTy itemN [ mkPathTy i' | TyParam _ (Ident i _ _) _ _ _ <- copyPs
                                              , let i' = mkIdent (map toUpper i)
                                              ]

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
         -> Name             -- ^ What name to give the reprC struct
         -> [Type]           -- ^ Fields of the struct
         -> Q (TranslatedTy ( Ty ()        --   Output type
                            , (Ident, Int) --   Output name and arity
                            , Item ()      --   Struct definition
                            ))
mkStruct ctx dict copy n1 n2  tys = do
  let itemN1 = mkIdent (nameBase n1)
      itemN2 = mkIdent (nameBase n2)
      haskTysUsed = concatMap getAllVars tys
      copyPs = [ if copy then consBound copyBound typ else typ
               | (ht, typ) <- dict, ht `elem` haskTysUsed
               ]
      mkStructItem as n varD = StructItem as PublicV n varD (mkGenerics copyPs) ()
      mkOutTy n = mkGenPathTy n [ mkPathTy i' | TyParam _ (Ident i _ _) _ _ _ <- copyPs
                                              , let i' = mkIdent (map toUpper i)
                                              ]

  var <- mkVariant ctx tys True
  case var of
    Translated v Nothing  ->
      let si = mkStructItem [reprC, deriveCopyClone] itemN1 v
      in pure (Translated (mkOutTy itemN1, (itemN1, length tys), si) Nothing)
    Translated v (Just r) -> 
      let si = mkStructItem [deriveCopyClone] itemN1 v
          si' = mkStructItem [reprC, deriveCopyClone] itemN2 r
      in pure (Translated (mkOutTy itemN1, (itemN1, length tys), si)
                          (Just (mkOutTy itemN2, (itemN2, length tys), si')))

-- | Construct the variant data for struct fields.
mkVariant :: Context                           -- ^ current context
          -> [Type]                            -- ^ Haskell types
          -> Bool                              -- ^ Whether to make fields public
          -> Q (TranslatedTy (VariantData ())) -- ^ Rust data
mkVariant _ [] _ = pure (Translated (UnitD ()) Nothing)
mkVariant ctx tys pub = do
  let vis = if pub then PublicV else InheritedV 

  -- Regular Rust types corresponding to Haskell ones
  rustTys <- traverse (\ht -> getHTypeInContext ht ctx) tys
  let structFlds  = [ StructField Nothing vis t [] () | t <- rustTys ]
      variantData = TupleD structFlds ()

  -- Intermediate Rust types corresponding to Haskell ones
  rustTys' <- traverse (\rt -> sequence . snd $ getRTypeInContext rt ctx) rustTys
  let needInter    = any isJust rustTys'
      rustInterTys = zipWith fromMaybe rustTys rustTys'
      structFlds'  = [ StructField Nothing vis t [] () | t <- rustInterTys ]
      variantData' | needInter = Just (TupleD structFlds' ())
                   | otherwise = Nothing

  pure (Translated variantData variantData')

