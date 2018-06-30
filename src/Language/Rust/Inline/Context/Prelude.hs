{-|
Module      : Language.Rust.Inline.Context.Prelude
Description : Defines contexts for Prelude types
Copyright   : (c) Alec Theriault, 2017
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : GHC
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -w #-}

module Language.Rust.Inline.Context.Prelude where

import Language.Rust.Inline.Context
import Language.Rust.Inline.TH

import Language.Rust.Data.Ident            ( Ident(..), mkIdent )

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Language.Rust.Syntax

import Foreign.Storable

import Control.Monad ( join )
import Data.List     ( intercalate )

-- Some 'Storable' instances
--
--   * 'Maybe'
--   * 'Either'
--   * Tuples up and including to arity 16
--
-- Note that arity 0 is in 'Foreign.Storable' and arity 1 makes no sense in Haskell.
mkStorable [t| forall a. Storable a => Storable (Maybe a) |]
mkStorable [t| forall l r. (Storable l, Storable r) => Storable (Either l r) |]
fmap join (traverse mkTupleStorable [2..16])

-- | Make a generic path type (e.g. something like @Vec<T>@).
mkGenPathTy :: Ident -> [Ty ()] -> Ty ()
mkGenPathTy i g = PathTy Nothing (mkGenPath i g) ()

-- | Make a generic path from an idenitifer
mkGenPath :: Ident -> [Ty ()] -> Path ()
mkGenPath i [] = Path False [ PathSegment i Nothing () ] ()
mkGenPath i g = Path False [ PathSegment i (Just (AngleBracketed [] g [] ())) () ] ()

-- | Prelude context, includes 'Maybe', 'Either', and tuples (of arity up to 16)
prelude :: Q Context
prelude = maybeContext <> eitherContext <> mconcat [ tupleContext i | i <- [2..16] ]


-- | Context for 'Maybe' type
maybeContext :: Q Context
maybeContext = do
    maybeConT <- [t| Maybe |]
    pure (Context ([rule],[rev maybeConT],maybeItems))
  where
  rule pt context = do
    PathTy Nothing (Path False [PathSegment "Option" (Just (AngleBracketed [] [t] [] _)) _] _) _ <- pure pt
    (t', rInterOpt) <- lookupRTypeInContext t context
    let inter = mkGenPathTy "MaybeC" <$> ((\x -> [x]) <$> maybe (pure t) id rInterOpt)
    pure ([t| Maybe $t' |], Just inter)


  rev maybeConT pt context = do
    AppT maybeCon t <- pure pt
    if maybeCon /= maybeConT
      then mempty
      else do
        t' <- lookupHTypeInContext t context
        pure (fmap (\x -> PathTy Nothing (Path False [PathSegment "Option" (Just (AngleBracketed [] [x] [] ())) ()] ()) ()) t')

-- | Context for 'Either' type
eitherContext :: Q Context
eitherContext = do
    eitherConT <- [t| Either |]
    pure (Context ([rule],[rev eitherConT],eitherItems))
  where
  rule pt context = do
    PathTy Nothing (Path False [PathSegment "Result" (Just (AngleBracketed [] [l,r] [] _)) _] _) _ <- pure pt
    (l', lInter) <- lookupRTypeInContext l context
    (r', rInter) <- lookupRTypeInContext r context
    let inter = mkGenPathTy "EitherC" <$> ((\x y -> [x,y]) <$> maybe (pure r) id rInter
                                                           <*> maybe (pure l) id lInter)
    pure ([t| Either $r' $l' |], Just inter)

  rev eitherConT pt context = do
    AppT (AppT eitherCon l) r <- pure pt
    if eitherCon /= eitherConT
      then mempty
      else do
        l' <- lookupHTypeInContext l context
        r' <- lookupHTypeInContext r context
        pure ((\x y -> PathTy Nothing (Path False [PathSegment "Result" (Just (AngleBracketed [] [x,y] [] ())) ()] ()) ()) <$> r' <*> l')

-- | Context for tuple types
tupleContext :: Int -> Q Context
tupleContext n = pure (Context ([rule],[rev],tupleItems n))
  where
  rule pt ctx = do
    TupTy tys _ <- pure pt

    -- Filter out incorrect tuple types
    () <- if length tys == n then pure () else fail "Wrong sized tuple"

    (tys', tysInter) <- fmap unzip $ traverse (`lookupRTypeInContext` ctx) tys

    let tysGen = zipWith (\x m -> maybe (pure x) id m) tys tysInter
        inter = mkGenPathTy (mkIdent ("Tuple" ++ show n)) <$> sequence tysGen
    pure (foldl appT (tupleT n) tys', Just inter)

  rev pt ctx = do
    Just tys <- pure (getTupTy [] pt)

    -- Look up parameters recursively
    tys' <- traverse (`lookupHTypeInContext` ctx) tys

    -- Compute the Rust type
    pure (TupTy <$> sequence tys' <*> pure ())

  getTupTy acc (TupleT n) | length acc == n = Just acc
  getTupTy acc (AppT t1 t2) = getTupTy (t2:acc) t1
  getTupTy acc (ParensT t) = getTupTy acc t
  getTupTy _ _ = Nothing


maybeItems :: [String]
maybeItems = map unlines
    -- Tagged
  [ [ "#[repr(C)]"
    , "#[derive(Copy,Clone)]"
    , "pub struct MaybeC<T: Copy> {"
    , "  tag: u8,"
    , "  payload: TaggedMaybeC<T>,"
    , "}"
    ]
    -- Payload
  , [ "#[repr(C)]"
    , "#[derive(Copy,Clone)]"
    , "union TaggedMaybeC<T: Copy> {"
    , "  nothing: NothingC,"
    , "  just: JustC<T>,"
    , "}"
    ]
    -- Variants
  , [ "#[repr(C)]"
    , "#[derive(Copy,Clone)]"
    , "struct NothingC;"
    , "#[repr(C)]"
    , "#[derive(Copy,Clone)]"
    , "struct JustC<T>(T);"
    ]
    -- impl MarshalInto<MaybeC<T>> for Option<T1>
  , [ "impl<T: Copy, T1:  MarshalInto<T>> MarshalInto<MaybeC<T>> for Option<T1> {"
    , "  fn marshal(self) -> MaybeC<T> {"
    , "    match self {"
    , "      None => MaybeC { tag: 0, payload: TaggedMaybeC { nothing: NothingC } },"
    , "      Some(t) => MaybeC { tag: 1, payload: TaggedMaybeC { just: JustC(t.marshal()) } },"
    , "    }"
    , "  }"
    , "}"
    ]
    -- impl MarshalInto<Option<T>> for MaybeC<T1>
  , [ "impl<T, T1: MarshalInto<T> + Copy> MarshalInto<Option<T>> for MaybeC<T1> {"
    , "  fn marshal(self) -> Option<T> {"
    , "    match self.tag {"
    , "      0 => None,"
    , "      1 => unsafe { let JustC(t) = self.payload.just; Some(t.marshal()) }"
    , "      i => panic!(\"The tag {} is invalid for 'MaybeC'\", i),"
    , "    }"
    , "  }"
    , "}"
    ]
  ]


eitherItems :: [String]
eitherItems = map unlines
    -- Tagged
  [ [ "#[repr(C)]"
    , "#[derive(Copy,Clone)]"
    , "pub struct EitherC<L: Copy, R: Copy> {"
    , "  tag: u8,"
    , "  payload: TaggedEitherC<L,R>,"
    , "}"
    ]
    -- Payload
  , [ "#[repr(C)]"
    , "#[derive(Copy,Clone)]"
    , "union TaggedEitherC<L: Copy, R: Copy> {"
    , "  left: LeftC<L>,"
    , "  right: RightC<R>,"
    , "}"
    ]
    -- Variants
  , [ "#[repr(C)]"
    , "#[derive(Copy,Clone)]"
    , "struct LeftC<L>(L);"
    , "#[repr(C)]"
    , "#[derive(Copy,Clone)]"
    , "struct RightC<T>(T);"
    ]
    -- impl MarshalInto<EitherC<L,R>> for Result<L,R>
  , [ "impl<L: Copy, L1:  MarshalInto<L> + Copy, R: Copy, R1: MarshalInto<R> + Copy> MarshalInto<EitherC<L,R>> for Result<R1,L1> {"
    , "  fn marshal(self) -> EitherC<L,R> {"
    , "    match self {"
    , "      Err(l) => EitherC { tag: 0, payload: TaggedEitherC { left: LeftC(l.marshal()) } },"
    , "      Ok(r) => EitherC { tag: 1, payload: TaggedEitherC { right: RightC(r.marshal()) } },"
    , "    }"
    , "  }"
    , "}"
    ]
    -- impl MarshalInto<Result<L,R>> for EitherC<L,R>
  , [ "impl<L, L1: MarshalInto<L> + Copy, R, R1: MarshalInto<R> + Copy> MarshalInto<Result<R,L>> for EitherC<L1,R1> {"
    , "  fn marshal(self) -> Result<R,L> {"
    , "    match self.tag {"
    , "      0 => unsafe { let LeftC(l) = self.payload.left; Err(l.marshal()) },"
    , "      1 => unsafe { let RightC(r) = self.payload.right; Ok(r.marshal()) }"
    , "      i => panic!(\"The tag {} is invalid for 'EitherC'\", i),"
    , "    }"
    , "  }"
    , "}"
    ]
  ]


-- | Given the arity of a tuple, produce a corresponding Rust intermediate
-- @#[repr(C)]@ struct type, along with the @MarshalInto@ type.
tupleItems :: Int -> [String]
tupleItems n = map unlines 
    -- Struct declaration for intermediate type
  [ [ "#[repr(C)]"
    , "#[derive(Debug,Clone,Copy)]"
    , "pub struct " ++ reprTupleTy ts ++ tuple ts ++ ";"
    ]
    -- impl MarshalInto<TupleN<...>> for (...)
  , [ "impl<" ++ params ++ "> MarshalInto<" ++ reprTupleTy ts ++ "> for " ++ tuple ts1 ++ " {"
    , "  fn marshal(self) -> " ++ reprTupleTy ts ++ "{"
    , "    let " ++ tuple xs ++ " = self;"
    , "    " ++ reprTuple [ x ++ ".marshal()" | x <- xs ] 
    , "  }"
    , "}"
    ]
    -- impl MarshalInto<(...)> fof TupleN<...>
  , [ "impl<" ++ params ++ "> MarshalInto<" ++ tuple ts ++ "> for " ++ reprTupleTy ts1 ++ " {"
    , "  fn marshal(self) -> " ++ tuple ts ++ "{"
    , "    let " ++ reprTuple xs ++ " = self;"
    , "    " ++ tuple [ x ++ ".marshal()" | x <- xs ] 
    , "  }"
    , "}"
    ]
  ]
  where
    -- Type parameters
    ts, ts1 :: [String]
    ts  = take n [ l : i | i <- "" : map show [(1 :: Int)..], l <- ['A'..'Z'] ]
    ts1 = [ 'C' : t | t <- ts ]
    
    -- Full type parameters given to either @impl@
    params = intercalate ", " (ts ++ zipWith (\t1 t -> t1 ++ ": MarshalInto<" ++ t ++ ">") ts1 ts )

    -- Variables
    xs = take n [ 'x' : show i | i <- [(0 :: Int)..] ]
    
    -- Construct type/value tuples and their @#[repr(C)]@ equivalents
    tuple vs = "(" ++ intercalate ", " vs ++ ")"
    reprTuple vs = "Tuple" ++ show n ++ "(" ++ intercalate ", " vs ++ ")"
    reprTupleTy vs = "Tuple" ++ show n ++ "<" ++ intercalate ", " vs ++ ">"
   
