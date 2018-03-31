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

module Language.Rust.Inline.Context.Prelude where

import Language.Rust.Inline.Context
import Language.Rust.Inline.TH

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Language.Rust.Syntax

import Foreign.Storable

import Data.List ( intercalate )

-- | Context for 'Maybe' type
maybeContext :: Q Context
maybeContext = do
    addTopDecls =<< mkStorable [t| forall a. Storable a => Storable (Maybe a) |]
    maybeConT <- [t| Maybe |]
    pure (Context ([rule],[rev maybeConT],maybeItems))
  where
  rule pt context = do
    PathTy Nothing (Path False [PathSegment "Option" (Just (AngleBracketed [] [t] [] _)) _] _) _ <- pure pt
    (t', Nothing) <- lookupRTypeInContext t context
    pure ([t| Maybe $t' |], Nothing)

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
    addTopDecls =<< mkStorable [t| forall l r. (Storable l, Storable r) => Storable (Either l r) |]
    eitherConT <- [t| Either |]
    pure (Context ([rule],[rev eitherConT],eitherItems))
  where
  rule pt context = do
    PathTy Nothing (Path False [PathSegment "Result" (Just (AngleBracketed [] [l,r] [] _)) _] _) _ <- pure pt
    (l', Nothing) <- lookupRTypeInContext l context
    (r', Nothing) <- lookupRTypeInContext r context
    pure ([t| Either $l' $r' |], Nothing)

  rev eitherConT pt context = do
    AppT (AppT eitherCon l) r <- pure pt
    if eitherCon /= eitherConT
      then mempty
      else do
        l' <- lookupHTypeInContext l context
        r' <- lookupHTypeInContext r context
        pure ((\x y -> PathTy Nothing (Path False [PathSegment "Option" (Just (AngleBracketed [] [x,y] [] ())) ()] ()) ()) <$> l' <*> r')

maybeItems :: [String]
maybeItems = map unlines
    -- Tagged
  [ [ "#[repr(C)]"
    , "#[derive(Debug,Clone)]"
    , "struct MaybeC<T> {"
    , "  tag: u8,"
    , "  payload: TaggedMaybeC<T>,"
    , "}"
    ]
    -- Payload
  , [ "union TaggedMaybeC<T: Copy> {"
    , "  nothing: NothingC,"
    , "  just: JustC<T>,"
    , "}"
    ]
    -- Variants
  , [ "#[repr(C)]"
    , "#[derive(Debug,Clone)]"
    , "struct NothingC;"
    , "#[repr(C)]"
    , "#[derive(Debug,Clone)]"
    , "struct JustC<T>(T);"
    ]
    -- impl MarshalInto<MaybeC<T>> for Option<T1>
  , [ "impl<T, T1:  MarshalInto<T>> MarshalInto<MaybeC<T>> for Option<T1> {"
    , "  fn marshal(self) -> MaybeC<T> {"
    , "    match self {"
    , "      None => MaybeC { tag: 0, payload: TaggedMaybeC { nothing: NothingC } },"
    , "      Some(t) => MaybeC { tag: 1, payload: TaggedMaybeC { just: JustC(t) } },"
    , "    }"
    , "  }"
    , "}"
    ]
    -- impl MarshalInto<Option<T>> for MaybeC<T1>
  , [ "impl<T, T1: MarshalInto<T>> MarshalInto<Option<T>> for MaybeC<T1> {"
    , "  fn marshal(self) -> Option<T> {"
    , "    match self.tag {"
    , "      0 => None,"
    , "      1 => unsafe { let JustC(t) = self.payload.just; Some(t) }"
    , "    }"
    , "  }"
    , "}"
    ]
  ]


eitherItems :: [String]
eitherItems = map unlines
    -- Tagged
  [ [ "#[repr(C)]"
    , "#[derive(Debug,Clone)]"
    , "struct EitherC<L,R> {"
    , "  tag: u8,"
    , "  payload: TaggedEitherC<L,R>,"
    , "}"
    ]
    -- Payload
  , [ "union TaggedEitherC<L: Copy, R: Copy> {"
    , "  left: LeftC<L>,"
    , "  right: RightC<R>,"
    , "}"
    ]
    -- Variants
  , [ "#[repr(C)]"
    , "#[derive(Debug,Clone)]"
    , "struct LeftC<L>(L);"
    , "#[repr(C)]"
    , "#[derive(Debug,Clone)]"
    , "struct RightC<T>(T);"
    ]
    -- impl MarshalInto<EitherC<L,R>> for Result<L,R>
  , [ "impl<L, L1:  MarshalInto<L>, R, R1: MarshalInto<R>> MarshalInto<EitherC<L,R>> for Result<L1,R1> {"
    , "  fn marshal(self) -> EitherC<L,R> {"
    , "    match self {"
    , "      Ok(l) => EitherC { tag: 0, payload: TaggedEitherC { left: LeftC(l) } },"
    , "      Err(r) => EitherC { tag: 1, payload: TaggedEitherC { right: RightC(r) } },"
    , "    }"
    , "  }"
    , "}"
    ]
    -- impl MarshalInto<Result<L,R>> for EitherC<L,R>
  , [ "impl<L, L1: MarshalInto<L>, R, R1: MarshalInto<R>> MarshalInto<Result<L,R>> for EitherC<L1,R1> {"
    , "  fn marshal(self) -> Result<L,R> {"
    , "    match self.tag {"
    , "      0 => unsafe { let LeftC(l) = self.payload.left; Ok(l) },"
    , "      1 => unsafe { let RightC(t) = self.payload.right; Err(r) }"
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
    , "#[derive(Debug,Clone)]"
    , "struct Tuple" ++ show n ++
        "<" ++ intercalate ", " ts ++ ">" ++
        "(" ++ intercalate ", " ts ++ ")" ++ ";"
    ]
    -- impl MarshalInto<TupleN<...>> for (...)
  , [ "impl<" ++ params ++ "> MarshalInto<" ++ reprTuple ts ++ "> for " ++ tuple ts1 ++ " {"
    , "  fn marshal(self) -> " ++ reprTuple ts ++ "{"
    , "    let " ++ tuple xs ++ " = self;"
    , "    " ++ reprTuple [ x ++ ".marshal()" | x <- xs ] 
    , "  }"
    , "}"
    ]
    -- impl MarshalInto<(...)> fof TupleN<...>
  , [ "impl<" ++ params ++ "> MarshalInto<" ++ tuple ts ++ "> for " ++ reprTuple ts1 ++ " {"
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
    ts  = take n [ l : i | i <- "" : map show [(1 :: Int)..], l <- ['a'..'z'] ]
    ts1 = [ 'c' : t | t <- ts ]
    
    -- Full type parameters given to either @impl@
    params = intercalate ", " (ts ++ zipWith (\t1 t -> t1 ++ ": MarshalInto<" ++ t ++ ">") ts1 ts )

    -- Variables
    xs = take n [ 'x' : show i | i <- [(0 :: Int)..] ]
    
    -- Construct type/value tuples and their @#[repr(C)]@ equivalents
    tuple vs = "(" ++ intercalate ", " vs ++ ")"
    reprTuple vs = "Tuple" ++ show n ++ "(" ++ intercalate ", " vs ++ ")"
   
