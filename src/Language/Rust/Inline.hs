{-|
Module      : Language.Rust.Inline
Description : Quasiquotes for writing Rust code inline in Haskell
Copyright   : (c) Alec Theriault, 2017
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : GHC
-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Rust.Inline (
  -- * Overview
  --
  -- $overview
  --
  -- * Quasiquoters
  --
  -- $quasiquoters
  --
  -- ** Safe
  --
  -- $safe
  rust,
  rustIO,
  -- ** Unsafe
  --
  -- $unsafe
  rustUnsafe,
  rustUnsafeIO,
  -- ** Interruptible
  --
  -- $interruptible
  rustInterruptible,
  rustInterruptibleIO,
 
  -- * Contexts
  Context(..),
  RType,
  HType,
  -- ** Using and defining contexts
  setContext,
  extendContext,
  singleton,
  mkContext,
  lookupRTypeInContext,
  getRTypeInContext,
  lookupHTypeInContext,
  getHTypeInContext,
  -- ** Built-in contexts
  basic,
  libc,
  ghcUnboxed,
  functions,
  pointers,
  -- ** Marshalling
  with,
  alloca,
  free,
  new,
  withFunPtr,
  newFunPtr,
  freeHaskellFunPtr,
  withArrayLen,
  withStorableArrayLen,
  newArray,
  withByteString,
  unsafeLocalState,
  mkStorable,
  mkReprC,

  -- * Top-level Rust items
  externCrate,
) where

import Language.Rust.Inline.Context 
import Language.Rust.Inline.Internal
import Language.Rust.Inline.Marshal
import Language.Rust.Inline.Parser
import Language.Rust.Inline.Pretty
import Language.Rust.Inline.TH.Storable      ( mkStorable )
import Language.Rust.Inline.TH.ReprC         ( mkReprC )

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Quote             ( QuasiQuoter(..) )


import Foreign.Marshal.Utils                 ( with, new )
import Foreign.Marshal.Alloc                 ( alloca, free ) 
import Foreign.Marshal.Array                 ( withArrayLen, newArray )
import Foreign.Marshal.Unsafe                ( unsafeLocalState )
import Foreign.Ptr                           ( freeHaskellFunPtr, Ptr )

import Control.Monad                         ( void )
import Data.List                             ( intercalate )
import Data.Traversable                      ( for )

-- $overview
--
-- This module provides the facility for dropping in bits of Rust code into your
-- Haskell project.
--
-- ** How it works
--
-- This works by the magic of Template Haskell. In a nutshell, for every Haskell
-- source with a Rust quasiquote in it, a Rust source file is generated. Into
-- this file are added
-- 
--   - all top-level Rust quasiquotes (contents are added in as-is)
--
--   - functions for all expression-level quasiquotes (function arguments
--     correspond to referenced Haskell variables)
--
-- On the Haskell side, every expression quasiquote generates an FFI import
-- to match the generated Rust function and is then replaced with an expression
-- calling that function (passed in as arguments the Haskell variables the
-- quasiquote used).
--
-- The Rust source file is compiled (by `rustc` if there are no extern crates
-- or by `cargo` - dependencies are placed in `.inline-rust-quasi` - if there
-- are). Finally, the resulting static library is passed to GHC through
-- Template Haskell.

-- $quasiquoters
--
-- Rust is (like Haskell) an expression based language, so it is sufficient to
-- make quasiquoters for expressions only. Note that `{ <stmt>; ... }` is a
-- valid Rust block expression.
--
-- As Rust does not distinguish between pure and impure expressions, it is
-- entirely up to the user of this library to use the correct quasiquoter.
-- Quasiquoters with `IO` are meant for impure expressions and the rest are for
-- pure expressions. Incorrectly annotating an impure expression as pure will
-- /not/ cause a compile-time error but may break type safety and referential
-- transparency.


-- $safe
--
-- Safe quasiquoters are the most simple ones. When in doubt and not overly in
-- need of performance, use these.

-- | Safe and pure expression quasiquoter. It is up the user to make sure the
-- Rust expression they use is pure.
--
-- This can also be used in a declaration context to just emit raw Rust code.
-- You can use this to define Rust items that you can use in any quasiquote in
-- the module.
--
-- @
--     rustInc x :: Int32 -> Int32
--     rustInc x = [rust| i32 { 1i32 + $(x: i32) } |]
-- @ 
rust :: QuasiQuoter
rust = rustQuasiQuoter Safe True True

-- | Safe and impure expression quasiquoter. Like 'rust', this can also be used
-- to emit top-level Rust items when used in declaration context.
--
-- @
--     rustHello :: Int32 -> IO ()
--     rustHello n = [rustIO| () { println!("Your number: {}", $(n: i32)) } |]
-- @ 
rustIO :: QuasiQuoter
rustIO = rustQuasiQuoter Safe False True


-- $unsafe
--
-- Unsafe quasiquoters have less overhead than safe ones, but they can have
-- problems if the Rust expression calls back into the Haskell runtime or
-- if the Rust expression blocks indefinitely.
--
-- This [wiki page](wiki.haskell.org/Foreign_Function_Interface#Unsafe_calls)
-- and the [Haskell Report](www.haskell.org/definition/haskell2010.pdf) section
-- on "Import Declarations" detail the caveats of `unsafe`.

-- | Unsafe but pure expression quasiquoter. It is up the user to make sure the
-- Rust expression they use is pure, doesn't block, and doesn't call
-- back into the Haskell runtime.
--
-- Faster, but use with caution.
rustUnsafe :: QuasiQuoter
rustUnsafe = rustQuasiQuoter Unsafe True False

-- | Unsafe and impure expression quasiquoter. It is up the user to make sure
-- the Rust expression they use doesn't block and doesn't call back into the
-- Haskell runtime.
--
-- Faster, but use with caution.
rustUnsafeIO :: QuasiQuoter
rustUnsafeIO = rustQuasiQuoter Unsafe False False


-- $interruptible
--
-- Interruptible quasiquoters are slightly stronger (and slower) than safe ones:
-- they additionally try to make the foreign call promptly return when a
-- 'throwTo' is directed at a thread making the call.
--
-- The [GHC Docs](downloads.haskell.org/~ghc/latest/docs/html/users_guide/ffi-chap.html#interruptible-foreign-calls)
-- detail the behaviour of 'interruptible'.

-- | Interrupt and pure expression quasiquoter. It is up the user to make sure the
-- Rust expression they use is pure.
--
-- Slower, but safer around exception-heavy code.
rustInterruptible :: QuasiQuoter
rustInterruptible = rustQuasiQuoter Interruptible True False

-- | Interrupt and impure expression quasiquoter.
--
-- Slower, but safer around exception-heavy code.
rustInterruptibleIO :: QuasiQuoter
rustInterruptibleIO = rustQuasiQuoter Interruptible False False


-- | Make an expression/declaration quasiquoter.
--
-- For expressions, this packages together the work of parsing the quasiquote
-- contents, generating Haskell FFI bindings, generating and compiling Rust
-- source, then linking in the Rust object file.
--
-- For declarations (if supported), this emits raw code.
rustQuasiQuoter :: Safety      -- ^ safety of FFI
                -> Bool        -- ^ purity of FFI
                -> Bool        -- ^ support declarations
                -> QuasiQuoter
rustQuasiQuoter safety isPure supportDecs = QuasiQuoter { quoteExp = expQuoter
                                                        , quotePat = err 
                                                        , quoteType = err
                                                        , quoteDec = decQuoter 
                                                        }
  where
    who | supportDecs = "expressions and declarations"
        | otherwise   = "expressions"

    err = fail ("(inline-rust): Only " ++ who ++ " can be quasiquoted")

    expQuoter qq = do
      parsed <- parseQQ qq
      processQQ safety isPure parsed

    decQuoter | supportDecs = emitCodeBlock
              | otherwise = err


-- | This function sums up the packages. What it does:
--
--    1. Map the Rust type annotations in the quasiquote to their Haskell types.
--
--    2. Generate a Haskell FFI signature for passing all the captured Haskell
--       variables to Rust.
--
--    3. Generate a Rust function compatible with the Haskell FFI, and emit it
--       to a temporary file (the same file is kept for the whole module)
--
--    4. Generate and return a Haskell call to this function, slotting in the
--       right Haskell arguments.
--
processQQ :: Safety -> Bool -> RustQuasiquoteParse -> Q Exp
processQQ safety isPure (QQParse rustRet rustBody rustNamedArgs) = do

  -- Make a name to thread through Haskell/Rust (see Trac #13054)
  qqName' <- newName "quasiquote"
  qqName <- newName (show qqName')
  let qqStrName = show qqName

  -- Find out what the corresponding Haskell representations are for the
  -- argument and return types
  let (rustArgNames, rustArgs) = unzip rustNamedArgs 
  (haskRet, reprCRet) <- getRType (void rustRet)
  (haskArgs, reprCArgs) <- unzip <$> traverse (getRType . void) rustArgs

  -- Convert the Haskell return type to a marshallable FFI type
  (retByVal, haskRet') <- do
    marshallable <- ghcMarshallable haskRet
    if marshallable
      then pure (True, [t| IO $(pure haskRet) |])
      else pure (False, [t| Ptr $(pure haskRet) -> IO () |])
    
  -- Convert the Haskell arguments to marshallable FFI types
  (argsByVal, haskArgs') <- fmap unzip $
    for haskArgs $ \haskArg -> do
      marshallable <- ghcMarshallable haskArg
      if marshallable
        then pure (True, haskArg)
        else do ptr <- [t| Ptr $(pure haskArg) |]
                pure (False, ptr)

  -- Generate the Haskell FFI import declaration and emit it
  haskSig <- foldr (\l r -> [t| $(pure l) -> $r |]) haskRet' haskArgs'
  let ffiImport = ForeignD (ImportF CCall safety qqStrName qqName haskSig)
  addTopDecls [ffiImport]

  -- Generate the Haskell FFI call
  let goArgs :: [Q Exp] -> [(String, Bool)] -> Q Exp

      -- Once we run out of arguments we call the quasiquote function with all the
      -- accumulated arguments. If the return value is not marshallable, we have to 
      -- 'alloca' some space to put the return value.
      goArgs acc [] | retByVal = appsE (varE qqName : reverse acc)
                    | otherwise = do
                        ret <- newName "ret"
                        [e| alloca (\( $(varP ret) ) ->
                              do { $(appsE (varE qqName : reverse (varE ret : acc)))
                                 ; peek $(varE ret)
                                 }) |]
     
      -- If an argument is by value, we just stack it into the accumulated arguments.
      -- Otherwise, we use 'with' to get a pointer to its stack position.
      goArgs acc ((argStr, byVal) : args) = do
        arg <- lookupValueName argStr
        case arg of
          Nothing -> fail ("Could not find Haskell variable `" ++ argStr ++ "'")
          Just argName | byVal -> goArgs (varE argName : acc) args
                       | otherwise -> do
                          x <- newName "x"
                          [e| with $(varE argName) (\( $(varP x) ) ->
                                $(goArgs (varE x : acc) args)) |]

  let haskCallIO = goArgs [] (rustArgNames `zip` argsByVal)
      haskCall | isPure = [e| unsafeLocalState $haskCallIO |]
               | otherwise = haskCallIO

  -- Generate the Rust function arguments and the converted arguments
  let (rustArgs', rustConvertedArgs) = unzip $ zipWith mergeArgs rustArgs reprCArgs
      (rustRet', rustConvertedRet) = mergeArgs rustRet reprCRet
      
     -- mergeArgs :: Ty Span -> Maybe RType -> (Ty Span, Ty Span)
      mergeArgs t Nothing       = (t, t)
      mergeArgs t (Just tInter) = (fmap (const mempty) tInter, t)
  
  -- Generate the Rust function.
  let (retArg, retTy, ret)
         | retByVal  = ( []
                       , renderType rustRet'
                       , "out.marshal()"
                       )
         | otherwise = ( ["ret_" ++ qqStrName ++ ": *mut " ++ renderType rustRet']
                       , "()"
                       , "unsafe { std::ptr::write(ret_" ++ qqStrName ++ ", out.marshal()) }"
                       )
  void . emitCodeBlock . unlines $
    [ "#[no_mangle]"
    , "pub extern \"C\" fn " ++ qqStrName ++ "("
    , "  " ++ intercalate ", " ([ s ++ ": " ++ marshal (renderType t)
                                | (s,t,v) <- zip3 rustArgNames rustArgs' argsByVal
                                , let marshal x = if v then x else "*const " ++ x
                                ] ++ retArg)
    , ") -> " ++ retTy ++ " {"
    , unlines [ "  let " ++ s ++ ": " ++ renderType t ++ " = " ++ marshal s ++ ".marshal();"
              | (s,t,v) <- zip3 rustArgNames rustConvertedArgs argsByVal
              , let marshal x = if v then x else "unsafe { std::ptr::read(" ++ x ++ ") }"
              ]
    , "  let out: " ++ renderType rustConvertedRet ++ " = " ++ renderTokens rustBody ++ ";"
    , "  " ++ ret
    , "}"
    ]

  -- Return the Haskell call to the FFI import
  haskCall



