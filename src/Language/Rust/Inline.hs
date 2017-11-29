{-|
Module      : Language.Rust.Inline
Description : QuasiQuotes for writing Rust code inline in Haskell
Copyright   : (c) Alec Theriault, 2017
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : GHC
-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Rust.Inline (
  -- $overview
  rust,
  rustIO,
  rustUnsafe,
  rustUnsafeIO,
  setContext,
  Context,
  module Language.Rust.Inline.Context, 
) where

import Language.Rust.Inline.Context 
import Language.Rust.Inline.Internal
import Language.Rust.Inline.Parser

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote               ( QuasiQuoter(..) )

import Control.Monad                           ( void )
import Data.List                               ( intercalate )
import Data.Traversable                        ( for )

import Language.Rust.Pretty                    ( Pretty(..) )
import Language.Rust.Syntax                    ( TokenStream(..), TokenTree(..) )

import Data.Text.Prettyprint.Doc               ( layoutPretty, defaultLayoutOptions )
import Data.Text.Prettyprint.Doc.Render.String ( renderString )


{- TODO:
 -
 -  * dumping in literal Rust code in the top-level
 -  * related to the above, support adding crate dependencies
 -
 -}

-- $overview
-- 
-- This module provides Template Haskell quasiquotes for using into Rust code
-- inline in Haskell code.
--
-- Since Rust is (like Haskell) an expression based language, it is sufficient
-- to make quasiquoters for expressions only. Note that `{ <stmt>; ... }` is a
-- valid Rust block expression.


-- | Quasiquoter for FFI into pure Rust expressions. It is up the user to make
-- sure the Rust expression they use really _is_ pure.
--
-- @
--     rustInc x :: Int32 -> Int32
--     rustInc x = [rust| i32 { 1i32 + $(x: i32) } |]
-- @ 
rust :: QuasiQuoter
rust = rustQuasiQuoter Safe True

-- | Quasiquoter for FFI into impure Rust expressions.
--
-- @
--     rustHello :: IO ()
--     rustHello = [rustIO| () { println!("Hello from Rust") } |]
-- @ 
rustIO :: QuasiQuoter
rustIO = rustQuasiQuoter Safe False

-- | Like 'rust', but for Rust expressions also guaranteed not to call back
-- into the initial Haskell runtime (see 'unsafe' Template Haskell foreign
-- imports).
--
-- Faster, but use with caution.
rustUnsafe :: QuasiQuoter
rustUnsafe = rustQuasiQuoter Unsafe True

-- | Like 'rustIO', but for Rust expressions also guaranteed not to call back
-- into the initial Haskell runtime (see 'unsafe' Template Haskell foreign
-- imports).
--
-- Faster, but use with caution.
rustUnsafeIO :: QuasiQuoter
rustUnsafeIO = rustQuasiQuoter Unsafe True


-- | Make an expression quasiquoter. This packages together the work of parsing
-- the quasiquote contents, generating Haskell FFI bindings, generating and
-- compiling Rust source, then linking in the Rust object file.
rustQuasiQuoter :: Safety -> Bool -> QuasiQuoter
rustQuasiQuoter safety isPure = QuasiQuoter
  { quoteExp = expQuoter
  , quotePat = fail "Only expressions can be quasiquoted"
  , quoteType = fail "Only expressions can be quasiquoted"
  , quoteDec = fail "Only expressions can be quasiquoted"
  }
  where
  expQuoter qq = do
    parsed <- parseQQ qq
    processQQ safety isPure parsed


-- | Generate the Haskell FFI bindings, emit the Rust source code to the
-- 'ModuleState', and return a call to the FFI import.
processQQ :: Safety -> Bool -> RustQuasiquoteParse -> Q Exp
processQQ safety isPure (QQParse rustRet rustBody rustArgs) = do

  -- Make a name to thread through Haskell/C/Rust
  qqName <- newName "quasiquote"
  let qqStrName = show qqName

  -- Find out what the corresponding Haskell representations are for the
  -- argument and return types
  haskRet <- lookupType (void rustRet)
  haskArgs <- traverse (\(_, rustArg) -> lookupType (void rustArg)) rustArgs

  -- Generate the Haskell FFI import declaration and emit it
  haskSig <- foldr (\l r -> [t| $(pure l) -> $r |])
                   (if isPure then pure haskRet else [t| IO $(pure haskRet) |])
                   haskArgs
  let ffiImport = ForeignD (ImportF CCall safety qqStrName qqName haskSig)
  addTopDecls [ffiImport]

  -- Generate the Haskell FFI call
  haskArgsE <- for rustArgs $ \(argStr, _) -> do 
                 arg <- lookupValueName argStr
                 case arg of
                   Nothing -> fail ("could not find Haskell variable `" ++ argStr ++ "'")
                   Just argName -> pure (VarE argName)
  let haskCall = foldl AppE (VarE qqName) haskArgsE

  -- Generate the Rust function
  let render :: Pretty a => a -> String
      render = renderString . layoutPretty defaultLayoutOptions . prettyUnresolved

      rustSrc = concat [ "#![feature(libc)]"
                       , "extern crate libc;"
                       , "#[no_mangle]\npub extern \"C\" fn ", qqStrName, "("
                       , intercalate ", " (map (\(s,t) -> s ++ ": " ++ render t) rustArgs)
                       , ") -> ", render rustRet
                       , render (Stream (map (Tree . Token mempty) rustBody))
                       ]
  emitCodeBlock rustSrc

  -- Return the Haskell call to the FFI import
  pure haskCall



