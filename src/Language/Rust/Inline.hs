{-# LANGUAGE ForeignFunctionInterface, QuasiQuotes, TemplateHaskell, BangPatterns #-}

module Language.Rust.Inline where

import Language.Rust.Inline.Context
import Language.Rust.Inline.Parser

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote

import Data.List
import Data.Traversable (for)
import Control.Monad (when)

import Language.Rust.Pretty
import Language.Rust.Syntax

import Data.Text.Prettyprint.Doc (layoutPretty, defaultLayoutOptions)
import Data.Text.Prettyprint.Doc.Render.String (renderString)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import System.IO.Temp

import System.Process
import System.Exit

import System.Directory (removeFile)

exp :: QuasiQuoter
exp = QuasiQuoter
  { quoteExp = expQuoter Safe True
  , quotePat = fail "Only expressions can be quasiquoted"
  , quoteType = fail "Only expressions can be quasiquoted"
  , quoteDec = fail "Only expressions can be quasiquoted"
  }

expEffects :: QuasiQuoter
expEffects = QuasiQuoter
  { quoteExp = expQuoter Safe False
  , quotePat = fail "Only expressions can be quasiquoted"
  , quoteType = fail "Only expressions can be quasiquoted"
  , quoteDec = fail "Only expressions can be quasiquoted"
  }

-- | Make a quasiquoter
expQuoter :: Safety -> Bool -> String -> Q Exp
expQuoter safety isPure qq = do
  parsed <- parseQQ qq
  context <- getContext
  processQQ context safety isPure parsed

-- | Generates the C and Rust files
processQQ :: Context -> Safety -> Bool -> RustQuasiquoteParse -> Q Exp
processQQ con safety isPure (QQParse rustRet rustBody rustArgs) = do

  -- Make a name to thread through Haskell/C/Rust
  qqName <- newName "quasiquote"
  let qqStrName = show qqName

  -- Find out what the corresponding Haskell/C representations are for the arg and return types
  haskRet <- lookupType rustRet con
  haskArgs <- traverse (\(_, rustArg) -> lookupType rustArg con) rustArgs

  -- Generate the Haskell FFI import
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

      rustSrc = concat [ "#[no_mangle]\nextern \"C\" fn ", qqStrName, "("
                       , intercalate ", " (map (\(s,t) -> s ++ ": " ++ render t) rustArgs)
                       , ") -> ", render rustRet
                       , render (Stream (map (Tree . Token mempty) rustBody))
                       ]
  addForeignRustFile rustSrc

  pure haskCall


addForeignRustFile :: String -> Q ()
addForeignRustFile rustSrc = addForeignObject =<< runIO staticLib 
  where staticLib :: IO ByteString
        staticLib = do
          -- Make input/output files
          fpIn <- writeTempFile "." "quasiquote.rs" rustSrc
          fpOut <- emptyTempFile "." "quasiquote.a"
          
          -- Call `rustc`
          (ec, _, stderr) <- readProcessWithExitCode "rustc" ["--crate-type=staticlib", fpIn, "-o", fpOut] ""
          when (ec /= ExitSuccess) $ do
            fail ("Rust source in quasiquote failed to compile:\n" ++ stderr)

          -- Read in linkable library output
          !obj <- BS.readFile fpOut

          -- Clean up
          removeFile fpIn
          removeFile fpOut

          -- Return the binary from the library
          pure obj

-- Proposed in <https://phabricator.haskell.org/D4064>
addForeignObject :: ByteString -> Q ()
addForeignObject _ = pure () 

