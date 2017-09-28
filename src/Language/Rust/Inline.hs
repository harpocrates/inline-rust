module Language.Rust.Inline where

import Language.Rust.Inline.Context
import Language.Rust.Inline.Parser

import Language.Haskell.TH

-- | Generates the C and Rust files
processQQ :: Context -> RustQuasiquoteParse -> Q Exp
processQQ con (QQParse rustRet b haskArgsStr) = do

  -- Find out what the corresponding Rust/C representations are for the return type
  let (cRet, haskRet) = lookupRust rustRet con
