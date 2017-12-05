{-|
Module      : Language.Rust.Inline.Pretty
Description : Utility functions for pretty-printing
Copyright   : (c) Alec Theriault, 2017
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : GHC
-}

module Language.Rust.Inline.Pretty (
  renderType,
  renderTokens,
) where

import Language.Rust.Pretty                    ( Pretty(..) )
import Language.Rust.Data.Position             ( Spanned(..) )
import Language.Rust.Syntax                    ( Ty, Token(..), TokenTree(..), TokenStream(..) )

import Data.Text.Prettyprint.Doc               ( layoutPretty, defaultLayoutOptions )
import Data.Text.Prettyprint.Doc.Render.String ( renderString )

-- | Render a something that is 'Pretty' into a 'String'
render :: Pretty a => a -> String
render = renderString . layoutPretty defaultLayoutOptions . prettyUnresolved

-- | Render a Rust type into a 'String'.
renderType :: Ty a -> String
renderType = render

-- | Render a sequence of Rust 'Token's into a 'String'.
renderTokens :: [Spanned Token] -> String
renderTokens toks = render (Stream [ Tree (Token t s) |  Spanned s t <- toks ])
