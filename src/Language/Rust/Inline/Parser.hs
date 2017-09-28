module Language.Rust.Inline.Parser where

import qualified Language.Rust.Syntax as Rust
import qualified Language.Rust.Parser as Rust
import qualified Language.Rust.Data.Position as Rust

import Language.Haskell.TH

import Data.String (fromString)
import Control.Monad.Trans.Writer

-- | Result of parsing a quasiquote. Quasiquotes are of the form `<ty> <block>` where the `<block>`
-- possibly contains `$<ident>` to unescape. 
data RustQuasiquoteParse = QQParse
  { ty :: Rust.Ty Rust.Span                       -- The leading type
  , body :: [Rust.Token]                          -- The body tokens, with `$(<ident>: <ty>)` pairs replaced by just `<ident>`
  , variables :: [(String, Rust.Ty Rust.Span)]    -- The `$(<ident>: <ty>)` args to escape
  }

-- | Parse an inline Rust quasiquote
parseQQ :: String -> Q RustQuasiquoteParse
parseQQ input = do

  -- Start by tokenizing everything
  spToks <- case Rust.execParser (Rust.lexTokens Rust.lexNonSpace) (fromString input) Rust.initPos of
              Left (_, msg) -> fail msg
              Right spToks' -> pure spToks'

  -- TODO: This is an approximate way to split up the type and body. It isn't foolproof though,
  -- since types can contain macros which _can_ be of the form `HList!{i32, (), u64}` (although most
  -- people usually use brackets for type-macros `HList![i32, (), u64]).
  let (tyToks, bodyToks) = span (\t -> case t of { Rust.Spanned (Rust.OpenDelim Rust.Brace) _ -> True; _ -> False }) spToks

  -- Parse the type
  ty <- case Rust.execParserFromTokens parser tyToks initPos of
          Left (_, msg) -> fail msg
          Right ty' -> pure ty'

  ---WIP from here on
  --
  -- Extract from the list of tokens successive '$' 'IdentTok' tokens, drop the '$' and keep note of
  -- the identifier string
  let process :: [Rust.Token] -> Writer [(String, Rust.Ty Rust.Span)] [Rust.Token]
      process [] = pure []
      process (Rust.Dollar : rest@(Rust.IdentTok i : _)) = tell [show i] *> process rest
      process (tok : rest) = fmap (tok :) (process rest)

  let (body, variables) = runWriter $ process toks

  -- Pack all of the information up
  pure (QQParse ty body variables)

