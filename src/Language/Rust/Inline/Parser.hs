module Language.Rust.Inline.Parser where

import Language.Rust.Syntax
import Language.Rust.Parser
import Language.Rust.Data.Position

import Language.Haskell.TH

import Data.String (fromString)

-- | Result of parsing a quasiquote. Quasiquotes are of the form `<ty> <block>` where the `<block>`
-- possibly contains `$<ident>` to unescape. 
data RustQuasiquoteParse = QQParse
  { ty :: Ty Span                       -- The leading type
  , body :: [Token]                     -- The body tokens, with `$(<ident>: <ty>)` pairs replaced by just `<ident>`
  , variables :: [(String, Ty Span)]    -- The `$(<ident>: <ty>)` args to escape
  }

-- | Parse an inline Rust quasiquote. The grammar for a quasiquote is
--
-- <quasiquote> ::= <ty> <body>
--
-- <body>       ::= '$' '(' <ident> ':' <ty> ')' <body>
--                | <tok> <body>
--                | {- empty -}
--
parseQQ :: String -> Q RustQuasiquoteParse
parseQQ input = case execParser (lexTokens lexNonSpace) (fromString input) initPos of
                  Left (_, msg) -> fail msg
                  Right spToks' -> go (LeadingType []) spToks'
  where
  go :: ProcessState -> [Spanned Token] -> Q RustQuasiquoteParse
  -- While in the leading type phase, keep going until you hit the first '{'
  go (LeadingType _     )       []                                   = fail "Ran out of input while parsing leading type in quasiquote"
  go (LeadingType tyToks) body'@(Spanned (OpenDelim Brace) _ : _   ) = either (fail . snd) (\ty' -> go (Body ty' [] []) body') (parseFromToks tyToks)
  go (LeadingType tyToks)       (tok                         : rest) = go (LeadingType (tok : tyToks)) rest 
  -- While in the body phase, keep going until there are no more tokens, or you hit '$(<ident> :'
  go (Body ty' body' vars)      []                                   = pure (QQParse ty' body' vars)
  go (Body ty' body' vars)      (Spanned Dollar _ : Spanned (OpenDelim Paren) _ : Spanned (IdentTok i) _ : Spanned Colon _ : rest) = go (Escape ty' body' vars 1 i rest) rest
  go (Body ty' body' vars)      (tok                                                                                       : rest) = go (Body ty' (unspan tok : body') vars) rest
  -- While in the escape phase, keep going until you balance out the parens
  go (Escape _   _    _    _ _ _     ) []   = fail "Ran out of input while parsing variable escape (did you forget a closing paren?)"
  go (Escape ty' body' vars 0 i tyToks) rest = either (fail . snd) (\ty'' -> go (Body ty' (IdentTok i : body') ((show i, ty'') : vars)) rest) (parseFromToks tyToks)
  go (Escape ty' body' vars p i tyToks) ( tok@(Spanned (OpenDelim Paren) _) : rest) = go (Escape ty' body' vars (p + 1) i (tok : tyToks)) rest
  go (Escape ty' body' vars p i tyToks) ( tok@(Spanned (CloseDelim Paren) _) : rest) = go (Escape ty' body' vars (p - 1) i (tok : tyToks)) rest
  go (Escape ty' body' vars p i tyToks) ( tok : rest) = go (Escape ty' body' vars p i (tok : tyToks)) rest


  parseFromToks :: Parse a => [Spanned Token] -> Either (Position, String) a
  parseFromToks toks = execParserTokens parser (reverse toks) initPos

data ProcessState
  = LeadingType [Spanned Token]                   -- Tokens so far in the type
  | Body (Ty Span)                                -- Leading type
         [Token] [(String, Ty Span)]              -- Tokens to far in the body, as well as escape args so far
  | Escape (Ty Span) [Token] [(String, Ty Span)]  -- State from 'Body'
           Int                                    -- Paren count (when back to 0, we return to the 'Body' state)
           Ident [Spanned Token]                  -- Identifier name and tokens so far in the type 
