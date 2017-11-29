{-|
Module      : Language.Rust.Inline.Parser
Description : Parser for Quasiquotes
Copyright   : (c) Alec Theriault, 2017
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : GHC
-}

module Language.Rust.Inline.Parser where

import Language.Rust.Data.Ident (Ident)
import Language.Rust.Syntax
import Language.Rust.Parser
import Language.Rust.Data.Position

import Language.Haskell.TH

import Debug.Trace

-- | Result of parsing a quasiquote. Quasiquotes are of the form
-- `<ty> { <block> }` where the `<block>` possibly contains unescaped arguments
-- in the form of `$(<ident>: <ty>)`.
data RustQuasiquoteParse = QQParse
  { ty :: Ty Span                     -- ^ leading type
  , body :: [Token]                   -- ^ body tokens, with `$(<ident>: <ty>)`
                                      -- pairs replaced by just `<ident>`
  , variables :: [(String, Ty Span)]  -- ^ `$(<ident>: <ty>)` args to escape
  } deriving (Show)

-- | Parse an inline Rust quasiquote. The grammar for a quasiquote is
--
-- @
--     <quasiquote> ::= <ty> '{' <body> '}'
--
--     <body>       ::= '$' '(' <ident> ':' <ty> ')' <body>
--                    | <tok> <body>
--                    | {- empty -}
-- @
parseQQ :: String -> Q RustQuasiquoteParse
parseQQ input = case execParser lexer stream initPos of
                  Left (_, msg) -> fail msg
                  Right spToks' -> go (LeadingType []) spToks'
  where
  -- Our parser and its input
  lexer = lexTokens lexNonSpace
  stream = inputStreamFromString input

  go :: ProcessState -> [Spanned Token] -> Q RustQuasiquoteParse
  -- While in the leading type phase, keep going until you hit the first '{'
  go (LeadingType _     )       []                                   = fail "Ran out of input while parsing leading type in quasiquote"
  go (LeadingType tyToks) body'@(Spanned (OpenDelim Brace) _ : _   ) = either (fail . snd) (\ty' -> go (Body ty' [] []) body') (parseFromToks tyToks)
  go (LeadingType tyToks)       (tok                         : rest) = go (LeadingType (tok : tyToks)) rest 
  -- While in the body phase, keep going until there are no more tokens, or you hit '$(<ident> :'
  go (Body ty' body' vars)      []                                   = pure (QQParse ty' (reverse body') vars)
  go (Body ty' body' vars)      (Spanned Dollar _ : Spanned (OpenDelim Paren) _ : Spanned (IdentTok i) _ : Spanned Colon _ : rest) = go (Escape ty' body' vars 1 i []) rest
  go (Body ty' body' vars)      (tok                                                                                       : rest) = go (Body ty' (unspan tok : body') vars) rest
  -- While in the escape phase, keep going until you balance out the parens
  go (Escape _   _    _    _ _ _     ) []   = fail "Ran out of input while parsing variable escape (did you forget a closing paren?)"
  go (Escape ty' body' vars p i tyToks) ( tok@(Spanned (CloseDelim Paren) _) : rest)
    | p - 1 > 0 = go (Escape ty' body' vars (p - 1) i (tok : tyToks)) rest
    | otherwise = either (fail . snd) (\ty'' -> go (Body ty' (IdentTok i : body') ((show i, ty'') : vars)) rest) (parseFromToks tyToks)
  go (Escape ty' body' vars p i tyToks) ( tok@(Spanned (OpenDelim Paren) _) : rest) = go (Escape ty' body' vars (p + 1) i (tok : tyToks)) rest
  go (Escape ty' body' vars p i tyToks) ( tok : rest) = go (Escape ty' body' vars p i (tok : tyToks)) rest


  parseFromToks :: Parse a => [Spanned Token] -> Either (Position, String) a
  parseFromToks toks = execParserTokens parser (reverse toks) initPos

-- | States of the parsing DFA
data ProcessState
  = LeadingType            -- ^ Still parsing the leading type...
      [Spanned Token]      -- ^ Tokens so far in the leading type

  | Body                   -- ^ Parsing the body...
      (Ty Span)            -- ^ Parsed leading type
      [Token]              -- ^ Tokens so far in the body
      [(String, Ty Span)]  -- ^ Escape arguments so far in the body

  | Escape                 -- ^ Parsing an escape argument in the body...
      (Ty Span)            -- ^ Parsed leading type
      [Token]              -- ^ Tokens so far in the body
      [(String, Ty Span)]  -- ^ Escape arguments so far in the body
      Int                  -- ^ Unclosed open parens (if 0, go back to 'Body')
      Ident                -- ^ Escape argument identifier name
      [Spanned Token]      -- ^ Tokens so far in the escape argument type
