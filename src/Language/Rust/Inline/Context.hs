{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}

module Language.Rust.Inline.Context (
  -- * Types
  RType, HType,
  -- * Getting and setting contexts
  setContexts, setContext, getContext, Context, singleton, primitive,
  -- * Other
  lookupType,
) where

import Data.Map (Map)
import qualified Data.Map as M

import qualified Language.Haskell.TH as Haskell
import qualified Language.Rust.Syntax as Rust

import Language.Rust.Quote (ty)
import Language.Haskell.TH.Syntax (Q, putQ, getQ)

import Data.Semigroup
import Data.Typeable
import Data.Maybe (fromMaybe)

import Control.Monad (void)

import Foreign.C.Types

type RType = Rust.Ty ()
type HType = Haskell.Type

setContexts :: [Context] -> Q ()
setContexts = setContext . mconcat

setContext :: Context -> Q ()
setContext = putQ

getContext :: Q Context
getContext = fromMaybe (fail "No context found. Don't forget to `setContext(s)`")  =<< getQ

-- | Represents a set of conversions of types. It is expected that:
--
-- 'HType' have a storable instance which has the memory layout of 'RType'. (Usually ensured by
-- making both of them correspond to the memory layout of C).
newtype Context = Context (Map RType HType) deriving (Monoid, Semigroup, Typeable) 

singleton :: Rust.Ty a -> HType -> Context
singleton rt ht = Context (M.singleton (void rt) ht)

lookupType :: Rust.Ty a -> Context -> Q HType
lookupType rt (Context con) = case M.lookup (void rt) con of
                                Nothing -> fail "Could not find information about TODO in the context"
                                Just tup -> pure tup


primitive :: Q Context
primitive = Context . M.fromList <$> traverse (\(qht, rts) -> fmap (\ht -> (void rts, ht)) qht)
  [{- ([t| CBool         |], [ty| libc::boolean_t  |]) -- _Bool
  
  -- integral                                                             
  ,-} ([t| CChar         |], [ty| libc::c_char     |]) -- char
  , ([t| CShort        |], [ty| libc::c_short    |]) -- short
  , ([t| CInt          |], [ty| libc::c_int      |]) -- int
  , ([t| CLong         |], [ty| libc::c_long     |]) -- long
  , ([t| CLLong        |], [ty| libc::c_longlong |]) -- long long
  , ([t| CSize         |], [ty| libc::size_t     |]) -- size_t
  
  -- Floating point                                                
  , ([t| CFloat        |], [ty| libc::c_float    |]) -- float
  , ([t| CDouble       |], [ty| libc::c_double   |]) -- double
  ]

  -- TODO add Complex Float and Complex Double, signed and unsigned integral types
