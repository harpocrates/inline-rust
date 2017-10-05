{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, OverloadedLists, GeneralizedNewtypeDeriving #-}

module Language.Rust.Inline.Context where

import Data.Map (Map)
import qualified Data.Map as M

import qualified Language.Haskell.TH as Haskell
import qualified Language.Rust.Syntax as Rust

import Language.Rust.Quote (ty)
import Language.Haskell.TH (Type(ConT), mkName)

import Data.Semigroup

import Control.Monad (void)

type RType = Rust.Ty ()
type HType = Haskell.Type
type CType = String


-- | Represents a set of conversions of types. It is expected that:
--
-- 'HType' have a storable instance which has the memory layout of 'RType'. (Usually ensured by
-- making both of them correspond to the memory layout of C).
newtype Context = Context (Map RType HType) deriving (Monoid, Semigroup) 

lookupType :: Rust.Ty a -> Context -> Haskell.Q HType
lookupType rt (Context con) = case M.lookup (void rt) con of
                                Nothing -> fail "Could not find information about TODO in the context"
                                Just tup -> pure tup


primitive :: Context
primitive = Context $ M.fromList $ map (\(ht, rts) -> (void rts, ht))
  [ (ConT (mkName "CBool"),   [ty| libc::boolean_t |])  -- "_Bool",     
                                                                   
  -- integral                                                      
  , (ConT (mkName "CChar"),   [ty| libc::c_char |])     -- "char",      
  , (ConT (mkName "CShort"),  [ty| libc::c_short |])    -- "short",     
  , (ConT (mkName "CInt"),    [ty| libc::c_int |])      -- "int",       
  , (ConT (mkName "CLong"),   [ty| libc::c_long |])     -- "long",      
  , (ConT (mkName "CLLong"),  [ty| libc::c_longlong |]) -- "long long", )
  , (ConT (mkName "CSize"),   [ty| libc::size_t |])     -- "size_t",    
                                                                   
  -- Floating point                                                
  , (ConT (mkName "CFloat"),  [ty| libc::c_float |])    -- "float",     
  , (ConT (mkName "CDouble"), [ty| libc::c_double |])   -- "double",    
  ]

  -- TODO add Complex Float and Complex Double, signed and unsigned integral types
