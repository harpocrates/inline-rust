{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, OverloadedLists #-}

module Language.Rust.Inline.Context where

import Data.Map
import qualified Data.Map as M

import qualified Language.Haskell.TH as Haskell
import qualified Language.Rust.Syntax as Rust

import Language.Rust.Quote (ty)

import Data.Semigroup
import GHC.Exts (IsList(..))

import Data.Int
import Data.Word

import Control.Monad (void)

type RType = Rust.Ty ()
type HType = Haskell.Type
type CType = String


-- | Represents a set of conversions of types. It is expected that:
--
-- 'HType' have a storable instance which has the memory layout of 'CType', which should in turn
-- share its memory layout with 'RType'.
data Context = Context { fromRust    :: Map RType (CType, HType)
                       , fromHaskell :: Map HType (CType, RType)
                       }

lookupRust :: RType -> Context -> (CType, HType)
lookupRust rt con = fromRust con rt

lookupHaskell :: HType -> Context -> (CType, RType)
lookupHaskell ht con = fromHaskell con ht

instance Semigroup Context where
  Context r1 h1 <> Context r2 h2 = Context (r1 <> r2) (h1 <> h2)

instance Monoid Context where
  mempty = Context mempty mempty
  mappend = (<>)

instance IsList Context where
  type Item Context = (HType, CType, RType)
  
  toList (Context _ m) = [ (h, c, r) | (h, (c, r)) <- M.toList m ]
  
  fromList elms | length elms /= size rMap || size rMap /= size hMap = error "Injectivity failed"
                | otherwise = Context rMap hMap
    where rMap = M.fromList [ (r, (c, h)) | (h, c, r) <- elms ]
          hMap = M.fromList [ (h, (c, r)) | (h, c, r) <- elms ]


primitive :: Haskell.Q Context
primitive = GHC.Exts.fromList <$> traverse (\(qht, ct, rts) -> (,,) <$> qht <*> pure ct <*> pure (void rts))
  [ ([t| Bool |],  "_Bool",        [ty| bool |])
  
  -- Signed
  , ([t| Int8 |],  "signed char",  [ty| i8 |])
  , ([t| Int16 |], "signed short", [ty| i16 |])
  , ([t| Int32 |], "signed int",   [ty| i32 |])
  , ([t| Int64 |], "signed long",  [ty| i64 |])

  -- Unsigned
  , ([t| Word8 |],  "unsigned char",  [ty| u8 |])
  , ([t| Word16 |], "unsigned short", [ty| u16 |])
  , ([t| Word32 |], "unsigned int",   [ty| u32 |])
  , ([t| Word64 |], "unsigned long",  [ty| u64 |])

  -- Floating point
  , ([t| Float |], "float",   [ty| f32 |])
  , ([t| Double |], "double", [ty| f64 |])
  
  ]

  -- TODO add Complex Float and Complex Double
