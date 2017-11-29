{-# LANGUAGE TypeFamilies, QuasiQuotes, TemplateHaskell, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}

module Language.Rust.Inline.Context (
  -- * Types
  RType,
  HType,
  Context,
  singleton,
  lookupTypeInContext,

  -- * Contexts
  basic,
  libc,
) where

import qualified Language.Haskell.TH as Haskell
import qualified Language.Rust.Syntax as Rust

import Language.Rust.Quote        ( ty )
import Language.Haskell.TH.Syntax ( Q )

import Data.Semigroup             ( Semigroup )
import Data.Monoid                ( First(..) )
import Data.Typeable              ( Typeable )

import Control.Monad              ( void )

import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.Ptr

type RType = Rust.Ty ()
type HType = Haskell.Type

-- | Represents a prioritized set of rules for converting a Rust type to a
-- Haskell one.
newtype Context = Context [ RType -> Context -> First (Q HType) ]
  deriving (Monoid, Semigroup, Typeable) 

-- | Search in a 'Context' for the Haskell type corresponding to a Rust type.
--
-- It is expected that the 'HType' found from an 'RType' have a 'Storable'
-- instance which has the memory layout of 'RType'.
lookupTypeInContext :: RType -> Context -> Q HType
lookupTypeInContext rustType context@(Context rules) = 
  case getFirst matchingRules of
    Nothing -> fail "Could not find information about TODO in the context"
    Just tup -> tup
  where
    matchingRules = foldMap (\fits -> fits rustType context) rules
    

-- | Convenient function for making the definition of simple 'Context's nice.
mkListContext :: [(Rust.Ty a, Q HType)] -> Context
mkListContext = Context . map fits 
  where
    fits (rts, qht) rt _ | rt == void rts = First (Just qht)
                         | otherwise = First Nothing

-- | Make a singleton 'Context'.
singleton :: Rust.Ty a -> Q HType -> Context
singleton rts qht = mkListContext [(rts, qht)]


-- | Types defined in 'Foreign.C.Types' and the 'libc' crate.
--
-- TODO: Expand this
libc :: Context
libc = mkListContext
  [ ([ty| libc::boolean_t  |], [t| CBool    |]) -- _Bool
  , ([ty| libc::c_char     |], [t| CChar    |]) -- char
  , ([ty| libc::c_short    |], [t| CShort   |]) -- short
  , ([ty| libc::c_int      |], [t| CInt     |]) -- int
  , ([ty| libc::c_long     |], [t| CLong    |]) -- long
  , ([ty| libc::c_longlong |], [t| CLLong   |]) -- long long
  , ([ty| libc::size_t     |], [t| CSize    |]) -- size_t
  , ([ty| libc::c_float    |], [t| CFloat   |]) -- float
  , ([ty| libc::c_double   |], [t| CDouble  |]) -- double
  , ([ty| libc::intptr_t   |], [t| CIntPtr  |]) -- intptr_t
  , ([ty| libc::uintptr_t  |], [t| CUIntPtr |]) -- uintptr_t
  ]

-- | Basic Haskell and Rust types.
--
-- TODO: Expand this
basic :: Context
basic = mkListContext
  [ ([ty| bool  |], [t| Bool    |])
  , ([ty| i8    |], [t| Int8    |])
  , ([ty| i16   |], [t| Int16   |])
  , ([ty| i32   |], [t| Int32   |])
  , ([ty| i64   |], [t| Int64   |])
  , ([ty| u8    |], [t| Word8   |])
  , ([ty| u16   |], [t| Word16  |])
  , ([ty| u32   |], [t| Word32  |])
  , ([ty| u64   |], [t| Word64  |])
  , ([ty| f32   |], [t| Float   |])
  , ([ty| f64   |], [t| Double  |])
  , ([ty| isize |], [t| IntPtr  |]) 
  , ([ty| usize |], [t| WordPtr |]) 
  ]
