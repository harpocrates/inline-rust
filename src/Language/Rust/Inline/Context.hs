{-|
Module      : Language.Rust.Inline.Context
Description : Defines contexts (rules mapping Rust types to Haskell types)
Copyright   : (c) Alec Theriault, 2017
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : GHC
-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Language.Rust.Inline.Context where

import Language.Rust.Inline.Pretty ( renderType )

import Language.Rust.Syntax        ( Ty(BareFn, Ptr), Abi(..), FnDecl(..), Arg(..) )
import Language.Rust.Quote         ( ty )

import Language.Haskell.TH 
import Language.Haskell.TH.Syntax  ( addTopDecls ) 

import Data.Semigroup              ( Semigroup )
import Data.Monoid                 ( First(..) )
import Data.Typeable               ( Typeable )
import Control.Monad               ( void )
import Data.Traversable            ( for )

import Data.Int                    ( Int8, Int16, Int32, Int64 )
import Data.Word                   ( Word8, Word16, Word32, Word64 )
import Foreign.Ptr                 ( Ptr, FunPtr, plusPtr )
import Foreign.ForeignPtr          ( withForeignPtr )
import Foreign.Storable            ( Storable )
import Foreign.C.Types             -- pretty much every type here is used

import Data.ByteString.Internal    ( ByteString(..) )
import Data.Array.Storable         ( StorableArray, Ix, withStorableArray,
                                     getBounds )

-- Easier on the eyes
type RType = Ty ()
type HType = Type

-- | Represents a prioritized set of rules for converting a Rust type to a
-- Haskell one. The 'Context' argument encodes the fact that we may need look
-- recursively into the 'Context' again before possibly producing a Haskell
-- type.
newtype Context = Context [ RType -> Context -> First (Q HType) ]
  deriving (Semigroup, Monoid, Typeable)

-- | Search in a 'Context' for the Haskell type corresponding to a Rust type.
-- The approach taken is to scan the context rules from left to right looking
-- for the first successful conversion to a Haskell type.
--
-- It is expected that the 'HType' found from an 'RType' have a 'Storable'
-- instance which has the memory layout of 'RType'.
lookupTypeInContext :: RType -> Context -> First (Q (HType))
lookupTypeInContext rustType context@(Context rules) =
  foldMap (\fits -> fits rustType context) rules


-- | Partial version of 'lookupTypeInContext' that fails with an error message
-- if the type is not convertible.
getTypeInContext :: RType -> Context -> Q HType
getTypeInContext rustType context =
  case getFirst (lookupTypeInContext rustType context) of
    Just hType -> hType
    Nothing -> fail $ unwords [ "Could not find information about"
                              , renderType rustType
                              , "in the context"
                              ]


-- | Make a 'Context' consisting of rules to map the Rust types on the left to
-- the Haskell types on the right.
mkContext :: [(Ty a, Q HType)] -> Context
mkContext = Context . map fits 
  where
    fits (rts, qht) rt _ | rt == void rts = pure qht
                         | otherwise = mempty

-- | Make a singleton 'Context' consisting of a rule to map the given Rust type
-- to the given Haskell type.
singleton :: Ty a -> Q HType -> Context
singleton rts qht = mkContext [(rts, qht)]


-- | Types defined in 'Foreign.C.Types' and the 'libc' crate.
--
-- There should be no conversion required here - these have /identical/ memory
-- layouts (since they both promise to have the same memory layout as C) and are
-- passed on the stack.
libc :: Context
libc = mkContext
  [ ([ty| libc::c_char      |], [t| CChar      |]) -- char
  , ([ty| libc::c_schar     |], [t| CSChar     |]) -- signed char
  , ([ty| libc::c_uchar     |], [t| CUChar     |]) -- unsigned char
  , ([ty| libc::c_short     |], [t| CShort     |]) -- short
  , ([ty| libc::c_ushort    |], [t| CUShort    |]) -- unsigned short
  , ([ty| libc::c_int       |], [t| CInt       |]) -- int
  , ([ty| libc::c_uint      |], [t| CUInt      |]) -- unsigned int
  , ([ty| libc::c_long      |], [t| CLong      |]) -- long
  , ([ty| libc::c_ulong     |], [t| CULong     |]) -- unsigned long
  , ([ty| libc::ptrdiff_t   |], [t| CPtrdiff   |]) -- ptrdiff_t
  , ([ty| libc::size_t      |], [t| CSize      |]) -- size_t
  , ([ty| libc::wchar_t     |], [t| CWchar     |]) -- wchar_t
  , ([ty| libc::c_longlong  |], [t| CLLong     |]) -- long long
  , ([ty| libc::c_ulonglong |], [t| CULLong    |]) -- unsigned long long
  , ([ty| libc::boolean_t   |], [t| CBool      |]) -- bool
  , ([ty| libc::intptr_t    |], [t| CIntPtr    |]) -- intptr_t
  , ([ty| libc::uintptr_t   |], [t| CUIntPtr   |]) -- uintptr_t
  , ([ty| libc::intmax_t    |], [t| CIntMax    |]) -- intmax_t
  , ([ty| libc::uintmax_t   |], [t| CUIntMax   |]) -- unsigned intmax_t
  , ([ty| libc::clock_t     |], [t| CClock     |]) -- clock_t
  , ([ty| libc::time_t      |], [t| CTime      |]) -- time_t
  , ([ty| libc::useconds_t  |], [t| CUSeconds  |]) -- useconds_t
  , ([ty| libc::suseconds_t |], [t| CSUSeconds |]) -- suseconds_t
  , ([ty| libc::c_float     |], [t| CFloat     |]) -- float
  , ([ty| libc::c_double    |], [t| CDouble    |]) -- double
  , ([ty| libc::FILE        |], [t| CFile      |]) -- FILE
  , ([ty| libc::fpos_t      |], [t| CFpos      |]) -- fpos_t
  ]

-- | Basic numeric (and similar) Haskell and Rust types.
--
-- There should be no conversion required here as these should have identical
-- memory layouts.
basic :: Context
basic = mkContext
  [ ([ty| bool  |], [t| Word8   |])
  , ([ty| char  |], [t| Char    |]) -- 4 bytes
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
  , ([ty| isize |], [t| Int     |])
  , ([ty| usize |], [t| Word    |])
  , ([ty| ()    |], [t| ()      |])
  ]

-- | Haskell pointers map onto Rust pointers. Note that unlike Rust, Haskell
-- doesn't really distinguish between pointers pointing to immutable memory from
-- those pointing to to mutable memory, so it is up to the user to enforce this.
pointers :: Context
pointers = Context [ rule ]
  where
  rule pt context = do
    Ptr _ t _ <- pure pt
    t' <- lookupTypeInContext t context
    pure [t| Ptr $t' |]
  
-- | See 'FunPtr'  
functions :: Context
functions = Context [ rule ]
  where
  rule ft context = do
    BareFn _ C _ (FnDecl args retTy False _) _ <- pure ft
    args' <- for args $ \arg -> do
               Arg _ argTy _ <- pure arg
               lookupTypeInContext argTy context

    retTy' <- case retTy of
                Just t -> lookupTypeInContext t context
                Nothing -> pure [t| IO () |]

    let hFunTy = foldr (\l r -> [t| $l -> $r |]) retTy' args'
    let hFunPtr = [t| FunPtr $hFunTy |]

    pure hFunPtr

toFunPtr :: Q HType -> Q Exp
toFunPtr hTy = do
  -- Generate FFI
  mkFun <- newName . show =<< newName "to_fun_ptr" -- Make a name to thread through Haskell/Rust (see Trac #13054)
  dec <- forImpD CCall Safe "wrapper" mkFun [t| $hTy -> IO (FunPtr $hTy) |]
  addTopDecls [dec]
  
  -- Call FFI
  pure (VarE mkFun)


withByteString :: ByteString -> (Ptr Word8 -> Word -> IO a) -> IO a
withByteString (PS ptr off len) cont = withForeignPtr ptr go
  where go ptr' = cont (ptr' `plusPtr` off) (fromIntegral len)

withArray :: (Storable a, Ix i) => StorableArray i a -> (Ptr a -> (i, i) -> IO b) -> IO b
withArray arr cont = withStorableArray arr (\ptr -> cont ptr =<< getBounds arr)
