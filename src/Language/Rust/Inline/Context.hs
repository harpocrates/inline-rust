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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}

module Language.Rust.Inline.Context where

import Language.Rust.Inline.Pretty ( renderType )

import Language.Rust.Quote         ( ty )
import Language.Rust.Syntax        ( Ty(BareFn, Ptr), Abi(..), FnDecl(..),
                                     Arg(..), Mutability(..) )

import Language.Haskell.TH 

import Data.Semigroup              ( Semigroup )
import Data.Monoid                 ( First(..) )
import Data.Typeable               ( Typeable )
import Control.Monad               ( void, liftM2 )
import Data.Traversable            ( for )

import Data.Int                    ( Int8, Int16, Int32, Int64 )
import Data.Word                   ( Word8, Word16, Word32, Word64 )
import Foreign.Ptr                 ( Ptr, FunPtr )
import Foreign.C.Types             -- pretty much every type here is used

import GHC.Exts                    ( Char#, Int#, Word#, Float#, Double#,
                                     ByteArray# )

-- Easier on the eyes
type RType = Ty ()
type HType = Type

-- | Represents a prioritized set of rules for mapping Haskell types into Rust
-- ones and vice versa.
--
-- The 'Context' argument encodes the fact that we may need look
-- recursively into the 'Context' again before possibly producing a Haskell
-- type.
newtype Context =
    Context ( [ RType -> Context -> First (Q HType, Maybe (Q RType)) ]
            -- Given a Rust type in a quasiquote, we need to look up the
            -- corresponding Haskell type (for the FFI import) as well as the
            -- C-compatible Rust type (if the initial Rust type isn't already
            -- @#[repr(C)]@.
            
            , [ HType -> Context -> First (Q RType) ]
            -- Given a field in a Haskell ADT, we need to figure out which
            -- (not-necessarily @#[repr(C)]@) Rust type normally maps into this
            -- Haskell type.
            )
  deriving (Semigroup, Monoid, Typeable)

-- | Applicative lifting of the 'Context' instance
instance Semigroup (Q Context) where
  (<>) = liftM2 (<>)

-- | Applicative lifting of the 'Context' instance
instance Monoid (Q Context) where
  mappend = (<>)
  mempty = pure mempty
  

-- | Search in a 'Context' for the Haskell type corresponding to a Rust type.
-- If the Rust type is not C-compatible, also return a C compatible type. It is
-- expected that:
--
--   1. The Haskell type have a 'Storable' instance
--   2. The C-compatible Rust type have the same layout
--
lookupRTypeInContext :: RType -> Context -> First (Q HType, Maybe (Q RType))
lookupRTypeInContext rustType context@(Context (rules, _)) =
  foldMap (\fits -> fits rustType context) rules

-- | Search in a 'Context' for the Rust type corresponding to a Haskell type.
-- Looking up the Rust type using 'lookupRTypeInContext' should yield the
-- initial Haskell type again.
lookupHTypeInContext :: HType -> Context -> First (Q RType)
lookupHTypeInContext haskType context@(Context (_, rules)) =
  foldMap (\fits -> fits haskType context) rules

-- | Partial version of 'lookupRTypeInContext' that fails with an error message
-- if the type is not convertible.
getRTypeInContext :: RType -> Context -> (Q HType, Maybe (Q RType))
getRTypeInContext rustType context =
  case getFirst (lookupRTypeInContext rustType context) of
    Just found -> found
    Nothing -> ( fail $ unwords [ "Could not find information about"
                                , renderType rustType
                                , "in the context"
                                ]
               , Nothing )

-- | Partial version of 'lookupHTypeInContext' that fails with an error message
-- if the type is not convertible.
getHTypeInContext :: HType -> Context -> Q RType
getHTypeInContext haskType context =
  case getFirst (lookupHTypeInContext haskType context) of
    Just found -> found
    Nothing -> fail $ unwords [ "Could not find information about"
                              , pprint haskType
                              , "in the context"
                              ]


-- | Make a 'Context' consisting of rules to map the Rust types on the left to
-- the Haskell types on the right. The Rust types should all be @#[repr(C)]@
-- and the Haskell types should all be 'Storable'.
mkContext :: [(Ty a, Q HType)] -> Q Context
mkContext tys = do
    tys' <- traverse (\(rt,qht) -> fmap ((,) (void rt)) qht) tys
    pure (Context (map fits tys', map rev tys'))
  where
    fits (rts, hts) rt _ | rt == rts = pure (pure hts, Nothing)
                         | otherwise = mempty

    rev (rts, hts) ht _  | ht == hts = pure (pure rts)
                         | otherwise = mempty

-- | Make a singleton 'Context' consisting of a rule to map the given Rust type
-- to the given Haskell type.
singleton :: Ty a -> Q HType -> Q Context
singleton rts qht = mkContext [(rts, qht)]


-- * Some handy contexts

-- | Types defined in 'Foreign.C.Types' and the 'libc' crate.
--
-- There should be no conversion required here - these have /identical/ memory
-- layouts (since they both promise to have the same memory layout as C) and are
-- passed on the stack.
libc :: Q Context
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
  , ([ty| libc::int8_t      |], [t| Int8       |]) -- int8_t
  , ([ty| libc::int16_t     |], [t| Int16      |]) -- int16_t
  , ([ty| libc::int32_t     |], [t| Int32      |]) -- int32_t
  , ([ty| libc::int64_t     |], [t| Int64      |]) -- int64_t
  , ([ty| libc::uint8_t     |], [t| Word8      |]) -- uint8_t
  , ([ty| libc::uint16_t    |], [t| Word16     |]) -- uint16_t
  , ([ty| libc::uint32_t    |], [t| Word32     |]) -- uint32_t
  , ([ty| libc::uint64_t    |], [t| Word64     |]) -- uint64_t
  ]

-- | Basic numeric (and similar) Haskell and Rust types.
--
-- There should be no conversion required here as these should have identical
-- memory layouts.
basic :: Q Context
basic = mkContext
  [ ([ty| char  |], [t| Char    |]) -- 4 bytes
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
  , ([ty| bool  |], [t| Word8   |])
  , ([ty| ()    |], [t| ()      |])
  ]

-- | Basic unboxed Haskell types
--
-- TODO: MutableByteArray#
ghcUnboxed :: Q Context
ghcUnboxed = mkContext
  [ ([ty| char      |], [t| Char#   |])
  , ([ty| isize     |], [t| Int#    |])
  , ([ty| usize     |], [t| Word#   |])
  , ([ty| f32       |], [t| Float#  |])
  , ([ty| f64       |], [t| Double# |])
  , ([ty| *const i8 |], [t| ByteArray# |])
  ]

-- | Haskell pointers map onto Rust pointers. Note that unlike Rust, Haskell
-- doesn't really distinguish between pointers pointing to immutable memory from
-- those pointing to to mutable memory, so it is up to the user to enforce this.
--
-- NOTE: pointers will not support pointed types that require an intermediate
--       Rust type.
pointers :: Q Context
pointers = do
    ptrConT <- [t| Ptr |]
    pure (Context ([rule],[rev ptrConT]))
  where
  rule pt context = do
    Ptr _ t _ <- pure pt
    (t', Nothing) <- lookupRTypeInContext t context
    pure ([t| Ptr $t' |], Nothing)

  rev ptrConT pt context = do
    AppT ptrCon t <- pure pt
    if ptrCon /= ptrConT
      then mempty
      else do
        t' <- lookupHTypeInContext t context
        pure (Ptr Mutable <$> t' <*> pure ())
  
-- | This maps a Rust function type into the corresponding 'FunPtr' wrapped
-- Haskell function type.
--
-- Note that as a user, you are still responsible for marshalling values of
-- type 'FunPtr'. The reason for this is simple: the GHC runtime has no way of
-- automatically detecting when a pointer to a function is no longer present on
-- the Rust side.
--
-- NOTE: function pointers will not support pointed types that require an intermediate
--       Rust type.
functions :: Q Context
functions = pure (Context ([rule], undefined))
  where
  rule ft context = do
    BareFn _ C _ (FnDecl args retTy False _) _ <- pure ft
    args' <-
      for args $ \arg -> do
        Arg _ argTy _ <- pure arg
        (t', Nothing) <- lookupRTypeInContext argTy context
        pure t'

    retTy' <-
      case retTy of
        Nothing -> pure [t| IO () |]
        Just t -> do
          (t', Nothing) <- lookupRTypeInContext t context
          pure t'

    let hFunTy = foldr (\l r -> [t| $l -> $r |]) retTy' args'
    let hFunPtr = [t| FunPtr $hFunTy |]

    pure (hFunPtr, Nothing)

