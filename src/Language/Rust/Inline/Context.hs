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

import Language.Rust.Syntax        ( Ty(BareFn), Abi(..), FnDecl(..), Arg(..) )
import Language.Rust.Quote         ( ty )

import Language.Haskell.TH         ( Q, Type )

import Data.Semigroup              ( Semigroup )
import Data.Monoid                 ( First(..) )
import Data.Typeable               ( Typeable )
import Control.Monad               ( void )
import Data.Traversable            ( for )

import Data.Int                    ( Int8, Int16, Int32, Int64 )
import Data.Word                   ( Word8, Word16, Word32, Word64 )
import Foreign.Ptr                 ( IntPtr, WordPtr, FunPtr )
import Foreign.C.Types             ( CBool, CChar, CShort, CInt, CLong, CLLong,
                                     CSize, CFloat, CDouble, CIntPtr, CUIntPtr )
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
lookupTypeInContext :: RType -> Context -> First (Q HType)
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
-- TODO: Expand this
libc :: Context
libc = mkContext
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
basic = mkContext
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
  , ([ty| ()    |], [t| ()      |])
  ]

-- | Function pointers.
functions :: Context
functions = Context [ rule ]
  where

  rule :: RType -> Context -> First (Q HType)
  rule (BareFn _ C _ (FnDecl args retTy False _) _) context = do

    args' <- for args $ \arg -> do
                Arg _ argTy _ <- pure arg
                lookupTypeInContext argTy context

    retTy' <- case retTy of
                Just t -> lookupTypeInContext t context
                Nothing -> pure [t| () |]
    
    let hFunTy = foldl1 (\l r -> [t| $l -> $r |]) (args' ++ [ retTy' ])
    let hFunPtr = [t| FunPtr $hFunTy |]

    pure hFunPtr
  
  rule _ _ = mempty

