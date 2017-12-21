{-|
Module      : Language.Rust.Inline.Marshal
Description : Utilities for marshalling HAskell values
Copyright   : (c) Alec Theriault, 2017
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : GHC
-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Rust.Inline.Marshal where

import Language.Rust.Inline.Context

import Language.Haskell.TH 
import Language.Haskell.TH.Syntax  ( addTopDecls ) 

import Data.Word                   ( Word8 )

import Foreign.Ptr                 ( Ptr, FunPtr, plusPtr )
import Foreign.ForeignPtr          ( withForeignPtr )
import Foreign.Storable            ( Storable )

import Data.ByteString.Internal    ( ByteString(..) )
import Data.Array.Storable         ( StorableArray, Ix, withStorableArray,
                                     getBounds )


-- * Function pointers

-- | TH utility for producing a function that converts plain Haskell functions
-- into their 'FunPtr' form. Remember to use the 'functions' context.
--
-- @
--     let f x = x^2 + 1
--     func <- $(newFunPtr [t| Double -> Double |]) f
--     x <- [rustIO| f64 {
--       let f = $( func: extern "C" fn(f64) -> f64 );
--       f(3.0)
--     } |]
--     freeHaskellFunPtr func
-- @
--
newFunPtr :: Q HType -> Q Exp
newFunPtr hTy = do
  -- Generate FFI
  mkFun <- newName . show =<< newName "to_fun_ptr" -- Make a name to thread through Haskell/Rust (see Trac #13054)
  dec <- forImpD CCall Safe "wrapper" mkFun [t| $hTy -> IO (FunPtr $hTy) |]
  addTopDecls [dec]
  
  -- Call FFI
  pure (VarE mkFun)

-- | TH utility for allocating, marshalling, and then freeing function pointers.
-- Remember to use the 'functions' context. Note that the function pointer is
-- free as soon as the continuation function ends, so don't try to call it from
-- the Rust side after that!
--
-- @
--    x <- $(withFunPtr [t| Double -> Double |]) (\x -> x^2 + 1) $
--      [rustIO| f64 {
--         let f = $( func: extern "C" fn(f64) -> f64 );
--         f(3.0)
--       } |]
-- @
--
withFunPtr :: Q HType -> Q Exp
withFunPtr hTy = do
  func <- newName "func"
  ret <- newName "ret"
  [e| \f cont -> do { $(varP func) <- $(newFunPtr hTy) f
                    ; $(varP ret) <- cont $(varE func)
                    ; freeHaskellFunPtr $(varE func)
                    ; pure $(varE ret)
                    }
    |]


-- * Bytestrings

-- | Utility for marshalling a 'ByteString' into a pointer and a length,
--  Note that the pointer/length may not be valid anymore once the continuation
-- function ends, so don't try to access them from the Rust side after that!
--
-- Note also that Haskell 'ByteString's are supposed to be immutable, so make
-- sure that you do not get a mutable pointer on the Rust side.
withByteString :: ByteString -> (Ptr Word8 -> Word -> IO a) -> IO a
withByteString (PS ptr off len) cont = withForeignPtr ptr go
  where go ptr' = cont (ptr' `plusPtr` off) (fromIntegral len)


-- * Storable arrays

-- | Utility for marshalling a 'StorableArray' into a pointer and bounds.
-- Note that the pointer/length may not be valid anymore once the continuation
-- function ends, so don't try to access them from the Rust side after that!
withStorableArrayLen :: (Storable a, Ix i) => StorableArray i a
                     -> (Ptr a -> (i, i) -> IO b) -> IO b
withStorableArrayLen arr cont = withStorableArray arr go
  where go ptr' = cont ptr' =<< getBounds arr

