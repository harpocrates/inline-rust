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
{-# LANGUAGE MagicHash #-}

module Language.Rust.Inline.Marshal where

import Language.Rust.Inline.Context

import Language.Haskell.TH 
import Language.Haskell.TH.Syntax  ( addTopDecls ) 

import Data.Word
import Data.Int

import Foreign.Ptr                 ( Ptr, FunPtr, plusPtr )
import Foreign.ForeignPtr          ( withForeignPtr )
import Foreign.StablePtr           ( StablePtr )
import Foreign.Storable            ( Storable )

import Data.ByteString.Internal    ( ByteString(..) )
import Data.Array.Storable         ( StorableArray, Ix, withStorableArray,
                                     getBounds )

import GHC.Exts

data MarshalForm
  = UnboxedDirect      -- ^ value is marshallable and must be passed directly to the FFI
  | BoxedDirect        -- ^ value is marshallable and can be passed directly to the FFI
  | BoxedIndirect      -- ^ value isn't marshallable directly but may be passed indirectly via a 'Ptr'
  deriving (Eq)

-- | Identify which types can be marshalled by the GHC FFI and which types are
-- unlifted. A negative response to the first of these questions doesn't mean
-- the type can't be marshalled - just that we aren't sure it can.
--
-- This is based on section 8.4.2 (Foreign Types) of Haskell2010 and section
-- 11.1.1 (Unboxed types) of the GHC manual. We could do a better job here by
-- also letting through type synonyms / newtypes.
ghcMarshallable :: Type -> Q MarshalForm
ghcMarshallable ty = do
   simpleU <- sequence qSimpleUnboxed
   simpleB <- sequence qSimpleBoxed
   tyconsU <- sequence qTyconsUnboxed
   tyconsB <- sequence qTyconsBoxed

   case ty of
     _          | ty  `elem` simpleU -> pure UnboxedDirect
                | ty  `elem` simpleB -> pure BoxedDirect
     AppT con _ | con `elem` tyconsU -> pure UnboxedDirect
                | con `elem` tyconsB -> pure BoxedDirect
     _                               -> pure BoxedIndirect
  where
  qSimpleUnboxed = [ [t| Char#   |]
                   , [t| Int#    |]
                   , [t| Word#   |]
                   , [t| Double# |]
                   , [t| Float#  |]
                   , [t| Addr# |]
                --   , [t| ForeignObj# |] TODO: where is this even defined
                   , [t| ByteArray# |]
                   ]

  qTyconsUnboxed = [ [t| StablePtr# |]
                   , [t| MutableByteArray# |]
                   ]

  qSimpleBoxed   = [ [t| Char   |] 
                   , [t| Int    |]
                   , [t| Word   |]
                   , [t| Double |]
                   , [t| Float  |]
                   
                   , [t| Bool |], [t| () |] -- TODO: let through `IO ()` but not `()`
                   
                   , [t| Int8  |], [t| Int16  |], [t| Int32  |], [t| Int64  |]
                   , [t| Word8 |], [t| Word16 |], [t| Word32 |], [t| Word64 |]
                  
                   ]

  qTyconsBoxed   = [ [t| Ptr |]
                   , [t| FunPtr |]
                   , [t| StablePtr |]
                   ]


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

-- | TH utility for converting function pointers back into Haskell functions. 
-- Remember to use the 'functions' context. Note that the function is only
-- valid as long as the function pointer.
--
-- @
--    unFunPtr [t| Int -> Int |] fooPtr
-- @
unFunPtr :: Q HType -> Q Exp
unFunPtr hTy = do
  -- Generate FFI
  unFun <- newName . show =<< newName "dyn" -- Make a name to thread through Haskell/Rust (see Trac #13054)
  dec <- forImpD CCall Safe "dynamic" unFun [t| FunPtr $hTy -> $hTy |]
  addTopDecls [dec]
  
  -- Call FFI
  pure (VarE unFun)


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

