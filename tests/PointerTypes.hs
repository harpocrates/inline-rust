{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module PointerTypes where

import Language.Rust.Inline
import Test.Hspec

import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Utils  ( new )
import Foreign.Marshal.Array  ( newArray )
import System.IO              ( fixIO )
import Data.Word              ( Word )

extendContext pointers 
extendContext basic
setCrateModule

pointerTypes :: Spec
pointerTypes = describe "Pointer types" $ do
  it "Can marshal an immutable `Ptr Int` argument/return" $ do
    arr <- newArray ([5,-7,3,9] :: [Int]) 
   
    -- The pointer at offset 2 is the same
    let off = [rust| *const isize { unsafe { $(arr: *const isize).offset(2) } } |]
        off' = arr `plusPtr` (2 * sizeOf off')
    off `shouldBe` off'
    
    -- The value at offset 2 is the same
    x <- [rustIO| isize { unsafe { *$(arr: *const isize).offset(2) } } |]
    x' <- arr `peekElemOff` 2
    x `shouldBe` x'

  it "Can marshal a mutable `Ptr Word` argument" $ do
    let x = 8 :: Word
    ptr <- new x
    [rustIO| () { unsafe { *$(ptr: *mut usize) += 1; } } |]
    x' <- peek ptr
    x' `shouldBe` x + 1

  it "Supports null pointers" $ do
    nullPtr `shouldBe` [rust| *const char { use std::ptr; ptr::null() } |]
    nullPtr `shouldBe` [rust| *const char { use std::ptr; ptr::null_mut() } |]

