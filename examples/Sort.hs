{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Main where

import Language.Rust.Inline ( rustIO, extendContext, setCrateRoot, basic, pointers, withStorableArrayLen )
import Data.Array.Storable  ( StorableArray, newListArray, getElems )
import Data.Int             ( Int64 )

extendContext basic
extendContext pointers

setCrateRoot []

main :: IO ()
main = do
  arr <- newListArray (0,9) [5,1,9,0,3,4,7,6,1,8]
  print =<< getElems arr 
  sortStorableArray arr
  print =<< getElems arr 

-- | Sort a storable array
sortStorableArray :: StorableArray Word Int64 -> IO ()
sortStorableArray arr = withStorableArrayLen arr $ \ptr (lo,hi) ->
  [rustIO|
   () {
     unsafe {
       std::slice::from_raw_parts_mut(
         $(ptr: *mut i64),
         $(hi: usize) - $(lo: usize) + 1,
       ).sort()
     }
   }
  |]


