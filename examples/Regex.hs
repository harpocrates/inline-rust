{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}

module Main where

import Language.Rust.Inline
import Foreign.Marshal.Unsafe
import Foreign.Ptr
import Foreign.Marshal.Utils

import Data.ByteString
import Data.Word ( Word64 )
import Data.Int  ( Int64 )

extendContext basic
extendContext pointers

setCrateRoot
  [ ("regex", "0.2") 
  ]

[rust|
extern crate regex;
use regex::bytes::Regex;
use std::slice::from_raw_parts_mut;
|]

main = do
  print $ multipleOfThree "1111100010101110010101"   -- 4074389 = 2 (mod 3)
  print $ multipleOfThree "100101"                   -- 37      = 1 (mod 3)
  print $ multipleOfThree "101100101001010101111"    -- 1462959 = 0 (mod 3)

-- | Check if a bytestring fits a regular expression
multipleOfThree :: ByteString -> Bool
multipleOfThree input = unsafeLocalState $ withByteString input $ \ptr len -> do
  out <- [rustIO|
   bool {
     let input: &mut [u8] = unsafe {
       from_raw_parts_mut( $(ptr: *mut u8), $(len: usize) )
     };
     let regex = Regex::new(r"^(1(01*0)*1|0)*$").unwrap();

     regex.is_match(input)
   }
  |]
  pure (toBool out)


