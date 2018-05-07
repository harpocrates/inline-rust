{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Main where

import Language.Rust.Inline  ( rust, rustIO, extendContext, setCrateRoot, basic, pointers, libc )
import Foreign.Marshal       ( free, withArrayLen )
import Foreign.C.String      ( newCString, peekCString )

extendContext basic
extendContext pointers
extendContext libc

setCrateRoot
  [ ("libc", "*")
  , ("rayon", "0.9")
  ]

[rust|
extern crate libc;
extern crate rayon;
|]

main :: IO ()
main = do

  -- [Integers](http://jakegoulding.com/rust-ffi-omnibus/integers/)
  print $ let a = 1
              b = 2
          in [rust| libc::uint32_t { $(a: libc::uint32_t) + $(b: libc::uint32_t) } |]

  -- [String Arguments](http://jakegoulding.com/rust-ffi-omnibus/string_arguments/)
  str <- newCString "göes to élevên"
  let n = [rust| libc::uint32_t {
    use std::ffi::CStr;

    let c_str = unsafe {
      let s = $(str: *const libc::c_char);
      assert!(!s.is_null());
      CStr::from_ptr(s)
    };

    let r_str = c_str.to_str().unwrap();
    r_str.chars().count() as libc::uint32_t
  } |]
  putStrLn $ "There are " ++ show n ++ " characters"
  free str

  -- [String Return Values](http://jakegoulding.com/rust-ffi-omnibus/string_return/)
  -- TODO: unicode
  let n = 8
  str <- [rustIO| *mut libc::c_char {
    use std::ffi::CString;
    use std::iter;

    let mut song = String::from("💣 ");
    song.extend(iter::repeat("na ").take( $(n: usize) ));
    song.push_str("Batman! 💣 ");

    let c_str_song = CString::new(song).unwrap();
    c_str_song.into_raw()
  } |]
  song <- peekCString str
  [rustIO| () {
    use std::ffi::CString;
    
    unsafe {
      CString::from_raw( $(str: *mut libc::c_char) )
    };
  } |]
  putStrLn song

  -- [Slice Arguments](http://jakegoulding.com/rust-ffi-omnibus/slice_arguments/)
  -- TODO: argument used twice rust
  withArrayLen [1,2,3,4,5,6] $ \len arr ->
    print [rust| isize {
      use std::slice;

      let numbers = unsafe {
        let a = $(arr: *const isize);
        assert!(!a.is_null());

        slice::from_raw_parts(a, $(len: isize) as usize)
      };

      numbers.iter()
             .filter(|&v| v % 2 == 0)
             .fold(0, |acc, v| acc + v)
    } |]
 
