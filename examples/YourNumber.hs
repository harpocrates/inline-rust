{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Main where

import Language.Rust.Inline
import Foreign.Ptr
import Data.Int (Int32, Int64)


setContext (basic <> functions)

externCrate "rayon" "0.9"

-- Top-level functions - accessible from all quasiquotes
[rust|
fn hi_from_rust(n: u16) -> () {
  print!("hi");
  for i in 1..n {
    print!(" hi")
  }
  println!()
}

pub fn foo(m: i32) {
    println!("Your number plus 2: {}", m + 1);
}
|]

-- | Print your number, and 2 more, from Rust
yourNumber :: Int32 -> IO ()
yourNumber n = [rustIO|
  () {
     println!("Your number: {}", $(n: i32));
     foo(n + 1)
  }
|]

main = do
  n <- readLn
  yourNumber n
  putStrLn $ "Printing `hi` " ++ show n ++ " times:"
  [rustIO| () { hi_from_rust($(n: i32) as u16) } |]
