{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Main where

import Language.Rust.Inline
import Data.Int

extendContext basic

setCrateRoot []

main = do
  putStrLn "Haskell: Hello. Enter a number:"
  x <- readLn
  y <- [rustIO| i32 {
    println!("Rust: Your number is {}", $(x: i32));
    $(x: i32) + 1
  } |]
  putStrLn $ "Haskell: Rust says number plus 1 is " ++ show y

