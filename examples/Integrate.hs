{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Main where

import Language.Rust.Inline
import Data.Int (Int32, Int64)

extendContext basic
extendContext functions

setCrateRoot [("rayon", "0.9")]

[rust|
extern crate rayon;

use rayon::prelude::*;
|]

main = do
  print (integrate (0, pi) (\x -> sin x)              1000)
  print (integrate (0, 5)  (\x -> x^2 + 5 * x - 3)    1000) 

-- | Take a double integral
integrate :: (Double, Double) -> (Double -> Double) -> Int32 -> Double
integrate (lo, hi) func n = unsafeLocalState $
  $(withFunPtr [t| Double -> Double |]) func $ \func1 ->
    [rustIO|
     f64 {
            let f = $( func1: extern "C" fn(f64) -> f64 );
            let delta = ( $(hi: f64) - $(lo: f64) ) / $(n: i32) as f64;

            (0..n)
              .into_par_iter()
              .map(|i| {
                let a = i as f64 * delta;
                delta * (f(a) + f(a + delta)) / 2f64
              })
              .sum()
     }
    |]

