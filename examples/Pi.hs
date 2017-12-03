{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Main where

import Language.Rust.Inline
import Data.Int

setContext (basic <> functions)
externCrate "rayon" "0.9"

[rust|
use rayon::prelude::*;
|]

-- | Approximate π using the first 'n' terms of the series.
--
-- \[
--    \frac{\pi}{4} = 1 - \frac{1}{3} + \frac{1}{5} - \frac{1}{7} + \frac{1}{9} - \cdots
-- \]
piApprox :: Int64 -> Double
piApprox x = [rust|
  f64 {
    let mut approximation = 0.0;
    let mut multiplier = 1.0;

    for i in (0 .. $(x: i64)) {
      approximation += multiplier / (2 * i + 1) as f64;
      multiplier *= -1.0;
    }

    4.0 * approximation
  }
|]

-- | Approximate π up to 'n' hex decimal points using the Bailey–Borwein–Plouffe formula.
--
-- \[
--    \pi = \sum_{k=0}^\infty \frac{1}{16^k} \cdot \left( 
--            \frac{4}{8k + 1} - \frac{2}{8k + 4} - \frac{1}{8k + 5} - \frac{1}{8k + 6}
--          \right)
-- \]
--
-- Do this in parallel using `rayon`!
piBbpRayon :: Int32 -> Double
piBbpRayon n = [rust|
  f64 {
    (0 .. $(n: i32))
      .into_par_iter()
      .map(|k| {
        let a = 4.0 / (8*k + 1) as f64;
        let b = 2.0 / (8*k + 4) as f64;
        let c = 1.0 / (8*k + 5) as f64;
        let d = 1.0 / (8*k + 6) as f64;
          
        (a - b - c - d) / f64::powi(16.0, k)
      })
      .sum()
  }
|]

main = do
  putStrLn $ "π (alternating) ~ " ++ show (piApprox 10000)
  putStrLn $ "π (BBP)         ~ " ++ show (piBbpRayon 10)
