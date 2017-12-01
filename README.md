# inline-rust

This package allows you to write Rust inline in your Haskell source using quasiquotes. For example, the following compiles and outputs `3.1414926535900345`.

```haskell
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Main where

import Language.Rust.Inline

import Data.Int

externCrate "rayon" "0.9"

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
piBbpRayon :: Int64 -> Double
piBbpRayon n = [rust|
                f64 {

                  (0 .. $(n: i64))
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
  print (piApprox 10000)
  print (piBbpRayon 10000)
```

This only works with simple types for now. If you want to use this with GHCi, make sure to pass in `-fobject-code`.

## Building

This currently depends on

  * A [special branch of GHC](https://github.com/harpocrates/ghc/tree/feature/th-foreign-objects) ([Phabricator ticket](https://phabricator.haskell.org/D4217))
  * My soon-to-be-released [`language-rust`](https://github.com/harpocrates/language-rust) package
  
You should be able to do something like:

    $ git clone https://github.com/harpocrates/ghc.git
    $ git clone https://github.com/harpocrates/language-rust.git
    $ git clone https://github.com/harpocrates/inline-rust.git
    
    $ cd ghc
    ghc$ git checkout th-foreign-objects
    ghc$ ./configure && make -j4.             # Good luck. See the docs on building GHC
    
    $ cd ../inline-rust
    inline-rust$ cabal sandbox init
    inline-rust$ cabal sandbox add-source ../language-rust
    inline-rust$ cabal install --only-dependencies -w ../ghc/inplace/bin/ghc-stage2  # put full GHC path
    inline-rust$ cabal install -w ../ghc/inplace/bin/ghc-stage2                      # put full GHC path

## Bugs

Please report bugs to [the issue tracker](https://github.com/harpocrates/inline-rust/issues)
  

