# inline-rust

This package allows you to write Rust inline in your Haskell source using
quasiquotes. Here is a short example. For more examples, check out the
[examples](examples) folder.

```haskell
-- examples/Hello.hs
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Main where

import Language.Rust.Inline
import Data.Int

main = do
  putStrLn "Haskell: Hello. Enter a number:"
  x <- readLn
  y <- [rustIO| i32 {
    let x = $(x: i32);
    println!("Rust: Your number is {}", x);
    x + 1
  } |]
  putStrLn $ "Haskell: Rust says number plus 1 is " ++ show y

```

This works with a limited number of types for now. If you want to use this with
GHCi, make sure to pass in `-fobject-code`.

## Building

This currently depends on

  * A [special branch of GHC][0] ([Phabricator ticket][1])
  * My soon-to-be-released [`language-rust`][2] package
  
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

Please report bugs to [the issue tracker][4]


[0]: https://github.com/harpocrates/ghc/tree/feature/th-foreign-objects
[1]: https://phabricator.haskell.org/D4217
[2]: https://github.com/harpocrates/inline-rust/tree/master/examples
[3]: https://github.com/harpocrates/language-rust
[4]: https://github.com/harpocrates/inline-rust/issues
