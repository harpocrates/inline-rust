# inline-rust

[![Build Status][5]][6]
[![Windows build status][7]][8]

This package allows you to write Rust inline in your Haskell source using
quasiquotes. Here is a short example. For more examples, check out the
[examples](examples) folder.

```haskell
-- examples/Hello.hs
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
    let x = $(x: i32);
    println!("Rust: Your number is {}", x);
    x + 1
  } |]
  putStrLn $ "Haskell: Rust says number plus 1 is " ++ show y

```

If you want to use this with GHCi, make sure to pass in `-fobject-code`.

## Building

This depends ghc >= 8.6. An easy way to run the examples is to use environment files:

    $ cabal build --write-ghc-environment-files=always
    $ ghc -threaded examples/Hello.hs
    [1 of 1] Compiling Main             ( examples/Hello.hs, examples/Hello.o )
    ...
    Linking examples/Hello ...
    $ ./examples/Hello
    Haskell: Hello. Enter a number:
    42
    Rust: Your number is 42
    Haskell: Rust says number plus 1 is 43

## Bugs

Please report bugs to [the issue tracker][4]

[0]: https://ghc.haskell.org/trac/ghc/wiki/Building
[1]: https://phabricator.haskell.org/D4217
[2]: https://github.com/harpocrates/inline-rust/tree/master/examples
[3]: https://github.com/harpocrates/language-rust
[4]: https://github.com/harpocrates/inline-rust/issues
[5]: https://travis-ci.org/harpocrates/inline-rust.svg?branch=master 
[6]: https://travis-ci.org/harpocrates/inline-rust
[7]: https://ci.appveyor.com/api/projects/status/xiwf8743n2f7n400?svg=true
[8]: https://ci.appveyor.com/project/harpocrates/inline-rust
