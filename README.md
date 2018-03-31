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

If you want to use this with GHCi, make sure to pass in `-fobject-code`.

## Building

This currently depends on a GHC [feature][1] that will be available in GHC 8.6.
Until then, you'll have to [build GHC][0] yourself. Assuming you've done this,
you should be able to build with `new-build`.

    $ cabal new-build -w /usr/local/bin/ghc-head

Running the examples is only a matter of threading through the right package
databases.

    $ /usr/local/bin/ghc-head \
      -package-db $HOME/.cabal/store/ghc-head/package.db \
      -package-db dist-newstyle/packagedb/ghc-head \
      -threaded \
      examples/Hello.hs
    [1 of 1] Compiling Main             ( examples/Hello.hs, examples/Hello.o )
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
