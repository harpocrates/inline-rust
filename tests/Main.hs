{-# LANGUAGE TemplateHaskell, QuasiQuotes, CPP #-}

#ifdef darwin_HOST_OS
{-# OPTIONS_GHC -optl-Wl,-all_load #-}
#elif mingw32_HOST_OS
#error TODO
#else
{-# OPTIONS_GHC -optl-Wl,--whole-archive #-}
#endif

module Main where

import Language.Rust.Inline

import SimpleTypes
import GhcUnboxedTypes

import Test.Hspec

extendContext basic
setCrateRootContext []

[rust|
mod GhcUnboxedTypes;
mod SimpleTypes;

pub use GhcUnboxedTypes::*;
pub use SimpleTypes::*;
|]

main :: IO ()
main = do
  hspec $
    describe "Rust quasiquoter" $ do
      simpleTypes
      ghcUnboxedTypes
