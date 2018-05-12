{-# LANGUAGE TemplateHaskell, QuasiQuotes, CPP #-}

#ifdef darwin_HOST_OS
{-# OPTIONS_GHC -optl-Wl,-all_load #-}
#else
{-# OPTIONS_GHC -optl-Wl,--whole-archive #-}
#endif

module Main where

import Language.Rust.Inline

import SimpleTypes
import GhcUnboxedTypes
import PointerTypes

import Test.Hspec

extendContext basic
setCrateRoot []

[rust|
mod GhcUnboxedTypes;
mod SimpleTypes;
mod PointerTypes;

pub use GhcUnboxedTypes::*;
pub use SimpleTypes::*;
pub use PointerTypes::*;
|]

main :: IO ()
main = hspec $
  describe "Rust quasiquoter" $ do
    simpleTypes
    ghcUnboxedTypes
    pointerTypes
