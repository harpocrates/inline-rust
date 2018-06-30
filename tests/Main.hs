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
import FunctionPointerTypes
import PreludeTypes
import CompoundTypes

import Test.Hspec

extendContext basic
setCrateRoot []

[rust|
mod GhcUnboxedTypes;
mod SimpleTypes;
mod PointerTypes;
mod FunctionPointerTypes;
mod PreludeTypes;
mod CompoundTypes;

pub use GhcUnboxedTypes::*;
pub use SimpleTypes::*;
pub use PointerTypes::*;
pub use FunctionPointerTypes::*;
pub use PreludeTypes::*;
pub use CompoundTypes::*;
|]

main :: IO ()
main = hspec $
  describe "Rust quasiquoter" $ do
    simpleTypes
    ghcUnboxedTypes
    pointerTypes
    funcPointerTypes
    preludeTypes
    compoundTypes
