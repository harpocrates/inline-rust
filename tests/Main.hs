module Main where

import Language.Rust.Inline

import SimpleTypes
import GhcUnboxedTypes

import Test.Hspec

main :: IO ()
main = hspec $
  describe "Rust quasiquoter" $ do
    simpleTypes
    ghcUnboxedTypes
