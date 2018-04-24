module Main where

import Language.Rust.Inline
import SimpleTypes
import Test.Hspec

main :: IO ()
main = hspec $
  describe "Rust quasiquoter" $ do
    simpleTypes
