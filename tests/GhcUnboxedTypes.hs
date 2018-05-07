{-# LANGUAGE QuasiQuotes, TemplateHaskell, MagicHash, UnliftedFFITypes #-}
module GhcUnboxedTypes where

import Language.Rust.Inline
import Test.Hspec

import GHC.Prim
import GHC.Types

extendContext ghcUnboxed
extendContext basic
setCrateModule

ghcUnboxedTypes :: Spec
ghcUnboxedTypes = describe "GHC unboxed types" $ do
  it "Can marshal a Char# argument/return" $ do
    let x = 'a'#
    C# [rust| char { $(x: char).to_uppercase().next().unwrap() } |] `shouldBe` C# 'A'#
  
  it "Can marshal an Int# argument/return" $ do
    let x = -3#
    I# [rust| isize { $(x: isize) + 4 } |] `shouldBe` I# (x +# 4#)
  
  it "Can marshal a Word# argument/return" $ do
    let x = 3##
    W# [rust| usize { $(x: usize) + 4 } |] `shouldBe` W# (x `plusWord#` 4##)
  
  it "Can marshal a Float# argument/return" $ do
    let x = 0.3#
    F# [rust| f32 { $(x: f32) * 0.7 } |] `shouldBe` F# (x `timesFloat#` 0.7#)
  
  it "Can marshal a Double# argument/return" $ do
    let x = 0.3##
    D# [rust| f64 { $(x: f64) * 0.7 } |] `shouldBe` D# (x *## 0.7##)

