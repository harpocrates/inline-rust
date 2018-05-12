{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module SimpleTypes where

import Language.Rust.Inline
import Test.Hspec

import Data.Int
import Data.Word

extendContext basic
setCrateModule 

simpleTypes :: Spec
simpleTypes = describe "Simple types" $ do
  it "Can marshal a `Char` argument/return" $ do
    let x = 'a'
    [rust| char { $(x: char).to_uppercase().next().unwrap() } |] `shouldBe` 'A'
  
  it "Can marshal a `Int` argument/return" $ do
    let x = -3 :: Int
    [rust| isize { $(x: isize) + 4 } |] `shouldBe` (x + 4)
  
  it "Can marshal an `Int8` argument/return" $ do
    let x = -3 :: Int8
    [rust| i8 { $(x: i8) + 4 } |] `shouldBe` (x + 4)
  
  it "Can marshal an `Int16` argument/return" $ do
    let x = -3 :: Int16
    [rust| i16 { $(x: i16) + 1004 } |] `shouldBe` (x + 1004)
  
  it "Can marshal an `Int32` argument/return" $ do
    let x = -3 :: Int32
    [rust| i32 { $(x: i32) + 100004 } |] `shouldBe` (x + 100004)
  
  it "Can marshal an `Int64` argument/return" $ do
    let x = -3 :: Int64
    [rust| i64 { $(x: i64) + 10000000004 } |] `shouldBe` (x + 10000000004)
  
  it "Can marshal an `Word` argument/return" $ do
    let x = 3 :: Word
    [rust| usize { $(x: usize) + 4 } |] `shouldBe` (x + 4)
  
  it "Can marshal a `Word8` argument/return" $ do
    let x = 3 :: Word8
    [rust| u8 { $(x: u8) + 4 } |] `shouldBe` (x + 4)
  
  it "Can marshal a `Word16` argument/return" $ do
    let x = 3 :: Word16
    [rust| u16 { $(x: u16) + 1004 } |] `shouldBe` (x + 1004)
  
  it "Can marshal a `Word32` argument/return" $ do
    let x = 3 :: Word32
    [rust| u32 { $(x: u32) + 100004 } |] `shouldBe` (x + 100004)
  
  it "Can marshal a `Word64` argument/return" $ do
    let x = 3 :: Word64
    [rust| u64 { $(x: u64) + 10000000004 } |] `shouldBe` (x + 10000000004)

  it "Can marshal a `Float` argument/return" $ do
    let x = 0.3 :: Float
    [rust| f32 { $(x: f32) * 0.7 } |] `shouldBe` (x * 0.7)
  
  it "Can marshal a `Double` argument/return" $ do
    let x = 0.3 :: Double
    [rust| f64 { $(x: f64) * 0.7 } |] `shouldBe` (x * 0.7)

  it "Can marshal a `Bool` argument/return" $ do
    let x = 0 :: Word8
    [rust| bool { !$(x: bool) } |] `shouldBe` (1 :: Word8)
  
--   it "Can marshal a `()` argument/return" $ do
--     let x = ()
--     [rust| () { $(x: ()) } |] `shouldBe` ()
