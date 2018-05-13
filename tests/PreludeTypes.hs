{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module PreludeTypes where

import Language.Rust.Inline
import Test.Hspec

import Foreign.Storable
import Data.Int  ( Int32 )
import Data.Word   ( Word32 )
import Data.Char  ( toUpper )
import Data.Bifunctor ( bimap )

extendContext prelude 
extendContext basic
setCrateModule

preludeTypes :: Spec
preludeTypes = describe "Common Prelude types" $ do
  it "Can marshal a `Maybe Int32` argument/return" $ do
    let x1, x2 :: Maybe Int32
        x1 = Just 9
        x2 = Nothing
    [rust| Option<i32> { $(x1: Option<i32>).map(|n| n+2) } |] `shouldBe` fmap (+2) x1
    [rust| Option<i32> { $(x2: Option<i32>).map(|n| n+2) } |] `shouldBe` fmap (+2) x2

  it "Can marshal an `Either Int32 Char` argument/return" $ do
    let x1, x2 :: Either Int32 Char
        x1 = Left 9
        x2 = Right 'e'
    
    [rust| Result<char,i32> {
      $(x1: Result<char,i32>)
        .map(|c| c.to_uppercase().next().unwrap())
        .map_err(|n| n+2)
    } |] `shouldBe` bimap (+2) toUpper x1
    
    [rust| Result<char,i32> {
      $(x2: Result<char,i32>)
        .map(|c| c.to_uppercase().next().unwrap())
        .map_err(|n| n+2)
    } |] `shouldBe` bimap (+2) toUpper x2

  it "Can marshal `(Int32, Char)` argument and `(Int32, Char, Word32)` return" $ do
    let x = (-9, 'c')
    
    [rust| (i32, char, u32) { 
      let (n, c) = $(x: (i32, char));
      (n * 3, c.to_uppercase().next().unwrap(), n.abs() as u32)
    } |] `shouldBe` (fst x * 3, toUpper (snd x), fromIntegral (abs (fst x)))

  it "Can marshal `Maybe (Int32, Either Char Word32)` argument/return" $ do
    let x1, x2, x3 :: Maybe (Int32, Either Char Word32)
        x1 = Nothing
        x2 = Just (3, Left 'c')
        x3 = Just (4, Right 8)

    let f x = [rust| Option<(i32, Result<u32,char>)> {
      $(x: Option<(i32, Result<u32,char>)>).map(|x| {
        match x {
          (x1, Ok(x2)) => (x1 * x2 as i32, Err('a')),
          (x1, e) => (x1 - 1, e),
        }
      })
    } |]

    let f' = fmap (\x -> case x of
                           (x1, Right x2) -> (x1 * fromIntegral x2, Left 'a')
                           (x1, e) -> (x1 - 1, e))

    f x1 `shouldBe` f' x1
    f x2 `shouldBe` f' x2
    f x3 `shouldBe` f' x3

