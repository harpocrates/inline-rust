{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module FunctionPointerTypes where

import Language.Rust.Inline
import Test.Hspec

import Foreign.Storable
import Foreign.Ptr
import Data.Word              ( Word )
import Data.Char              ( ord )

extendContext functions 
extendContext basic
setCrateModule

[rust|
extern fn foo(x: isize) -> isize { 2*x + 3 }
extern fn bar(w: usize, c: char) -> isize { w as isize + c as isize }
|]


funcPointerTypes :: Spec
funcPointerTypes = describe "Function pointer types" $ do
  it "Can marshal a `FunPtr (Int -> Int)` argument" $ do
    let foo x = 2*x + 3
    x <- $(withFunPtr [t| Int -> Int |]) foo $ \fooPtr ->
      [rustIO| isize {
        let foo = $( fooPtr: extern "C" fn(isize) -> isize );
        foo(4) + foo(9)
      } |]
    let x' = foo 4 + foo 9
    x `shouldBe` x'

  it "Can marshal a `FunPtr (Int -> Int)` return" $ do
    let fooPtr = [rust| extern fn(isize) -> isize { foo } |]
    let foo = $(unFunPtr [t| Int -> Int |]) fooPtr
    [rust| isize { foo(4) } |] `shouldBe` foo 4


  it "Can marshal a `FunPtr (Word -> Char -> Int)` argument" $ do
    let bar :: Word -> Char -> Int
        bar w c = fromIntegral w + 8 * ord c
    x <- $(withFunPtr [t| Word -> Char -> Int |]) bar $ \barPtr ->
      [rustIO| isize {
        let bar = $( barPtr: extern "C" fn(usize,char) -> isize );
        bar(4,'a') + bar(9,'o')
      } |]
    let x' = bar 4 'a' + bar 9 'o'
    x `shouldBe` x'

  it "Can marshal a `FunPtr (Word -> Char -> Int)` return" $ do
    let barPtr = [rust| extern fn(usize,char) -> isize { bar } |]
    let bar = $(unFunPtr [t| Word -> Char -> Int |]) barPtr
    [rust| isize { bar(4, 'a') } |] `shouldBe` bar 4 'a'
    
