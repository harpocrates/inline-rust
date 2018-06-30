{-# LANGUAGE QuasiQuotes, TemplateHaskell, ExplicitForAll, ScopedTypeVariables #-}
module CompoundTypes where

import Language.Rust.Inline
import Language.Rust.Inline.TH
import Language.Rust.Quote
import Test.Hspec

import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal.Utils  ( new )
import Foreign.Marshal.Array  ( newArray )
import System.IO              ( fixIO )
import Data.Word              ( Word )

import Data.Complex           ( Complex(..) )

data Foo a
  = Bar
  | Baz Word
  | Qux a a
  | Quux Int a
  deriving (Show, Eq)

mkStorable [t| forall a. Storable a => Storable (Foo a) |]

extendContext pointers 
extendContext (singleton [ty| Complex |] [t| Complex Float |])
extendContext basic
extendContext (rustTyCtx [t| forall a. Foo a |])
setCrateModule

-- Rust complex number
[rust|
#[repr(C)]
pub struct Complex { re: f32, im: f32 }

use std::ops::Add;
impl Add for Complex {
  type Output = Complex;
  fn add(self, other: Complex) -> Complex {
    Complex {
      re: self.re + other.re,
      im: self.im + other.im,
    }
  }
}
|]


compoundTypes :: Spec
compoundTypes = describe "Compound types" $ do
  it "Can marshal a `Complex Float` argument/return" $ do
    let c1, c2 :: Complex Float
        c1 = 1.9 :+ 2.3
        c2 = 8.1 :+ 4.7

    [rust| Complex { $(c1: Complex) + $(c2: Complex) } |] `shouldBe` (c1 + c2)

  it "Can marshal a custom `Foo Int` and `Foo (Foo Int)` return" $ do
    let f1, f2, f3, f4 :: Foo Int
        f1 = Bar
        f2 = Baz 3
        f3 = Qux (-1) 2
        f4 = Quux (-8) 3
    
    let fooed f = case f of 
                    Qux x y -> Qux (Qux x y) (Qux y x)
                    Quux i x -> Quux (i + 1) (Qux x x)
                    Bar -> Bar
                    Baz w -> Baz w

    let fooed' f = [rust| Foo<Foo<isize>> {
      match $(f: Foo<isize>) {
        Foo::Qux(x,y) => Foo::Qux(Foo::Qux(x,y), Foo::Qux(y,x)),
        Foo::Quux(i,x) => Foo::Quux(i+1, Foo::Qux(x,x)),
        Foo::Bar => Foo::Bar,
        Foo::Baz(w) => Foo::Baz(w),
      }
    } |]

    fooed f1 `shouldBe` fooed' f1
    fooed f2 `shouldBe` fooed' f2
    fooed f3 `shouldBe` fooed' f3
    fooed f4 `shouldBe` fooed' f4
