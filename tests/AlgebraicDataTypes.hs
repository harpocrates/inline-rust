{-# LANGUAGE QuasiQuotes, TemplateHaskell, ExplicitForAll, ScopedTypeVariables #-}
module AlgebraicDataTypes where

import Language.Rust.Inline
import Language.Rust.Inline.TH
import Language.Rust.Quote
import Test.Hspec

import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

import Data.Bifunctor ( Bifunctor(..) )
import Data.Char      ( toUpper )
import Data.Complex   ( Complex(..) )
import Data.Foldable  ( for_ )
import Data.Int       ( Int8, Int16, Int32, Int64 )


-- | A struct-like ADT where the fields have different sizes
data StructLike = StructLike  Int16 Int64 deriving (Show, Eq)
mkStorable [t| Storable StructLike |]

-- | A struct-like newtype ADT where the field is compound
newtype StructLike2 = StructLike2 (Int16, Int64) deriving (Show, Eq)
mkStorable [t| Storable StructLike2 |]


-- | An ADT where:
-- 
--   * Different variants have different alignment
--   * Variants have other complex structures
--
data Foo
  = Bar
  | Baz Char Int
  | Qux (Complex Float) Char 
  deriving (Show, Eq)
mkStorable [t| Storable Foo |]

-- | An ADT where fields are nested ADTs
data Croc
  = Lob (Maybe Foo) Int
  | Boo Int8 Int8
  deriving (Show, Eq)
mkStorable [t| Storable Croc |]

-- | A polymorphic ADT. (From the @these@ package).
data These a b
  = This a     
  | That b     
  | Both a b
  deriving (Show, Eq)
mkStorable [t| forall a b. (Storable a, Storable b) => Storable (These a b) |]

-- | An ADT that needs more that a 'Word8' to store the tag
data Big a
  = C000 | C001 | C002 | C003 | C004 | C005 | C006 | C007 | C008 | C009
  | C010 | C011 | C012 | C013 | C014 | C015 | C016 | C017 | C018 | C019
  | C020 | C021 | C022 | C023 | C024 | C025 | C026 | C027 | C028 | C029
  | C030 | C031 | C032 | C033 | C034 | C035 | C036 | C037 | C038 | C039
  | C040 | C041 | C042 | C043 | C044 | C045 | C046 | C047 | C048 | C049
  | C050 | C051 | C052 | C053 | C054 | C055 | C056 | C057 | C058 | C059
  | C060 | C061 | C062 | C063 | C064 | C065 | C066 | C067 | C068 | C069
  | C070 | C071 | C072 | C073 | C074 | C075 | C076 | C077 | C078 | C079
  | C080 | C081 | C082 | C083 | C084 | C085 | C086 | C087 | C088 | C089
  | C090 | C091 | C092 | C093 | C094 | C095 | C096 | C097 | C098 | C099
  | C100 | C101 | C102 | C103 | C104 | C105 | C106 | C107 | C108 | C109
  | C110 | C111 | C112 | C113 | C114 | C115 | C116 | C117 | C118 | C119
  | C120 | C121 | C122 | C123 | C124 | C125 | C126 | C127 | C128 | C129
  | C130 | C131 | C132 | C133 | C134 | C135 | C136 | C137 | C138 | C139
  | C140 | C141 | C142 | C143 | C144 | C145 | C146 | C147 | C148 | C149
  | C150 | C151 | C152 | C153 | C154 | C155 | C156 | C157 | C158 | C159
  | C160 | C161 | C162 | C163 | C164 | C165 | C166 | C167 | C168 | C169
  | C170 | C171 | C172 | C173 | C174 | C175 | C176 | C177 | C178 | C179
  | C180 | C181 | C182 | C183 | C184 | C185 | C186 | C187 | C188 | C189
  | C190 | C191 | C192 | C193 | C194 | C195 | C196 | C197 | C198 | C199
  | C200 | C201 | C202 | C203 | C204 | C205 | C206 | C207 | C208 | C209
  | C210 | C211 | C212 | C213 | C214 | C215 | C216 | C217 | C218 | C219
  | C220 | C221 | C222 | C223 | C224 | C225 | C226 | C227 | C228 | C229
  | C230 | C231 | C232 | C233 | C234 | C235 | C236 | C237 | C238 | C239
  | C240 | C241 | C242 | C243 | C244 | C245 | C246 | C247 | C248 | C249
  | C250 | C251 | C252 | C253 | C254 | C255 | C256 | C257 | C258 | C259
  | C260 | C261 | C262 | C263 | C264 | C265 | C266 | C267 | C268 | C269
  | C270 | C271 | C272 | C273 | C274 | C275 | C276 | C277 | C278 | C279
  | C280 | C281 | C282 | C283 | C284 | C285 | C286 | C287 | C288 | C289
  | C290 | C291 | C292 | C293 | C294 | C295 | C296 | C297 | C298 | C299 a
  deriving (Show, Eq)
mkStorable [t| forall a. Storable a => Storable (Big a) |]

-- | An ADT with a mixture of polymorphism and not.
data Foo2 a
  = Bar2
  | Baz2 Word
  | Qux2 a a
  | Quux2 Int a
  deriving (Show, Eq)
mkStorable [t| forall a. Storable a => Storable (Foo2 a) |]


-- Set the context
extendContext basic
extendContext prelude 
extendContext (singleton [ty| Cpx<f32> |] [t| Complex Float |])
extendContext (rustTyCtx [t| Foo |])
extendContext (rustTyCtx [t| StructLike |])
extendContext (rustTyCtx [t| StructLike2 |])
extendContext (rustTyCtx [t| Croc |])
extendContext (rustTyCtx [t| forall a b. These a b |])
extendContext (rustTyCtx [t| forall a. Big a |])
extendContext (rustTyCtx [t| forall a. Foo2 a |])
setCrateModule

[rust|
#[derive(Copy,Clone)]
#[repr(C)]
pub struct Cpx<T> { re: T, im: T }

use std::ops::Add;
impl<T: Add<Output=T>> Add for Cpx<T> {
  type Output = Cpx<T>;
  
  fn add(self, rhs: Self) -> Self {
    Cpx {
      re: self.re + rhs.re,
      im: self.im + rhs.im,
    }
  }
}
|]


-- | Function on 'Foo'
quux :: Foo -> Foo
quux (Baz 'a' 0) = Bar
quux (Baz c i) = Baz 'l' (i+1)
quux (Qux (_ :+ im) c) = Qux (9.0 :+ im) c
quux f = f

[rust|
impl Foo {
  pub fn quux(self) -> Self {
    match self {
      Foo::Baz('a', 0) => Foo::Bar,
      Foo::Baz(c, i) => Foo::Baz('l', i+1),
      Foo::Qux(Cpx { im, .. }, c) => Foo::Qux(Cpx { re: 9f32, im }, c),
      f => f,
    }
  }
}
|]


-- | Function on 'Croc'
croc :: Croc -> Croc
croc (Lob mfoo i) = Lob (fmap quux mfoo) (i + 4)
croc (Boo i1 i2) = Boo (negate i1) (abs i2)

[rust|
impl Croc {
  pub fn croc(self) -> Self {
    match self {
      Croc::Lob(mfoo, i) => Croc::Lob(mfoo.map(|x| x.quux()), i + 4),
      Croc::Boo(i1, i2) => Croc::Boo(-i1, i2.abs()),
    }
  }
}
|]


-- | Function 1 on 'StructLike'/'StructLike2'
in2 :: StructLike -> StructLike2
in2 (StructLike f i) = StructLike2 (f * 2, i * 3 - 8)

-- | Function 2 on 'StructLike'/'StructLike2'
out2 :: StructLike2 -> StructLike
out2 (StructLike2 (f, i)) = StructLike (f `quot` 2) ((i + 8) `quot` 3)

[rust|
impl StructLike {
  pub fn in2(self) -> StructLike2 {
    let StructLike(f,i) = self;
    StructLike2((f * 2, i * 3 - 8))
  }
}

impl StructLike2 {
  pub fn out2(self) -> StructLike {
    let StructLike2((f,i)) = self;
    StructLike(f / 2, (i + 8) / 3)
  }
}
|]


instance Bifunctor These where
  bimap f _ (This x) = This (f x)
  bimap _ g (That y) = That (g y)
  bimap f g (Both x y) = Both (f x) (g y)

[rust|
impl<A, B> These<A, B> {
  pub fn bimap<C, D, F, G>(self, f: F, g: G) -> These<C, D>
  where
    F: FnOnce(A) -> C,
    G: FnOnce(B) -> D,
  {
    match self {
      These::This(x) => These::This(f(x)),
      These::That(y) => These::That(g(y)),
      These::Both(x, y) => These::Both(f(x), g(y)),
    }
  }
}
|]


algebraicDataTypes :: Spec
algebraicDataTypes = describe "Algebraic data types" $ do
  it "Can marshal a `Complex Float` argument/return" $ do
    let z1, z2 :: Complex Float 
        z1 = 1.3 :+ 4.5 
        z2 = 6.7 :+ 8.9
    [rust| Cpx<f32> { $(z1: Cpx<f32>) + $(z2: Cpx<f32>) } |] `shouldBe` z1 + z2

  it "Can marshal a custom single-constructor ADT argument/return" $ do
    let s1 = StructLike 78 (negate 267)
        s2 = StructLike 92 45223
        s3 = StructLike2 (34, -92391)
        s4 = StructLike2 (576, 1234) 
    
    for_ [s1,s2] $ \si ->
        [rust| StructLike2 { $(si: StructLike).in2() } |] `shouldBe` in2 si
    for_ [s3,s4] $ \si ->
        [rust| StructLike { $(si: StructLike2).out2() } |] `shouldBe` out2 si

  it "Can marshal a custom monomorphic ADT argument/return" $ do
    let f1, f2, f3, f4 :: Foo
        f1 = Baz 'a' 0
        f2 = Baz 'b' 2
        f3 = Qux (7.1 :+ 3.4) 'f'
        f4 = Bar

    for_ [f1,f2,f3,f4] $ \fi ->
      [rust| Foo { $(fi: Foo).quux() } |] `shouldBe` quux fi

  it "Can marshal nested monomorphic ADT arguments/returns" $ do
    let c1, c2, c3, c4, c5, c6, c7 :: Croc
        c1 = Lob (Just (Baz 'a' 0)) 2
        c2 = Lob (Just (Baz 'b' 2)) 6
        c3 = Lob (Just (Qux (7.1 :+ 3.4) 'f')) 8
        c4 = Lob (Just Bar) 9
        c5 = Lob Nothing 3
        c6 = Boo 3 (-2)
        c7 = Boo (-4) 2

    for_ [c1,c2,c3,c4,c5,c6,c7] $ \ci ->
      [rust| Croc { $(ci: Croc).croc() } |] `shouldBe` croc ci

  it "Can marshal polymorphic ADT arguments/returns" $ do
    let t1, t2, t3 :: These Int8 Int64
        t1 = This maxBound
        t2 = That 432442
        t3 = Both (maxBound - 3) 879

    for_ [t1,t2,t3] $ \ti ->
      let v1 = [rust| These<i16,i64> {
                 $(ti: These<i8,i64>).bimap(|x| x as i16 * 2, |y| y + 2)
               } |]
          v2 = bimap (\x -> fromIntegral x * 2) (+ 2) ti
      in v1 `shouldBe` v2

  it "Can marshal nested polymorphic ADT arguments/returns" $ do
    let t1, t2, t3 :: These (Maybe Int) (These Int8 Int8)
        t1 = This (Just 6)
        t2 = This Nothing
        t3 = That (This 8)
        t4 = That (That 9)
        t5 = That (Both 1 2)
        t6 = Both (Just 3) (That 8)
        t7 = Both Nothing (Both 3 5)
        t8 = Both (Just 213) (Both 78 98)

    for_ [t1,t2,t3,t4,t5,t6,t7,t8] $ \ti ->
      let v1 = [rust| These<Option<isize>,These<i8,i8>> {
                 $(ti: These<Option<isize>,These<i8,i8>>).bimap(
                    |oi| oi.map(|i| i + 5),
                    |t| t.bimap(|i| i + 2, |j| j * 3),
                 )
               } |]
          v2 = bimap (fmap (+5)) (bimap (+2) (*3)) ti
      in v1 `shouldBe` v2

  it "Can marshal a big ADT whose tag needs more than a `Word8`" $ do
    let b1, b2, b3, b4 :: Big Int64
        b1 = C000
        b2 = C160
        b3 = C298
        b4 = C299 89

    for_ [b1,b2,b3,b4] $ \bi ->
      let v1 = [rust| Big<i64> {
                  match $(bi: Big<i64>) {
                    Big::C160 => Big::C161,
                    Big::C299(i) => Big::C299(i+1),
                    b => b,
                  }
               } |]
          v2 = case bi of
                 C160 -> C161
                 C299 i -> C299 (i + 1)
                 b -> b
      in v1 `shouldBe` v2

  it "Can marshal a custom `Foo2 Int` and `Foo2 (Foo2 Int)` return" $ do
    let f1, f2, f3, f4 :: Foo2 Int
        f1 = Bar2
        f2 = Baz2 3
        f3 = Qux2 (-1) 2
        f4 = Quux2 (-8) 3
    
    let fooed f = case f of 
                    Qux2 x y -> Qux2 (Qux2 x y) (Qux2 y x)
                    Quux2 i x -> Quux2 (i + 1) (Qux2 x x)
                    Bar2 -> Bar2
                    Baz2 w -> Baz2 w

    let fooed' f = [rust| Foo2<Foo2<isize>> {
      match $(f: Foo2<isize>) {
        Foo2::Qux2(x,y) => Foo2::Qux2(Foo2::Qux2(x,y), Foo2::Qux2(y,x)),
        Foo2::Quux2(i,x) => Foo2::Quux2(i+1, Foo2::Qux2(x,x)),
        Foo2::Bar2 => Foo2::Bar2,
        Foo2::Baz2(w) => Foo2::Baz2(w),
      }
    } |]

    fooed f1 `shouldBe` fooed' f1
    fooed f2 `shouldBe` fooed' f2
    fooed f3 `shouldBe` fooed' f3
    fooed f4 `shouldBe` fooed' f4

