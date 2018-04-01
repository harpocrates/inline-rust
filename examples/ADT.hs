{-# LANGUAGE TemplateHaskell, QuasiQuotes, ExplicitForAll, ScopedTypeVariables, EmptyCase #-}

module Main where

import Language.Rust.Inline
import Language.Rust.Inline.TH

import Foreign.Storable

-- Some ADTs
data Point a = Point a a deriving (Show)
-- data Maybe ...  {- already defined in 'Data.Maybe'
-- data Either ... {- already defined in 'Data.Either'

-- Make some 'Storable' instances
mkStorable [t| forall a. Storable a => Storable (Point a) |]

-- Generate corresponding Rust types
extendContext (rustTyCtx [t| forall a. Point a |])
extendContext (rustTyCtx [t| forall a. Maybe a |])
extendContext (rustTyCtx [t| forall e a. Either e a |])

-- Some Rust machinery
[rust|
impl<T> Maybe<T> {
  pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Maybe<U> {
    match self {
      Maybe::Just(x) => Maybe::Just(f(x)),
      Maybe::Nothing => Maybe::Nothing,
    }
  }
  
  pub fn and_then<U, F: FnOnce(T) -> Maybe<U>>(self, f: F) -> Maybe<U> {
    match self {
      Maybe::Just(x) => f(x),
      Maybe::Nothing => Maybe::Nothing,
    }
  }

  pub fn unwrap_or(self, def: T) -> T {
    match self {
      Maybe::Just(x) => x,
      Maybe::Nothing => def,
    }
  }
}

impl<E,T> Either<E,T> {
  pub fn right(self) -> Maybe<T> {
    match self {
      Either::Right(x) => Maybe::Just(x),
      Either::Left(_) => Maybe::Nothing,
    }
  }

  pub fn and_then<U, F: FnOnce(T) -> Either<E, U>>(self, op: F) -> Either<E, U> {
    match self {
      Either::Right(t) => op(t),
      Either::Left(e) => Either::Left(e),
    }
  }
}
|]

main = do
  let m1 = Just 1
      m2 = Nothing
      m3 = Just 8

      p1 = Point (Just 1) (Just 8)
      p2 = Point Nothing (Just 9)
      p3 = Point (Just 3) Nothing

  print [rust|
    Maybe<Point<Maybe<i32>>> {
      let ms = vec![ $(m1: Maybe<i32>), $(m2: Maybe<i32>), $(m3: Maybe<i32>) ];
      let ps = vec![ $(p1: Point<Maybe<i32>>), $(p2: Point<Maybe<i32>>), $(p3: Point<Maybe<i32>>) ];

      let mut x = 0i32;
      let mut y = 0i32;
      for (&m,&p) in ms.iter().zip(ps.iter()) {
        if let Maybe::Just(multiplier) = m {
          let Point(mx, my) = p;
          mx.map(|px| x += multiplier * px);
          my.map(|py| y += py);
        }
      }

      Maybe::Just(Point(Maybe::Just(x), Maybe::Just(y)))
    }
  |]

