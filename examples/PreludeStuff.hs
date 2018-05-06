{-# LANGUAGE TemplateHaskell, QuasiQuotes, ExplicitForAll, ScopedTypeVariables, EmptyCase #-}

module Main where

import Language.Rust.Inline
import Language.Rust.Inline.TH

import Foreign.Storable
import Data.Int (Int32)

extendContext basic
extendContext prelude 

setCrateRootContext []

-- Some ADTs
data Point a = Point a a deriving (Show)
mkStorable [t| forall a. Storable a => Storable (Point a) |]
extendContext (rustTyCtx [t| forall a. Point a |])

main = do
  let m1 = Just 1
      m2 = Nothing
      m3 = Just 8

      p1 = Point (Just 1) (Just 8)
      p2 = Point Nothing (Just 9)
      p3 = Point (Just 3) Nothing

  print [rust|
    Result<Point<Option<i32>>,i32> {
      let ms = vec![ $(m1: Option<i32>), $(m2: Option<i32>), $(m3: Option<i32>) ];
      let ps = vec![ $(p1: Point<Option<i32>>), $(p2: Point<Option<i32>>), $(p3: Point<Option<i32>>) ];

      let mut x = 0i32;
      let mut y = 0i32;
      for (&m,&p) in ms.iter().zip(ps.iter()) {
        if let Some(multiplier) = m {
          let Point(mx, my) = p;
          mx.map(|px| x += multiplier * px);
          my.map(|py| y += py);
        }
      }

      Ok(Point(Some(x), Some(y)))
    }
  |]

  print (eGCD 20 8)


eGCD :: Int32 -> Int32 -> (Int32,Int32,Int32)
eGCD a b = [rust| (i32,i32,i32) {
            	let mut x: i32 = $(a: i32);
            	let mut y: i32 = $(b: i32);
            	let mut x0 = 1i32;
            	let mut x1 = 0i32;
            	let mut y0 = 0i32;
            	let mut y1 = 1i32;
            	
            	loop {
            	    let q: i32 = x / y;
            	    
            	    x = x % y;
            	    x0 -= y0 * q;
            	    x1 -= y1 * q;
            	    
            	    if x == 0 { return (y, y0, y1) }
            	    
            	    let q: i32 = y / x;
            	    
            	    y = y % x;
            	    y0 -= x0 * q;
            	    y1 -= x1 * q;
            	    
            	    if y == 0 { return (x, x0, x1) }
            	}
            }
          |]
          
