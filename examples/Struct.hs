{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Main where

import Language.Rust.Quote    ( ty )
import Language.Rust.Inline
import Foreign.Marshal.Unsafe ( unsafeLocalState )
import Foreign.Ptr            ( Ptr )
import Foreign.Storable       ( peek )
import Foreign.Marshal        ( with )

import Data.Complex           ( Complex(..) )

-- We add to our context the knowledge that a Rust pointer to a 'Complex' is
-- the same as a Haskell 'Ptr' to a 'Complex Float'.
extendContext basic
extendContext (singleton [ty| *mut Complex |] [t| Ptr (Complex Float) |])

setCrateRoot []

-- Rust representation of Haskell's 'Complex'
[rust|
#[repr(C)]
pub struct Complex {
  re: f32,
  im: f32,
}
|]

-- | Add two complex numbers in Rust
add :: Complex Float -> Complex Float -> Complex Float
add z1 z2 = unsafeLocalState $
  with z1 $ \ptr1 ->
    with z2 $ \ptr2 -> do
      ptr3 <- [rustIO| *mut Complex {
        let z1: *mut Complex = $(ptr1: *mut Complex);
        let z2: *mut Complex = $(ptr2: *mut Complex);

        let z3: Box<Complex> = Box::new(unsafe {
          Complex {
            re: (*z1).re + (*z2).re,
            im: (*z1).im + (*z2).im,
          }
        });
        Box::into_raw(z3)
      } |]
      z3 <- peek ptr3
      [rustIO| () {
        unsafe { Box::from_raw($(ptr3: *mut Complex)) };
      } |]
      pure z3


main = let z1 = 1.3 :+ 2.4
           z2 = 7.1 :+ 3.9
           z3 = add z1 z2
       in putStrLn $ unwords [ show z1, "+", show z2, "=", show z3 ]

