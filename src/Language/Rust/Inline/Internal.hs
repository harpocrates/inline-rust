{-|
Module      : Language.Rust.Inline.Internal
Description : Manages the module-level state
Copyright   : (c) Alec Theriault, 2017
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : GHC
-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Rust.Inline.Internal (
  emitCodeBlock,
  getRType,
  getHType,
  getContext,
  extendContext,
  initCodeBlocks,
  setCrateRoot,
  setCrateModule,
) where

import Language.Rust.Inline.Context

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Control.Monad               ( when ) 
import Data.Typeable               ( Typeable )
import Data.Maybe                  ( fromMaybe )
import Data.List                   ( unfoldr )

import System.FilePath             ( (</>), (<.>), takeExtension )
import System.Directory            ( copyFile, createDirectoryIfMissing )
import System.Process              ( spawnProcess, readProcess, waitForProcess )
import System.Exit                 ( ExitCode(..) )

import Text.JSON

-- | Represents blocks of code to be emitted
newtype CodeBlocks = CodeBlocks { reversedCodeBlocks :: [String] }
  deriving ( Typeable )


-- | Figure out what file we are currently in.
--
-- TODO: package part of this is buggy
currentFile :: Q ( String    -- ^ package
                 , [String]  -- ^ dot-delimited segments of module name
                 )
currentFile = do
  Module (PkgName pkg) (ModName modName) <- thisModule
  pure (pkg, splitDots modName)
  where splitDots = unfoldr splitDot
        splitDot s | null s = Nothing
                   | otherwise = let (x,r) = break (== '.') s in Just (x,drop 1 r)


-- * Finalizers

-- | A finalizer to run Cargo and link in the static library.
cargoFinalizer :: [String]           -- ^ Extra @rustc@ arguments
                  -> [(String, String)] -- ^ Dependencies
                  -> Q ()
cargoFinalizer rustcArgs dependencies = do
  (pkg, mods) <- currentFile

  let dir = ".inline-rust" </> pkg
      thisFile = foldr1 (</>) mods <.> "rs"
      crate = "quasiquote_" ++ pkg

  -- Make contents of a @Cargo.toml@ file
  let cargoToml = dir </> "Cargo" <.> "toml"
      cargoSrc = unlines [ "[package]"
                         , "name = \"" ++ crate ++ "\""
                         , "version = \"0.0.0\""

                         , "[dependencies]"
                         , unlines [ name ++ " = \"" ++ version ++ "\""
                                   | (name, version) <- dependencies
                                   ]

                         , "[lib]"
                         , "path = \"" ++ thisFile ++ "\""
                         , "crate-type = [\"staticlib\"]"
                         ]
  runIO $ createDirectoryIfMissing True dir
  runIO $ writeFile cargoToml cargoSrc

  -- Run Cargo to compile the project
  let cargoArgs = [ "build"
                  , "--release"
                  , "--manifest-path=" ++ cargoToml
                  ] ++ rustcArgs 
      msgFormat = [ "--message-format=json" ]

  ec <- runIO $ spawnProcess "cargo" cargoArgs >>= waitForProcess
  when (ec /= ExitSuccess)
    (reportError rustcErrMsg)
 
  -- Run Cargo again to get the static library path
  jOut <- runIO $ readProcess "cargo" (cargoArgs ++ msgFormat) ""
  rustLibFp <-
    case decode jOut of
      Error msg -> fail ("cargoFinalizer: " ++ msg)
      Ok jObj -> case lookup "filenames" (fromJSObject jObj) of
                   Just (JSArray [ JSString jStr ]) -> pure (fromJSString jStr)
                   _ -> fail ("cargoFinalizer: did not find one static library")

  -- Move the library to a GHC temporary file
  let ext = takeExtension rustLibFp
  rustLibFp' <- addTempFile ext
  runIO $ copyFile rustLibFp rustLibFp'

  -- Link in the static library
  addForeignFilePath RawObject rustLibFp'

-- | Error message to display when @cargo@/@rustc@ fail to compile the module's
-- Rust file. Unfortunately, [errors reported by TH are always followed by the
-- piece of error code][0]. In this case, that ends up being the top of the file.
--
-- TODO: is there a way to avoid this?
--
-- [0]: https://stackoverflow.com/questions/47598270/whole-file-template-haskell-error
rustcErrMsg :: String
rustcErrMsg = "Rust source file associated with this module failed to compile"

-- | A finalizer to write out a Rust source file when we are done processing
-- this module.
fileFinalizer :: Q ()
fileFinalizer = do
  (pkg, mods) <- currentFile

  let dir = ".inline-rust" </> pkg
      thisFile = foldr1 (</>) mods <.> "rs"

  -- Figure out what we are putting into this file 
  Just codeBlocks <- fmap (reverse . reversedCodeBlocks) <$> getQ
  Just (Context (_,_,impls)) <- getQ
  let code = unlines (codeBlocks ++ 
                      [ "mod marshal {" ] ++
                      [ "use super::*;" ] ++
                      [ "pub trait MarshalInto<T> { fn marshal(self) -> T; }" ] ++
                      impls ++
                      [ "}" ] ++
                      [ "use marshal::*;" ])

  -- Write out the file
  runIO $ createDirectoryIfMissing True dir
  runIO $ writeFile (dir </> thisFile) code


-- * Module state

-- | Initialize the 'CodeBlocks' of the current module. Crash if it is already
-- intialized. This must be called exactly once.
initCodeBlocks :: Maybe [(String,String)]  -- ^ dependencies, if crate root
               -> Q () 
initCodeBlocks dependenciesOpt = do
  -- check if there is already something there
  cb <- getQ
  case cb of
    Nothing -> pure ()
    Just (CodeBlocks _) -> fail "initCodeBlocks: CodeBlocks already initialized"
  
  -- add hooks for writing out files (and possibly compiling the project)
  case dependenciesOpt of
    Nothing -> addModFinalizer fileFinalizer 
    Just deps -> addModFinalizer (fileFinalizer *> cargoFinalizer [] deps)

  -- add a module state
  putQ (CodeBlocks [])


-- | Emit a raw 'String' of Rust code into the current 'ModuleState'.
emitCodeBlock :: String -> Q [Dec]
emitCodeBlock code = do
  Just (CodeBlocks cbs) <- getQ 
  putQ (CodeBlocks (code : cbs))
  pure []

-- | Freeze the context and begin the part of the module which can contain Rust
-- quasiquotes. If this module is also the crate root, use 'setCrateRoot'
-- instead. 
--
-- This function must be called before any other Rust quasiquote in the file.
setCrateModule :: Q [Dec]
setCrateModule = do
  initCodeBlocks Nothing
  pure []

-- | Freeze the context and begin the part of the module which can contain Rust
-- quasiquotes. This function must be called in exactly one file in the
-- package; it is what will trigger compilation of all Rust quasiquotes in the
-- package and link the result into the final output.
--
-- This function must be called before any other Rust quasiquote in the file.
setCrateRoot :: [(String, String)] -> Q [Dec]
setCrateRoot dependencies = do
  initCodeBlocks (Just dependencies)
  pure []
  
-- | Get the existing context
getContext :: Q Context
getContext = fromMaybe mempty <$> getQ

-- | Append to the existing context 
extendContext :: Q Context -> Q [Dec]
extendContext qExtension = do
  extension <- qExtension
  ctx <- getContext
  putQ (ctx <> extension) 
  pure []

-- | Search in a 'Context' for the Haskell type corresponding to a Rust type.
getRType :: RType -> Q (HType, Maybe RType)
getRType rustType = do
  (qht, qrtOpt) <- getRTypeInContext rustType <$> getContext
  (,) <$> qht <*> sequence qrtOpt

-- | Search in a 'Context' for the Rust type corresponding to a Haskell type.
getHType :: HType -> Q RType
getHType haskType = getHTypeInContext haskType =<< getContext

