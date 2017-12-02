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
  externCrate,
  setContext,
  getType,
  addForeignRustFile,
  addForeignRustFile',
) where

import Language.Rust.Inline.Context

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Data.Typeable                           ( Typeable )
import Control.Monad                           ( void )
import Data.Maybe                              ( fromMaybe )

import System.FilePath                         ( (</>), (<.>) )
import System.Directory                        ( renameFile,
                                                 createDirectoryIfMissing )
import System.Process                          ( spawnProcess, waitForProcess )
import System.Exit                             ( ExitCode(..) )

-- | We maintain this state while processing the module. The idea is that each
-- module will correspond to one Rust file.
data ModuleState = ModuleState
                     { getContext :: Context       -- ^ how to translate types
                     , codeBlocks :: [String]      -- ^ blocks of code emitted
                     , crates :: [(String,String)] -- ^ crate name, version
                     } deriving (Typeable)


-- | Get the 'ModuleState' of the current module, initializing it if it isn't
-- already initialized.
initModuleState :: Maybe Context -- ^ how to initialize the context (default is
                                 -- 'basic') if uninitialized.
                -> Q ModuleState
initModuleState contextMaybe = do
  moduleStateMaybe <- getQ
  case moduleStateMaybe of
    -- Module state is already initialized
    Just moduleState -> pure moduleState
    
    -- Module state needs to be initialized
    Nothing -> do

      -- add a hook to actually generate, compile, etc. the Rust file when we
      -- are done processing the module.
      addModFinalizer $ do
        Just (ModuleState { codeBlocks = code, crates = deps }) <- getQ
        let code' = unlines (reverse code)

        -- If there are no dependencies, run `rustc`. Else, go through `cargo`
        -- and store dependencies in `.inline-rust-quasi` folder.
        if null deps
          then addForeignRustFile [ "--crate-type=staticlib" ] code'
          else do Module _ name <- thisModule
                  let dir = ".inline-rust-quasi" </> modString name
                  runIO $ createDirectoryIfMissing True dir
                  addForeignRustFile' dir [] code' deps

      
      -- add a module state
      let m = ModuleState { getContext = fromMaybe basic contextMaybe
                          , codeBlocks = []
                          , crates = []
                          }
      putQ m
      pure m


-- | Emit a raw 'String' of Rust code into the current 'ModuleState'.
emitCodeBlock :: String -> Q [Dec]
emitCodeBlock code = do
  moduleState <- initModuleState Nothing
  putQ (moduleState { codeBlocks = code : codeBlocks moduleState })
  pure []


-- | Sets the 'Context' for the current module. This function, if called, must
-- be called before any of the other TH functions in this module.
--
-- >  setContext (basic <> libc)
setContext :: Context -> Q [Dec]
setContext context = do
  moduleState :: Maybe ModuleState <- getQ
  case moduleState of
    Nothing -> void (initModuleState (Just context))
    Just _ -> reportError "The module has already been initialised (setContext)"
  pure []


-- | Search in a 'Context' for the Haskell type corresponding to a Rust type.
getType :: RType -> Q HType
getType rustType = do
  context <- getContext <$> initModuleState Nothing
  getTypeInContext rustType context

-- | Error message to display when `cargo`/`rustc` fail to compile the module's
-- Rust file. Unfortunately, [errors reported by TH are always followed by the
-- piece of error code][0]. In this case, that ends up being the top of the file.
--
-- TODO: is there a way to avoid this?
--
-- [0]: https://stackoverflow.com/questions/47598270/whole-file-template-haskell-error
rustcErrMsg :: String
rustcErrMsg = "Rust source file associated with this module failed to compile"

-- | Add an extern crate dependency to this module. This is equivalent to
-- adding `crate_name = "version"` to a Rust project's `Cargo.toml` file.
--
-- >  externCrate "rayon" "0.9"
externCrate :: String  -- ^ crate name
            -> String  -- ^ crate version
            -> Q [Dec]
externCrate crateName crateVersion = do
  moduleState <- initModuleState Nothing
  putQ (moduleState { crates = (crateName, crateVersion) : crates moduleState })

  emitCodeBlock ("extern crate " ++ crateName ++ ";")


-- | Compile Rust source code and link the raw object into the current binary.
--
-- TODO: think about the cross-compilation aspect of this (where is `runIO`?)
addForeignRustFile :: [String] -- ^ options to pass to `rustc`
                   -> String   -- ^ contents of a complete Rust source file
                   -> Q ()
addForeignRustFile rustcArgs rustSrc = do

  -- Make input/output files
  fpIn <- addTempFile "rs"
  fpOut <- addTempFile "a"
  
  -- Write in the Rust source
  runIO $ writeFile fpIn rustSrc
  
  -- Call `rustc`
  let rustcAllArgs = rustcArgs ++ [ fpIn, "-o", fpOut ]
  ec <- runIO $ spawnProcess "rustc" rustcAllArgs >>= waitForProcess
  if ec /= ExitSuccess
    then reportError rustcErrMsg
    else -- Link in the object
         addForeignFilePath RawObject fpOut


-- | This is a more involved version of 'addForeignRustFile' which works for
-- drawing in dependencies. It calls out to `cargo` instead of `rustc`.
addForeignRustFile' :: FilePath           -- ^ temporary folder
                    -> [String]           -- ^ option to pass to `rustc`
                    -> String             -- ^ contents of complete Rust file
                    -> [(String, String)] -- ^ crate dependencies
                    -> Q ()
addForeignRustFile' dir rustcArgs rustSrc dependencies = do

  -- Find a place to put the Rust source and `Cargo.toml`
  let rustFile  = dir </> "quasiquote" <.> "rs"
  let cargoToml = dir </> "Cargo" <.> "toml"
  let rustLib   = dir </> "target" </> "release" </> "libquasiquote" <.> "a"

  -- Write in Rust source
  runIO $ writeFile rustFile rustSrc

  -- Make a `Cargo.toml` file
  let cargoSrc = unlines [ "[package]"
                         , "name = \"quasiquote\""
                         , "version = \"0.0.0\""

                         , "[dependencies]"
                         , unlines [ name ++ " = \"" ++ version ++ "\""
                                   | (name, version) <- dependencies
                                   ]

                         , "[lib]"
                         , "path = \"quasiquote.rs\""
                         , "crate-type = [\"staticlib\"]"
                         ]
  runIO $ writeFile cargoToml cargoSrc

  -- Call `cargo`
  let cargoArgs = [ "rustc"
                  , "--release"
                  , "--manifest-path=" ++ cargoToml
                  , "--"
                  ] ++ rustcArgs

  ec <- runIO $ spawnProcess "cargo" cargoArgs >>= waitForProcess
  if (ec /= ExitSuccess)
    then reportError rustcErrMsg
    else do -- Move the library to a GHC temporary file
            rustLib' <- addTempFile "a"
            runIO $ renameFile rustLib rustLib'

            -- Link in the object
            addForeignFilePath RawObject rustLib'

