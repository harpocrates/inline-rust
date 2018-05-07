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

module Language.Rust.Inline.Internal {- (
  emitCodeBlock,
  externCrate,
  setContext,
  getRType,
  getHType,
  addForeignRustFile,
  addForeignRustFile',
  getContext,
  peekContext,
  extendContext,
  initModuleState,
) -} where

import Language.Rust.Inline.Context

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Data.Typeable                           ( Typeable )
import Control.Monad                           ( void )
import Data.Maybe                              ( fromMaybe )
import Control.Arrow                           ( (&&&) )
import Data.List                               ( unfoldr )

import System.FilePath                         ( (</>), (<.>) )
import System.Directory                        ( renameFile, copyFile,
                                                 createDirectoryIfMissing )
import System.Process                          ( spawnProcess, waitForProcess )
import System.Exit                             ( ExitCode(..) )

-- | Represents blocks of code to be emitted
newtype CodeBlocks = CodeBlocks { reversedCodeBlocks :: [String] }
  deriving ( Typeable )


-- | Figure out what file we are currently in.
currentFile :: Q ( String    -- ^ package
                 , [String]  -- ^ dot-delimited segments of module name
                 )
currentFile = do
  Module (PkgName pkg) (ModName modName) <- thisModule
  pure (pkg, splitDots modName)
  where splitDots = unfoldr splitDot
        splitDot s | null s = Nothing
                   | otherwise = let (x,r) = break (== '.') s in Just (x,drop 1 r)


-- | Add a finalizer to run Cargo
cargoFinalizer :: [String]           -- ^ Extra @rustc@ arguments
                  -> [(String, String)] -- ^ Dependencies
                  -> Q ()
cargoFinalizer rustcArgs dependencies = do
  (pkg, mods) <- currentFile

  let dir = ".inline-rust" </> pkg
      thisFile = foldr1 (</>) mods <.> "rs"
      crate = "quasiquote_" ++ pkg

  -- Decide where to put the `Cargo.toml`
  let rustFile  = dir </> thisFile
      cargoToml = dir </> "Cargo" <.> "toml"
      rustLib   = dir </> "target" </> "release" </> "lib" ++ crate <.> "a"

  -- Make contents of a @Cargo.toml@ file
  let cargoSrc = unlines [ "[package]"
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

  -- Run Cargo
  let cargoArgs = [ "build"
                  , "--release"
                  , "--manifest-path=" ++ cargoToml
                  ] ++ rustcArgs

  ec <- runIO $ spawnProcess "cargo" cargoArgs >>= waitForProcess
  if (ec /= ExitSuccess)
    then reportError rustcErrMsg
    else do -- Move the library to a GHC temporary file
            rustLib' <- addTempFile "a"
            runIO $ copyFile rustLib rustLib'

            -- Link in the static library
            addForeignFilePath RawObject rustLib'


-- | Add a finalizer to write out the Rust source file when we are done
-- processing the module.
fileFinalizer :: Q ()
fileFinalizer = do
  (pkg, mods) <- currentFile

  let dir = ".inline-rust" </> pkg
      thisFile = foldr1 (</>) mods <.> "rs"

  -- Figure out what we are putting into this file 
  Just (CodeBlocks code) <- getQ
  Just (Context (_,_,impls)) <- getQ
  let code' = unlines (reverse code ++ 
                      [ "mod marshal {" ] ++
                      [ "use super::*;" ] ++
                      [ "pub trait MarshalInto<T> { fn marshal(self) -> T; }" ] ++
                      impls ++
                      [ "}" ] ++
                      [ "use marshal::*;" ])

  -- Write out the file
  runIO $ createDirectoryIfMissing True dir
  runIO $ writeFile (dir </> thisFile) code'

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


-- | Sets the 'Context' for the current module. This function, if called, must
-- be called before any of the other TH functions in this module.
--
-- >  setModuleContext (basic <> libc)
setModuleContext :: Q [Dec]
setModuleContext = do
  initCodeBlocks Nothing
  pure []

setCrateRootContext :: [(String, String)] -> Q [Dec]
setCrateRootContext dependencies = do
  initCodeBlocks (Just dependencies)
  pure []
  

setContext :: Q Context -> Q ()
setContext ctx = putQ =<< ctx

getContext :: Q Context
getContext = fromMaybe mempty <$> getQ

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

getHType :: HType -> Q RType
getHType haskType = getHTypeInContext haskType =<< getContext

-- | Error message to display when `cargo`/`rustc` fail to compile the module's
-- Rust file. Unfortunately, [errors reported by TH are always followed by the
-- piece of error code][0]. In this case, that ends up being the top of the file.
--
-- TODO: is there a way to avoid this?
--
-- [0]: https://stackoverflow.com/questions/47598270/whole-file-template-haskell-error
rustcErrMsg :: String
rustcErrMsg = "Rust source file associated with this module failed to compile"

{-
-- | Add an extern crate dependency to this module. This is equivalent to
-- adding `crate_name = "version"` to a Rust project's `Cargo.toml` file.
--
-- >  externCrate "rayon" "0.9"
externCrate :: String  -- ^ crate name
            -> String  -- ^ crate version
            -> Q [Dec]
externCrate crateName crateVersion = do
  moduleState <- initModuleState Nothing
--  putQ (moduleState { crates = (crateName, crateVersion) : crates moduleState })

  emitCodeBlock ("extern crate " ++ crateName ++ ";")
-}
{-
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
-}
{-
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
-}
