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
import Data.Monoid                 ( Endo(..) )
import Data.Maybe                  ( fromMaybe )
import Data.List                   ( unfoldr )
import Data.Char                   ( isAlpha, isAlphaNum )

import System.FilePath             ( (</>), (<.>), takeExtension )
import System.Directory            ( copyFile, createDirectoryIfMissing )
import System.Process              ( spawnProcess, readProcess, waitForProcess )
import System.Exit                 ( ExitCode(..) )
import System.Environment          ( setEnv )

import Text.JSON


-- * Module State

-- ** Code Blocks

-- | All of the code that needs to be emitted to the final Rust file.
-- @initCodeBlocks@ is responsible for initializing TH state (in 'Q').
newtype CodeBlocks = CodeBlocks { showsCodeBlocks :: ShowS }
  deriving ( Typeable )


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
  let finalizer = case dependenciesOpt of
                    Nothing -> fileFinalizer 
                    Just deps -> fileFinalizer *> cargoFinalizer [] deps
  addModFinalizer finalizer

  -- add a module state
  putQ (CodeBlocks id)

-- | Emit a raw 'String' of Rust code into the current 'ModuleState'.
emitCodeBlock :: String -> Q [Dec]
emitCodeBlock code = do
  Just (CodeBlocks cbs) <- getQ 
  putQ (CodeBlocks (cbs . showString code . showString "\n"))
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

-- ** Contexts

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


-- * Finalizers

-- | A finalizer to run Cargo and link in the static library. This function
-- should be the very last @inline-rust@ related TH to run.
-- 
-- After generating an appropriate @Cargo.toml@ file, it calls out to Cargo to
-- compile all the Rust files into a static library and which it then tells TH
-- to link in. 
cargoFinalizer :: [String]           -- ^ Extra @cargo@ arguments
               -> [(String, String)] -- ^ Dependencies
               -> Q ()
cargoFinalizer extraArgs dependencies = do
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
  --
  -- NOTE: We set `--print native-static-libs` to inform the user these are the
  --       libraries they should be specifying in `ghc-options`. It would be
  --       much better if:
  --
  --         * We could parse the `stdout` and print out a `ghc-options` related
  --           message. _However_ the message only gets printed if cargo ended
  --           up doing work, and I don't know how to detect that.
  --
  --         * We could automatically link in these libraries, if GHC supported
  --           specifying libraries to pass to the final linker call.
  --
  runIO $ setEnv "RUSTFLAGS" "--print native-static-libs"
  let cargoArgs = [ "build"
                  , "--release"
                  , "--manifest-path=" ++ cargoToml
                  ] ++ extraArgs 
      msgFormat = [ "--message-format=json" ]

  ec <- runIO $ spawnProcess "cargo" cargoArgs >>= waitForProcess
  when (ec /= ExitSuccess)
    (reportError rustcErrMsg)
 
  -- Run Cargo again to get the static library path
  jOuts <- runIO $ readProcess "cargo" (cargoArgs ++ msgFormat) ""
  let jOut = last (lines jOuts)
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
-- a module. This emits into a file in the @.inline-rust@ directory all of the
-- Rust code we have produced while processing the current files contexts and
-- quasiquotes.
fileFinalizer :: Q ()
fileFinalizer = do
  (pkg, mods) <- currentFile

  let dir = ".inline-rust" </> pkg
      thisFile = foldr1 (</>) mods <.> "rs"

  -- Figure out what we are putting into this file 
  Just cb <- getQ
  Just (Context (_,_,impls)) <- getQ
  let code = showsCodeBlocks cb 
           . showString "pub mod marshal {\n"
           . showString "#[allow(unused_imports)] use super::*;\n"
           . showString "pub trait MarshalInto<T> { fn marshal(self) -> T; }\n"
           . appEndo (foldMap (\s -> Endo (showString s . showString "\n")) impls)
           . showString "}\n"
           . showString "#[allow(unused_imports)]  use self::marshal::*;\n"
           $ ""

  -- Write out the file
  runIO $ createDirectoryIfMissing True dir
  runIO $ writeFile (dir </> thisFile) code

-- | Figure out what file we are currently in.
currentFile :: Q ( String    -- ^ package name, amended to be a valid crate name
                 , [String]  -- ^ dot-delimited segments of module name
                 )
currentFile = do
  Module (PkgName pkg) (ModName modName) <- thisModule
  let prefix | null pkg = "krate"
             | isAlpha (head pkg) = ""
             | otherwise = "krate_"
      pkg' = prefix ++ map fixChar pkg
  pure (pkg', splitDots modName)
  where
    fixChar c | isAlphaNum c = c
              | otherwise = '_'

    splitDots = unfoldr splitDot
    splitDot s | null s = Nothing
               | otherwise = let (x,r) = break (== '.') s in Just (x,drop 1 r)


