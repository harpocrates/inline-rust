{-# LANGUAGE ScopedTypeVariables #-}

module Language.Rust.Inline.Internal (
  emitCodeBlock,
  setContext,
  lookupType,
  addForeignRustFile,
) where

import Language.Rust.Inline.Context

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Data.Typeable                           ( Typeable )
import Control.Monad                           ( when, void )
import Data.Maybe                              ( fromMaybe )

import Data.Text.Prettyprint.Doc               ( layoutPretty, defaultLayoutOptions )
import Data.Text.Prettyprint.Doc.Render.String ( renderString )

import System.Process                          ( readProcessWithExitCode )
import System.Exit                             ( ExitCode(..) )

-- | We maintain this state while processing the module. The idea is that each
-- module will correspond to one Rust file.
data ModuleState = ModuleState
                     { getContext :: Context   -- ^ how to translate types
                     , codeBlocks :: [String]  -- ^ blocks of code emitted
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
        Just (ModuleState { codeBlocks = code }) <- getQ
        addForeignRustFile [ "--crate-type=staticlib" ] (unlines (reverse code))
      
      -- add a module state
      let m = ModuleState { getContext = fromMaybe basic contextMaybe
                          , codeBlocks = []
                          }
      putQ m
      pure m


-- | Emit code into the 'codeBlocks' field of the 'ModuleState'.
emitCodeBlock :: String -> Q ()
emitCodeBlock code = do
  moduleState <- initModuleState Nothing
  putQ (moduleState { codeBlocks = code : codeBlocks moduleState })


-- | Sets the 'Context' for the current module. This function, if called, must
-- be called before any of the other TH functions in this module.
--
-- >  setContext (basic <> libc)
setContext :: Context -> Q ()
setContext context = do
  moduleState :: Maybe ModuleState <- getQ
  case moduleState of
    Nothing -> void (initModuleState (Just context))
    Just _ -> fail "The module has already been initialised (setContext)"


-- | Search in a 'Context' for the Haskell type corresponding to a Rust type.
lookupType :: RType -> Q HType
lookupType rustType = do
  context <- getContext <$> initModuleState Nothing
  lookupTypeInContext rustType context


-- | Compile Rust source code and link the raw object into the current binary.
--
-- TODO: think about the cross-compilation aspect of this (where is `runIO`?)
addForeignRustFile :: [String] -- ^ options to pass to `rustc`
                   -> String   -- ^ Contents of a complete Rust source file
                   -> Q ()
addForeignRustFile rustcArgs rustSrc = do

  -- Make input/output files
  fpIn <- addTempFile "rs"
  fpOut <- addTempFile "a"
  
  -- Write in the Rust source
  runIO $ writeFile fpIn rustSrc
  
  -- Call `rustc`
  let rustcAllArgs = rustcArgs ++ [ fpIn, "-o", fpOut ]
  (ec, _, stderr) <- runIO $ readProcessWithExitCode "rustc" rustcAllArgs ""
  when (ec /= ExitSuccess) $ do
    fail ("Rust source in quasiquote failed to compile:\n" ++ stderr)
  
  -- Link in the object
  addForeignFilePath RawObject fpOut

