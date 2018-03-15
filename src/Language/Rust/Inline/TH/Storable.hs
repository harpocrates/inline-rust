{-|
Module      : Language.Rust.Inline.TH.Storable
Description : Generate Storable instances
Copyright   : (c) Alec Theriault, 2018
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : GHC
-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wwarn #-}         -- TODO: GHC bug around "unused pattern binds" in splices
                                   -- TODO: GHC feature around setting extensions from within TH
module Language.Rust.Inline.TH.Storable (
  mkStorable,
) where

import Language.Rust.Inline.TH.Utilities

import Language.Haskell.TH
import Language.Haskell.TH.Syntax hiding (lift)
import Control.Monad.Trans.State (StateT(..), get, put)
import Control.Monad.Trans.Class (lift)
import Data.Traversable          (for)
import Foreign.Ptr               (plusPtr, castPtr, Ptr)
import Data.Word                 (Word8)
import Foreign.Storable

-- | Generate 'Storable' instance for a non-recursive simple algebraic data
-- type. The instance follows the usual C layout for determining alignment and
-- size.
--
-- Sum types are implemented as tagged unions.
--
-- >>> mkStorable [t| forall a. Storable a => Storable (Maybe a) |]
--
-- Remember to have 'ScopedTypeVariables', 'ExplicitForall', and 'EmptyCase'
-- enabled when calling this!
mkStorable :: TypeQ    -- ^ a type representing the desired instance head
           -> Q [Dec]  -- ^ the instance declaration
mkStorable tyq = do
  pseudoInstHead <- tyq
  
  -- Extract the context
  storable <- [t| Storable |]
  (ctx, ty') <-
    case pseudoInstHead of
      ForallT _ ctx (AppT s ty) | s == storable -> pure (ctx, ty)
      AppT s ty                 | s == storable -> pure ([], ty)
      _ -> fail "mkStorable: malformed 'Storable' instance head"

  -- Get the type constructors name
  (_,cons') <- getConstructors ty'

  -- Produce the instance
  methods <- processADT cons' 
  dec <- instanceD (pure ctx) (pure (AppT storable ty')) (map pure methods)
  pure [dec]


-- * Alignment

-- | This is the information you need to carry along as you visit the fields of
-- a struct/union.
data Alignment = Alignment
  { decs        :: [Dec]
  -- ^ declarations for variables relied on by offset and align
  
  , offsetSoFar :: TExp Int
  -- ^ total bytes occupied so far by fields
  
  , alignSoFar  :: TExp Int
  -- ^ size (in bytes) of the largest member in the struct
  }

-- | Combining alignment means concatenating the dependent declarations, and
-- take the maximum for offset and alignment.
instance Semigroup (Q Alignment) where
  a1 <> a2 = do
    newDecs <- (++) <$> fmap decs a1 <*> fmap decs a2
    newOff  <- [|| $$(offsetSoFar <$> a1) `max` $$(offsetSoFar <$> a2) ||]
    newAln  <- [|| $$(alignSoFar  <$> a1) `max` $$(alignSoFar  <$> a2) ||]
    pure $ Alignment newDecs newOff newAln 

-- | The 'mconcat' method calls 'maximum'
instance Monoid (Q Alignment) where
  mempty = Alignment [] <$> [|| 0 ||] <*> [|| 1 ||]
  mappend = (<>)
  mconcat as = do
    as' <- sequence as
    newOff <- [|| maximum $$(pure . listTE . map offsetSoFar $ as') ||]
    newAln <- [|| maximum $$(pure . listTE . map alignSoFar  $ as') ||]
    pure $ Alignment (concatMap decs as') newOff newAln

-- | This is the state we will bundle along while visiting fields.
type StructState = StateT Alignment Q

-- | Make a typed list. This function is like 'listE', but for 'TExp'.
listTE :: [TExp a] -> TExp [a]
listTE = TExp . ListE . map unType


-- * Peek and poke helper functions 

-- | Produces a 'do' block for peeking a constructor. The generated code has the
-- following shape:
--
-- @
--     do f1 <- ... ptr
--        f2 <- ... ptr
--        ...
--        fn <- ... ptr
--        return (Con f1 f2 ... fn)
-- @
--
peekCon :: Name              -- ^ name of the constructor
        -> [Exp -> Q Exp]    -- ^ how to peek every field
        -> Name              -- ^ the base pointer
        -> Q Exp             -- ^ a 'do' expression for peeking the constructor
peekCon con peekFields ptr = do
  (ns, binds) <- unzip <$> do
    for peekFields $ \fldCont -> do
       n <- newName "n"
       pure (varE n, bindS (varP n) (fldCont (VarE ptr)))
  let ret = [e| return $(appsE (conE con : ns)) |]
  doE (binds ++ [noBindS ret])

-- | Produces a 'do' block for poking a constructor, along with a pattern for
-- extracting out the right fields. Given a pattern like @Con f1 f2 ... fn@, the
-- generated block has the following shape:
--
-- @
--     do ... ptr f1
--        ... ptr f2
--        ...
--        ... ptr fn
-- @
pokeCon :: Name              -- ^ name of the constructor
        -> [Exp -> Q Exp]    -- ^ how to poke every field
        -> Name              -- ^ the base poniter
        -> Q (Pat, Exp)      -- ^ a pattern to match, an expression for poking
pokeCon con pokeFields ptr = do
  (ns, stmts) <- unzip <$> do
    for pokeFields $ \fldCont -> do
        n <- newName "n"
        pure (varP n, noBindS [e| $(fldCont (VarE ptr)) $(varE n) |])
  pat <- conP con ns
  expr <- if null stmts then [e| pure () |] else doE stmts
  return (pat, expr)


-- * Traversing fields (putting everything together)

-- | Process a field of a given type.
processField :: Type -> StructState (Exp -> Q Exp, Exp -> Q Exp)
processField ty = do
  let alignTy, sizeTy :: Q (TExp Int)
      alignTy  = TExp <$> [e| alignment (undefined :: $(pure ty)) |]
      sizeTy   = TExp <$> [e| sizeOf    (undefined :: $(pure ty)) |]

  -- get state at the end of the last field
  Alignment prevDecs prevOff prevAlign <- get

  -- beginning offset
  beginOffV <- lift $ newName "beginOff"
  let beginOffE, beginOff :: Q (TExp Int)
      beginOffE = [|| $$(pure prevOff) + mod (negate $$(pure prevOff)) $$alignTy ||]
      beginOff = TExp <$> varE beginOffV
  assignBeginOff <- lift [d| $(varP beginOffV) = $(unType <$> beginOffE) |]

  -- offset after this field
  newOffV <- lift $ newName "afterOff"
  let newOffE :: Q (TExp Int)
      newOffE = [|| $$beginOff + $$sizeTy ||]
  newOff <- lift (TExp <$> varE newOffV)
  assignNewOff <- lift [d| $(varP newOffV) = $(unType <$> newOffE) |] 

  -- alignment after this field
  newAlignV <- lift $ newName "algn"
  let newAlignE :: Q (TExp Int)
      newAlignE = [|| $$alignTy `max` $$(pure prevAlign) ||]
  newAlign <- lift (TExp <$> varE newAlignV)
  assignNewAlign <- lift [d| $(varP newAlignV) = $(unType <$> newAlignE) |]
  
  -- update state
  put (Alignment { decs = concat [ assignBeginOff
                                 , assignNewOff
                                 , assignNewAlign
                                 , prevDecs
                                 ]
                 , offsetSoFar = newOff
                 , alignSoFar = newAlign
                 })

  -- TODO: consider degenerate sizeof(..) = 0 cases
  let offset = unType <$> [|| $$beginOff `div` $$sizeTy ||]
  pure ( \addrE -> [e| peek (castPtr $(pure addrE) `plusPtr` $offset) |]
       , \addrE -> [e| poke (castPtr $(pure addrE) `plusPtr` $offset) |]
       )

-- | Process an algebraic data type.
--
-- TODO: think about the zero constructor case...
processADT :: [(Name, [Type])]  -- ^ constructors and the types of their fields
           -> Q [Dec]           -- ^ methods of the 'Storable' class

-- The one constructor case is special - we don't need to specify a tag
processADT [(con, fields)] = do
  
  initAlign <- mempty
  (peekPokes, Alignment ds off algn)
    <- runStateT (traverse processField fields) initAlign
  let ds' = map pure ds

  -- sizeOf
  sizeOf_    <- do
    Just sizeOfN <- lookupValueName "sizeOf"
    funD sizeOfN [clause [wildP]
                         (normalB [e| let c = $(pure . unType $ off)
                                      in c + mod (negate c) $(pure . unType $ algn) |])
                         ds']

  -- alignment
  alignment_ <- do
    Just alignmentN <- lookupValueName "alignment"
    funD alignmentN [clause [wildP] (normalB (pure . unType $ algn)) ds']

  let (peekFields, pokeFields) = unzip peekPokes
  
  -- peek
  peek_ <- do
    ptr <- newName "ptr"
    Just peekN <- lookupValueName "peek"
    funD peekN [clause [varP ptr] (normalB (peekCon con peekFields ptr)) ds']

  -- poke
  poke_ <- do
    ptr <- newName "ptr"
    (conPat,body) <- pokeCon con pokeFields ptr
    Just pokeN <- lookupValueName "poke"
    funD pokeN [clause [varP ptr, pure conPat] (normalB (pure body)) ds']

  pure [sizeOf_, alignment_, peek_, poke_]

processADT cons = do

  -- TODO put more than a word if there are more than 8 constructors
  i <- Alignment [] <$> [|| 1 ||] <*> [|| 1 ||]
  (conPeekPokess, algns) <- unzip <$> do
    for cons $ \(con, fields) -> do
      (peekPokes, algn) <- runStateT (traverse processField fields) i
      let (peekFields, pokeFields) = unzip peekPokes
      pure ((con, peekFields, pokeFields), algn)
  Alignment ds off algn <- mconcat (map pure algns)
  let ds' = map pure $ ds

  -- sizeOf
  sizeOf_ <- do
    Just sizeOfN <- lookupValueName "sizeOf"
    funD sizeOfN [clause [wildP]
                         (normalB [e| let c = $(pure . unType $ off)
                                      in c + mod (negate c) $(pure . unType $ algn) |])
                         ds']

  -- alignment
  alignment_ <- do
    Just alignmentN <- lookupValueName "alignment"
    funD alignmentN [clause [wildP] (normalB (pure . unType $ algn)) ds']

  -- peek
  peek_ <- do
    ptr <- newName "ptr"
    disc <- newName "disc"
    let mtchs = [ match (litP n') (normalB (peekCon con peekFields ptr)) []
                | (n, (con, peekFields, _)) <- zip [0..] conPeekPokess
                , let n' = IntegerL n
                ]
    Just peekN <- lookupValueName "peek"
    funD peekN
         [clause [varP ptr]
                 (normalB (doE [ bindS (varP disc) [e| peek (castPtr $(varE ptr) :: Ptr Word8) |]
                               , noBindS (caseE (varE disc) mtchs)
                               ]))
                 ds']

  -- poke
  poke_ <- do
    ptr <- newName "ptr"
    disc <- newName "disc"
    let mtchs = [ do { (pat,body) <- patBody
                     ; match (pure pat)
                             (normalB (doE (map noBindS [ [e| poke (castPtr $(varE ptr) :: Ptr Word8) $(litE n') |]
                                                        , pure body
                                                        ])))
                             []
                     }
                | (n, (con, _, pokeFields)) <- zip [0..] conPeekPokess
                , let patBody = pokeCon con pokeFields ptr
                , let n' = IntegerL n
                ]
    Just pokeN <- lookupValueName "poke"
    funD pokeN
         [clause [varP ptr, varP disc] (normalB (caseE (varE disc) mtchs)) ds']

  pure [sizeOf_, alignment_, peek_, poke_]

