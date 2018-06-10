{-# LANGUAGE LambdaCase #-}

module Cenary.Codegen.Memory where

import           Cenary.Codegen.CodegenState
import           Cenary.Codegen.Evm
import           Cenary.Codegen.Scope
import           Cenary.EvmAPI.API
import           Control.Lens

class (Functor m, Applicative m, Monad m) => MemoryM m where
  load
    :: Integer
    -> Scope
    -> m ()
  load'
    :: Scope
    -> m ()
  alloc
    :: Scope
    -> m Integer
  store'
    :: Scope
    -> m ()
  push
    :: Integer
    -> m ()


instance MemoryM Evm where
  load addr scope = do
    push32 addr
    load' scope

  load' = \case
    Global -> sload
    Local -> mload

  store' = \case
    Global -> sstore
    Local -> mstore

  alloc = \case
    Global -> stackStorageEnd <<+= 0x20
    Local -> sp <<+= 0x20

  push val =
    push32 val -- OPTIMIZE: different PUSH variants can be used for this task
