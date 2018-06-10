{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}

module Cenary.Codegen.CodegenM where

import           Cenary.Codegen.CodegenError
import           Cenary.Codegen.CodegenState
import           Cenary.Codegen.ContextM
import           Cenary.Codegen.Memory
import           Cenary.Codegen.TcM
import           Cenary.EvmAPI.OpcodeM
import           Control.Monad.Except
import           Control.Monad.State

-- | This type alias will be used for top-level codegen, since
-- at top level we use all contexts
type CodegenM m = (OpcodeM m, MonadState CodegenState m, MemoryM m, MonadError CodegenError m, ContextM m, TcM m, MonadFix m)
