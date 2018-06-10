{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cenary.Codegen.Evm where

import           Cenary.Codegen.CodegenError
import           Cenary.Codegen.CodegenState
import           Control.Monad.Except
import           Control.Monad.State

newtype Evm a = Evm { runEvm :: StateT CodegenState (Either CodegenError) a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState, MonadError CodegenError, MonadFix)

