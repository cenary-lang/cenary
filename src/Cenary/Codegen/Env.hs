{-# LANGUAGE TemplateHaskell #-}

module Cenary.Codegen.Env where

import           Cenary.Codegen.Context
import           Cenary.Syntax
import           Control.Lens hiding (Context)

data Sig = Sig String [(PrimType, String)] PrimType

data Env address = Env
  { _sig      :: Sig
  , _contexts :: [Context address] -- Essentially a stack
  }

makeLenses ''Env
